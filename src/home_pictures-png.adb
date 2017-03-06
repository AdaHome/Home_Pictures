with Home_Pictures.Swaps;
with GNAT.CRC32;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

-- http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html

package body Home_Pictures.PNG is

   function Create_Chunk_Kind (Item : PNG_Chunk_Kind_String) return PNG_Chunk_Kind is
      R : PNG_Chunk_Kind;
   begin
      for I in 0 .. 3 loop
         R (R'First + Stream_Element_Offset (I)) := Character'Pos (Item (Item'First + I));
      end loop;
      return R;
   end;

   procedure Update (Item : in out GNAT.CRC32.CRC32; Value : PNG_Chunk_Data_IHDR) is
      B : Stream_Element_Array (0 .. 12) with Address => Value'Address;
   begin
      GNAT.CRC32.Update (Item, B);
   end Update;


   procedure Read (Streamer : Stream_Access; Item : out Unsigned_32) is
   begin
      Unsigned_32'Read (Streamer, Item);
      Item := Home_Pictures.Swaps.Bswap_32 (Item);
      -- All integers that require more than one byte must be in network byte order:
      -- the most significant byte comes first, then the less significant bytes in
      -- descending order of significance (MSB LSB for two-byte integers, B3 B2 B1 B0 for four-byte integers).
      -- The highest bit (value 128) of a byte is numbered bit 7; the lowest bit (value 1) is numbered bit 0.
      -- Values are unsigned unless otherwise noted. Values explicitly noted as signed are represented in two's complement notation.
   end Read;


   procedure Read_Chunk_Begin (Streamer : Stream_Access; Length : out Unsigned_32; Kind : out PNG_Chunk_Kind; Calculated_Checksum : out GNAT.CRC32.CRC32) is
   begin
      Read (Streamer, Length);
      PNG_Chunk_Kind'Read (Streamer, Kind);
      GNAT.CRC32.Initialize (Calculated_Checksum);
      GNAT.CRC32.Update (Calculated_Checksum, Kind);
      -- A 4-byte CRC (Cyclic Redundancy Check) calculated on the preceding bytes in the chunk,
      -- including the chunk type code and chunk data fields, but not including the length field.
   end;


   procedure Read_Chunk_End (Streamer : Stream_Access; Calculated_Checksum : GNAT.CRC32.CRC32) is
      Checksum : Unsigned_32;
   begin
      Read (Streamer, Checksum);
      Assert (GNAT.CRC32.Get_Value (Calculated_Checksum) = Checksum, "Checksum does not match.");
   end;



   procedure Read_Signature (Streamer : Stream_Access) is
      type PNG_Signature is array (0 .. 7) of Unsigned_8 with Pack;
      Signature : PNG_Signature;
      -- The first eight bytes of a PNG file always contain the following (decimal) values:
      --     (decimal)              137  80  78  71  13  10  26  10
      --     (hexadecimal)           89  50  4e  47  0d  0a  1a  0a
      --     (ASCII C notation)    \211   P   N   G  \r  \n \032 \n
      Signature_Constant : constant PNG_Signature := (137, 80, 78, 71, 13, 10, 26, 10);
   begin
      -- The first 4 bytes must be a PNG signature.
      PNG_Signature'Read (Streamer, Signature);
      Assert (Signature = Signature_Constant, "The signature does not match a PNG signature");
   end;




   procedure Read_First_Chunk (Streamer : Stream_Access; Item : in out PNG_Chunk_Data_IHDR) is
      use Home_Pictures.Swaps;
      use GNAT.CRC32;
      Kind : PNG_Chunk_Kind;
      Kind_IHDR : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IHDR");
      Length : Unsigned_32;
      Calculated_Checksum : CRC32;
   begin
      Read_Chunk_Begin (Streamer, Length, Kind, Calculated_Checksum);
      Assert (Length = 13, "The first chunk length is invalid. First chunk must be 13 bytes long. This chunk length is" & Length'Img & "bytes long.");
      Assert (Kind = Kind_IHDR, "The first chunk kind is invalid. The chunk kind must be IHDR. This chunk kind is" & Kind (Kind'First)'Img & ".");
      -- The PNG_Chunk_Data_IHDR must appear first and be 13 bytes.
      -- These assertion fails if the PNG stream is corrupted.

      PNG_Chunk_Data_IHDR'Read (Streamer, Item);
      Update (Calculated_Checksum, Item);

      Read_Chunk_End (Streamer, Calculated_Checksum);

      Item.Width := PNG_Width (Bswap_32 (Unsigned_32 (Item.Width)));
      Item.Height := PNG_Height (Bswap_32 (Unsigned_32 (Item.Height)));
      -- All integers that require more than one byte must be in network byte order

   end;


   procedure Read_Chunk (Streamer : Stream_Access; Chunk : in out PNG_Chunk) is
      use Home_Pictures.Swaps;
      use Ada.Streams;
      Calculated_Checksum : GNAT.CRC32.CRC32;
   begin
      Read_Chunk_Begin (Streamer, Chunk.Length, Chunk.Kind, Calculated_Checksum);
      if Chunk.Length > 0 then
         declare
            subtype R is Stream_Element_Offset range 0 .. Stream_Element_Offset (Chunk.Length - 1);
            subtype S is Stream_Element_Array (R);
         begin
            Chunk.Data := new S;
            S'Read (Streamer, Chunk.Data.all);
            GNAT.CRC32.Update (Calculated_Checksum, Chunk.Data.all);
         end;
      else
         Chunk.Data := null;
      end if;
      -- Allocate new space for the chunk given chunk length.

      Read_Chunk_End (Streamer, Calculated_Checksum);
   end;



   procedure Read (Streamer : Stream_Access; Item : in out PNG_Information) is
      use type Ada.Containers.Count_Type;
      Chunk_Kind_IEND : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IEND");
      Chunk_Kind_IDAT : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IDAT");
   begin
      Read_Signature (Streamer);

      Read_First_Chunk (Streamer, Item.Chunk_Data_IHDR);
      -- IHDR must appear first.

      Item.Chunk_Count := Item.Chunk_Count + 1;

      declare
         C : PNG_Chunk;
      begin
         loop
            Read_Chunk (Streamer, C);
            Item.Chunk_Count := Item.Chunk_Count + 1;
            if C.Kind = Chunk_Kind_IDAT then
               Item.Chunk_IDAT_List.Append (C);
            else
               Item.Chunk_Unkown_List.Append (C);
            end if;
            exit when C.Kind = Chunk_Kind_IEND;
            Assert (Item.Chunk_Count < 10, "Max chunk count reached, no more chunk can be read. This is just a protection against infinite loop.");
         end loop;
      end;


   end Read;




end;
