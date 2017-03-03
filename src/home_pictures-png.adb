with Home_Pictures.Swaps;
with GNAT.CRC32;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

-- http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html

package body Home_Pictures.PNG is

   procedure Put (Item : Stream_Element_Array) is
      use Ada.Text_IO;
   begin
      for E of Item loop
         Put (Character'Val (E));
      end loop;
   end Put;

   function Create_Chunk_Kind (Item : PNG_Chunk_Kind_Name) return PNG_Chunk_Kind is
      R : PNG_Chunk_Kind;
   begin
      Assert (Item'Length = 4);
      for I in 0 .. 3 loop
         R (R'First + Stream_Element_Offset (I)) := Character'Pos (Item (Item'First + I));
      end loop;
      return R;
   end;

   procedure Update (Item : in out GNAT.CRC32.CRC32; Value : PNG_Chunk_IHDR) is
      B : Stream_Element_Array (0 .. 12) with Address => Value'Address;
      use GNAT.CRC32;
   begin
      Update (Item, B);
   end Update;


   function Calc_Checksum (Item : PNG_Chunk) return Unsigned_32 is
      use GNAT.CRC32;
      use Ada.Streams;
      use type Ada.Streams.Stream_Element_Array;
      --subtype R is Stream_Element_Offset range 0 .. Stream_Element_Offset (Item.Length - 1);
      --B : Stream_Element_Array (R) with Address => Item.Kind'Address;
      C : CRC32;
   begin
      Initialize (C);
      Update (C, Item.Kind);
      if Item.Data /= null then
         Update (C, Item.Data.all);
      end if;
      return Get_Value (C);
   end;

   -- http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html#PNG-file-signature
   procedure Read_Signature (Streamer : Stream_Access) is
      type PNG_Signature is array (0 .. 7) of Unsigned_8 with Pack;
      --This signature indicates that the remainder of the file contains a single PNG image.
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




   procedure Read_First_Chunk (Streamer : Stream_Access; Item : in out PNG_Chunk_IHDR) is
      use Home_Pictures.Swaps;
      use GNAT.CRC32;
      Kind : PNG_Chunk_Kind;
      Kind_IHDR : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IHDR");
      Length : Unsigned_32;
      Checksum : Unsigned_32;
      Calculated_Checksum : CRC32;
   begin
      Unsigned_32'Read (Streamer, Length);
      Length := Bswap_32 (Length);
      -- All integers that require more than one byte must be in network byte order

      Assert (Length = 13, "First chunk is not 13 bytes.");

      PNG_Chunk_Kind'Read (Streamer, Kind);
      Assert (Kind = Kind_IHDR, "IHDR is not first.");
      -- IHDR must appear first.

      PNG_Chunk_IHDR'Read (Streamer, Item);

      Unsigned_32'Read (Streamer, Checksum);
      Checksum := Bswap_32 (Checksum);
      -- All integers that require more than one byte must be in network byte order

      Initialize (Calculated_Checksum);
      Update (Calculated_Checksum, Kind);
      Update (Calculated_Checksum, Item);
      -- A 4-byte CRC (Cyclic Redundancy Check) calculated on the preceding bytes in the chunk,
      -- including the chunk type code and chunk data fields, but not including the length field

      Assert (Get_Value (Calculated_Checksum) = Checksum, "Checksum does not match.");


      Item.Width := Bswap_32 (Item.Width);
      Item.Height := Bswap_32 (Item.Height);
      -- All integers that require more than one byte must be in network byte order

   end;


   procedure Read_Chunk (Streamer : Stream_Access; Chunk : in out PNG_Chunk) is
      use Home_Pictures.Swaps;
      use Ada.Streams;
   begin
      Unsigned_32'Read (Streamer, Chunk.Length);
      Chunk.Length := Bswap_32 (Chunk.Length);
      -- All integers that require more than one byte must be in network byte order

      PNG_Chunk_Kind'Read (Streamer, Chunk.Kind);

      if Chunk.Length > 0 then
         declare
            subtype R is Stream_Element_Offset range 0 .. Stream_Element_Offset (Chunk.Length - 1);
            subtype S is Stream_Element_Array (R);
         begin
            Chunk.Data := new S;
            S'Read (Streamer, Chunk.Data.all);
         end;
      else
         Chunk.Data := null;
      end if;
      -- Allocate new space for the chunk given chunk length.

      Unsigned_32'Read (Streamer, Chunk.Checksum);
      Chunk.Checksum := Bswap_32 (Chunk.Checksum);
      -- All integers that require more than one byte must be in network byte order

      Assert (Calc_Checksum (Chunk) = Chunk.Checksum, "Checksum does not match");
   end;



   procedure Read (Streamer : Stream_Access; Item : in out PNG_Information) is
      use type Ada.Containers.Count_Type;
      Chunk_Kind_IEND : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IEND");
      Chunk_Kind_IDAT : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IDAT");
   begin
      Read_Signature (Streamer);
      --The first eight bytes of a PNG file always contain the PNG signature.

      Read_First_Chunk (Streamer, Item.Chunk_IHDR);
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
