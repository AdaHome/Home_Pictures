with Home_Pictures.Swaps;
with GNAT.CRC32;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

-- http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html

package body Home_Pictures.PNG is

   function Create_Chunk_Kind_32 (Item : PNG_Chunk_Kind_String) return Unsigned_32 is
      R : Unsigned_32 with Address => Item'Address;
      --N : Unsigned_32 := Home_Pictures.Swaps.Bswap_32 (R);
   begin
      return R;
   end;

   procedure Update (Item : in out GNAT.CRC32.CRC32; Value : PNG_Data_IHDR) is
      B : Stream_Element_Array (0 .. 12) with Address => Value'Address;
   begin
      GNAT.CRC32.Update (Item, B);
   end Update;

   procedure Update (Item : in out GNAT.CRC32.CRC32; Value : Unsigned_32) is
      B : Stream_Element_Array (0 .. 3) with Address => Value'Address;
   begin
      GNAT.CRC32.Update (Item, B);
   end Update;

   procedure Update (Item : in out GNAT.CRC32.CRC32; Value : PNG_Chunk_Kind) is
      B : Stream_Element_Array (0 .. 3) with Address => Value'Address;
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
   end;

   procedure Read_Chunk_Kind (Streamer : Stream_Access; Kind : out PNG_Chunk_Kind; Calculated_Checksum : in out GNAT.CRC32.CRC32) is
   begin
      PNG_Chunk_Kind'Read (Streamer, Kind);
      Update (Calculated_Checksum, Kind);
      -- TODO: Swap kind here?
      -- libpng is swapping the kind.
      -- It might not be useful to swap the kind.
   end;


   procedure Read_Chunk_Begin (Streamer : Stream_Access; Length : out Unsigned_32; Kind : out PNG_Chunk_Kind; Calculated_Checksum : out GNAT.CRC32.CRC32) is
   begin
      GNAT.CRC32.Initialize (Calculated_Checksum);
      Read (Streamer, Length);
      Read_Chunk_Kind (Streamer, Kind, Calculated_Checksum);
      -- A 4-byte CRC (Cyclic Redundancy Check) calculated on the preceding bytes in the chunk,
      -- including the chunk type code and chunk data fields, but not including the length field.
   end;


   procedure Read_Chunk_End (Streamer : Stream_Access; Calculated_Checksum : GNAT.CRC32.CRC32) is
      Checksum : PNG_Checksum;
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

   procedure Swap_Byte_Order (Item : in out PNG_Data_IHDR) is
      use Home_Pictures.Swaps;
   begin
      Item.Width := PNG_Width (Bswap_32 (Unsigned_32 (Item.Width)));
      Item.Height := PNG_Height (Bswap_32 (Unsigned_32 (Item.Height)));
      -- All integers that require more than one byte must be in network byte order
      -- TODO: Do not swap byte order on a network byte order machine.
   end;

   function Find_Channel_Count (Item : PNG_Color_Kind) return Unsigned_8 is
   begin
      case Item is
         when PNG_Color_Kind_Greyscale | PNG_Color_Kind_Indexed_Colour =>
            return 1;
         when PNG_Color_Kind_Truecolour =>
            return 3;
         when PNG_Color_Kind_Greyscale_With_Alpha =>
            return 2;
         when PNG_Color_Kind_Truecolour_With_Alpha =>
            return 4;
      end case;
   end;

   procedure Read_First_Chunk (Streamer : Stream_Access; Item : in out PNG_Data_IHDR) is
      use Home_Pictures.Swaps;
      use GNAT.CRC32;
      Kind : PNG_Chunk_Kind;
      --Kind_IHDR : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IHDR");
      Length : Unsigned_32;
      Calculated_Checksum : CRC32;
   begin
      Read_Chunk_Begin (Streamer, Length, Kind, Calculated_Checksum);
      Assert (Length = 13, "The first chunk length is invalid. First chunk must be 13 bytes long. This chunk length is" & Length'Img & "bytes long.");
      Assert (Kind = PNG_Chunk_Kind_IHDR, "The first chunk kind is invalid. The chunk kind must be IHDR. This chunk kind is" & Kind'Img & ".");
      -- The PNG_Chunk_Data_IHDR must appear first and be 13 bytes.
      -- These assertion fails if the PNG stream is corrupted.

      PNG_Data_IHDR'Read (Streamer, Item);
      Update (Calculated_Checksum, Item);
      Read_Chunk_End (Streamer, Calculated_Checksum);
      Swap_Byte_Order (Item);
   end;


   procedure Read_Chunk (Streamer : Stream_Access; Chunk : in out PNG_Chunk) is
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
   begin
      Read_Signature (Streamer);

      Read_First_Chunk (Streamer, Item.Data_IHDR);
      Item.Chunk_Count := Item.Chunk_Count + 1;
      -- The IHDR chunk shall be the first chunk in the PNG datastream.

      Item.Channel_Count := Find_Channel_Count (Item.Data_IHDR.Color_Kind);
      Item.Pixel_Depth_Bit := PNG_Bit_Depth'Enum_Rep (Item.Data_IHDR.Bit_Depth) * Item.Channel_Count;
      Item.Pixel_Depth_Byte := Shift_Right (Item.Pixel_Depth_Bit + 7, 3);
      Item.Row_Size_Byte := Unsigned_32 (Item.Pixel_Depth_Byte) * Item.Data_IHDR.Width;

      declare
         C : PNG_Chunk;
      begin
         loop
            Read_Chunk (Streamer, C);
            Item.Chunk_Count := Item.Chunk_Count + 1;
            if C.Kind = PNG_Chunk_Kind_IDAT then
               Item.Data_IDAT_List.Append (C);
            else
               Item.Data_Unkown_List.Append (C);
            end if;
            exit when C.Kind = PNG_Chunk_Kind_IEND;
            Assert (Item.Chunk_Count < 10, "Max chunk count reached, no more chunk can be read. This is just a protection against infinite loop.");
         end loop;
      end;
   end Read;

   procedure Read_Chunk (Streamer : Stream_Access; Item : out PNG_Small_Chunk) is
      Calculated_Checksum : GNAT.CRC32.CRC32;
      type Stream_Element_Array_Access is access all Stream_Element_Array;
      Data : Stream_Element_Array_Access;
      procedure Free is new Ada.Unchecked_Deallocation (Stream_Element_Array, Stream_Element_Array_Access);
   begin
      Read_Chunk_Begin (Streamer, Item.Length, Item.Kind, Calculated_Checksum);
      declare
         subtype R is Stream_Element_Offset range 1 .. Stream_Element_Offset (Item.Length);
         subtype S is Stream_Element_Array (R);
      begin
         Data := new S;
         S'Read (Streamer, Data.all);
      end;
      GNAT.CRC32.Update (Calculated_Checksum, Data.all);
      Free (Data);
      Read_Chunk_End (Streamer, Calculated_Checksum);
   end;

   procedure Read_Complete (Streamer : Stream_Access; Item : out PNG_Small_Chunk_Vector) is
      N : Natural := 0;
      E : PNG_Small_Chunk;
   begin
      Read_Signature (Streamer);
      loop
         Read_Chunk (Streamer, E);
         Item.Append (E);
         exit when E.Kind = PNG_Chunk_Kind_IEND;
         N := N + 1;
         exit when N > 40; -- Guard.
      end loop;
   end;

   procedure Read_Complete (File_Name : String; Item : out PNG_Small_Chunk_Vector) is
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      Open (F, In_File, File_Name);
      Read_Complete (Stream (F), Item);
      Close (F);
   end;

   function Reconstruction_Function (Filter_Type : PNG_Filter_Type; X, A, B, C : Unsigned_8) return Unsigned_8 is
   begin
      case Filter_Type is
      when PNG_Filter_Type_None =>
         return X;
      when PNG_Filter_Type_Sub =>
         return X + B;
      when PNG_Filter_Type_Up =>
         return X + A;
      when PNG_Filter_Type_Average =>
         return X + ((A + B) / 2);
      when PNG_Filter_Type_Paeth =>
         declare
            P, PA, PB, PC, PR : Unsigned_8;
         begin
            P := A + B - C;
            PA := abs (P - A);
            PB := abs (P - B);
            PC := abs (P - C);
            if PA <= PB and PA <= PC then
               PR := A;
            elsif PB <= PC then
               PR := B;
            else
               PR := C;
            end if;
            return PR;
         end;
      end case;
   end;

   procedure Reconstruction_Procedure (Filter_Type : PNG_Filter_Type; Pixel_Depth_Byte : Stream_Element_Offset; Previous : in Stream_Element_Array; Current : in out Stream_Element_Array) is
   begin
      for I in Current'First + 1 .. Current'Last loop
         declare
            X : constant Unsigned_8 := Unsigned_8 (Current (I));
            A : constant Unsigned_8 := Unsigned_8 (Current (I - Pixel_Depth_Byte));
            B : constant Unsigned_8 := Unsigned_8 (Previous (I));
            C : constant Unsigned_8 := Unsigned_8 (Previous (I - Pixel_Depth_Byte));
         begin
            Current (I) := Stream_Element (Reconstruction_Function (Filter_Type, X, A, B, C));
         end;
      end loop;
   end;

end;
