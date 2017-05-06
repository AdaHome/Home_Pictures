with Home_Pictures.Swaps;
with GNAT.CRC32;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
-- http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html

package body Home_Pictures.PNG is

   function Create_Chunk_Kind_32 (Item : PNG_Chunk_Kind_String) return Unsigned_32 is
      R : Unsigned_32 with Address => Item'Address;
      --N : Unsigned_32 := Home_Pictures.Swaps.Bswap_32 (R);
   begin
      return R;
   end;

   procedure Update1 (Item : in out GNAT.CRC32.CRC32; Value : System.Address; Size : Natural) is
      Count : Stream_Element_Count := Stream_Element_Count (Size / System.Storage_Unit);
      B : Stream_Element_Array (1 .. Count) with Address => Value;
   begin
      GNAT.CRC32.Update (Item, B);
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

   procedure Read (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_32) is
   begin
      Unsigned_32'Read (Streamer, Item);
      Item := Home_Pictures.Swaps.Bswap_32 (Item);
      -- All integers that require more than one byte must be in network byte order:
      -- the most significant byte comes first, then the less significant bytes in
      -- descending order of significance (MSB LSB for two-byte integers, B3 B2 B1 B0 for four-byte integers).
      -- The highest bit (value 128) of a byte is numbered bit 7; the lowest bit (value 1) is numbered bit 0.
      -- Values are unsigned unless otherwise noted. Values explicitly noted as signed are represented in two's complement notation.
   end;

   procedure Read_Chunk_Kind (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Kind : out PNG_Chunk_Kind; Calculated_Checksum : in out GNAT.CRC32.CRC32) is
   begin
      PNG_Chunk_Kind'Read (Streamer, Kind);
      Update (Calculated_Checksum, Kind);
      -- TODO: Swap kind here?
      -- libpng is swapping the kind.
      -- It might not be useful to swap the kind.
   end;


   procedure Read_Chunk_Begin (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Length : out Unsigned_32; Kind : out PNG_Chunk_Kind; Calculated_Checksum : out GNAT.CRC32.CRC32) is
   begin
      GNAT.CRC32.Initialize (Calculated_Checksum);
      Read (Streamer, Length);
      Read_Chunk_Kind (Streamer, Kind, Calculated_Checksum);
      -- A 4-byte CRC (Cyclic Redundancy Check) calculated on the preceding bytes in the chunk,
      -- including the chunk type code and chunk data fields, but not including the length field.
   end;


   procedure Read_Chunk_End (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Calculated_Checksum : GNAT.CRC32.CRC32) is
      Checksum : PNG_Checksum;
   begin
      Read (Streamer, Checksum);
      Assert (GNAT.CRC32.Get_Value (Calculated_Checksum) = Checksum, "Checksum does not match.");
   end;


   procedure Read_Signature (Streamer : not null access Ada.Streams.Root_Stream_Type'Class) is
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

   procedure Read_First_Chunk (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Item : in out PNG_Data_IHDR) is
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





   procedure Skip (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Count : Ada.Streams.Stream_Element_Count; Checksum : in out GNAT.CRC32.CRC32) is
      E : Stream_Element;
   begin
      for I in 1 .. Count loop
         Stream_Element'Read (Streamer, E);
         GNAT.CRC32.Update (Checksum, E);
      end loop;
   end Skip;


   procedure Read_Chunk_End_Arbitrary (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Data : out Stream_Element_Array; Checksum : in out GNAT.CRC32.CRC32) is
      Last : Stream_Element_Offset;
   begin
      Ada.Streams.Read (Streamer.all, Data, Last);
      Assert (Data'Last = Last);
      GNAT.CRC32.Update (Checksum, Data);
      Read_Chunk_End (Streamer, Checksum);
   end;

   procedure Read_Chunk_End_Header (Streamer : not null access Ada.Streams.Root_Stream_Type'Class; Item : out PNG_Complete_Fixed_Header; Length : Unsigned_32; Kind : PNG_Chunk_Kind; Checksum : in out GNAT.CRC32.CRC32) is
   begin
      case Kind is
         when PNG_Chunk_Kind_IHDR =>
            PNG_Data_IHDR'Read (Streamer, Item.Data_IHDR);
            Update1 (Checksum, Item.Data_IHDR'Address, Item.Data_IHDR'Size);
            Read_Chunk_End (Streamer, Checksum);
            Swap_Byte_Order (Item.Data_IHDR);

         when PNG_Chunk_Kind_IEND =>
            Assert (Length = 0);
            Read_Chunk_End (Streamer, Checksum);

         when others =>
            Skip (Streamer, Stream_Element_Count (Length), Checksum);
            Read_Chunk_End (Streamer, Checksum);

      end case;
   end;



   function Find_Channel_Count (Item : PNG_Color_Kind) return PNG_Channel_Count is
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

   function Find_Pixel_Byte_Depth (Color_Kind : PNG_Color_Kind; Bit_Depth : PNG_Bit_Depth) return PNG_Pixel_Byte_Depth is
      Channel_Count : PNG_Channel_Count;
      Pixel_Depth_Bit : Unsigned_32;
      Pixel_Depth_Byte : Unsigned_32;
   begin
      Channel_Count := Find_Channel_Count (Color_Kind);
      Pixel_Depth_Bit := PNG_Bit_Depth'Enum_Rep (Bit_Depth) * Channel_Count;
      Pixel_Depth_Byte := Shift_Right (Pixel_Depth_Bit + 7, 3);
      return Pixel_Depth_Byte;
   end;


end;
