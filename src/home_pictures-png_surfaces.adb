with Home_Pictures.Swaps;
with GNAT.CRC32;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

package body Home_Pictures.PNG_Surfaces is


   function Calc_Checksum (Item : PNG_Chunk_IHDR) return Unsigned_32 is
      use GNAT.CRC32;
      use Ada.Streams;
--        type Stream_Element_Array13_Access is access all Stream_Element_Array (0 .. 12);
--        function Convert is new Ada.Unchecked_Conversion (Address, Stream_Element_Array13_Access);
--        A : Stream_Element_Array13_Access;
      B : Stream_Element_Array (0 .. 16) with Address => Item'Address;
      C : CRC32;
   begin
      Initialize (C);
      Update (C, B);
      return Get_Value (C);
   end;

   procedure Read_Image (Streamer : Stream_Access; Surface : in out PNG_Surface) is
      use Home_Pictures.Swaps;
      PNG_Signature_Constant : constant PNG_Signature := (137, 80, 78, 71, 13, 10, 26, 10);
      IHDR : constant PNG_Chunk_Kind := (73, 72, 68, 82);
      Length : Unsigned_32;
      Checksum_CRC32 : Unsigned_32;
   begin
      -- The first 4 bytes must be a PNG signature.
      PNG_Signature'Read (Streamer, Surface.Signature);
      Assert (Surface.Signature = PNG_Signature_Constant, "The signature does not match a PNG signature");

      -- The chunk length is the first 4 bytes for each chunk.
      Unsigned_32'Read (Streamer, Length);
      Length := Bswap_32 (Length);

      Assert (Length = 13, "The IHDR chunk size must be 13 bytes.");

      -- The PNG header (IHDR) chunk is allways first.
      PNG_Chunk_IHDR'Read (Streamer, Surface.Chunk_IHDR);
      Unsigned_32'Read (Streamer, Checksum_CRC32);

      -- All integers that require more than one byte shall be in network byte order.
      -- This is not cross compatable. TODO: Change Bswap_32 to htonl.
      --Surface.Chunk_IHDR.Width := Bswap_32 (Surface.Chunk_IHDR.Width);
      --Surface.Chunk_IHDR.Height := Bswap_32 (Surface.Chunk_IHDR.Height);
      Checksum_CRC32 := Bswap_32 (Checksum_CRC32);
      Put_Line ("Checksum_CRC32'Img: " & Checksum_CRC32'Img);
      Put_Line ("Calc_Checksum'Img: " & Calc_Checksum (Surface.Chunk_IHDR)'Img);

   end;

end;
