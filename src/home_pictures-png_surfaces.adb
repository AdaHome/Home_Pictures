with Home_Pictures.Swaps;

package body Home_Pictures.PNG_Surfaces is


   procedure Read_Image (Streamer : Stream_Access; Surface : in out PNG_Surface) is
      PNG_Signature_Constant : constant PNG_Signature := (137, 80, 78, 71, 13, 10, 26, 10);
      IHDR : constant PNG_Chunk_Kind := (73, 72, 68, 82);
      use Home_Pictures.Swaps;
   begin
      -- The signature must have a PNG signature
      PNG_Signature'Read (Streamer, Surface.Signature);
      Assert (Surface.Signature = PNG_Signature_Constant);

      -- The PNG header (IHDR) chunk is allways first.
      PNG_Header_Chunk'Read (Streamer, Surface.Header);
      -- All integers that require more than one byte shall be in network byte order.
      -- This is not cross compatable. TODO: Change Bswap_32 to htonl.
      Surface.Header.Chunk_Data_Length := Bswap_32 (Surface.Header.Chunk_Data_Length);
      Surface.Header.Width := Bswap_32 (Surface.Header.Width);
      Surface.Header.Height := Bswap_32 (Surface.Header.Height);
   end;

end;
