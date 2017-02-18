with Home_Pictures.Swaps;

package body Home_Pictures.PNG_Images is


   procedure Read_Image (Streamer : Stream_Access; Data : in out PNG) is
      use Home_Pictures.Swaps;
   begin
      PNG_Signature'Read (Streamer, Data.Signature);
      PNG_Header_Chunk'Read (Streamer, Data.Header);
      Data.Header.Chunk_Data_Length := Bswap_32 (Data.Header.Chunk_Data_Length);
      Data.Header.Width := Bswap_32 (Data.Header.Width);
      Data.Header.Height := Bswap_32 (Data.Header.Height);
   end;

end;
