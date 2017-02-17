

package body Home_Pictures.BMP_Images is

   procedure Read_Image (Streamer : Stream_Access; Data : out BMP_Header) is
      --Offset : Positive_Count;
   begin
      BMP_Header'Read (Streamer, Data);
      --Set_Index (File, Offset);
   end;

end;
