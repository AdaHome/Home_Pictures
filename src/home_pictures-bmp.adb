

package body Home_Pictures.BMP is

   procedure Read_Image (Streamer : Stream_Access; Surface : out BMP_Information) is
      --Offset : Positive_Count;
   begin
      BMP_Information'Read (Streamer, Surface);
      --Set_Index (File, Offset);
   end;

end;
