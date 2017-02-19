

package body Home_Pictures.BMP_Surfaces is

   procedure Read_Image (Streamer : Stream_Access; Surface : out BMP_Surface) is
      --Offset : Positive_Count;
   begin
      BMP_Surface'Read (Streamer, Surface);
      --Set_Index (File, Offset);
   end;

end;
