with GL.Textures;
with Ada.Streams;

package Home_Pictures.BMP.GL_Compatible is

   -- Just experimental. Not finnished.
   procedure Read_Image
     (Filename : String;
      Tex : GL.Textures.Texture;
      Surface : in out BMP_Information;
      Data : out Ada.Streams.Stream_Element_Array);

end;
