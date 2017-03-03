with GL.Textures;
with System.Storage_Elements;

package Home_Pictures.BMP.GL_Compatible is

   procedure Read_Image (Filename : String; Tex : GL.Textures.Texture; Surface : in out BMP_Information; Data : out System.Storage_Elements.Storage_Array);

end;
