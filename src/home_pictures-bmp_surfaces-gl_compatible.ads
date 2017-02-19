with GL.Textures;
with System.Storage_Elements;

package Home_Pictures.BMP_Surfaces.GL_Compatible is

   procedure Read_Image (Filename : String; Tex : GL.Textures.Texture; Surface : in out BMP_Surface; Data : out System.Storage_Elements.Storage_Array);

end;
