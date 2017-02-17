with GL.Textures;
with System.Storage_Elements;

package Home_Pictures.BMP_Images.GL_Compatible is

   procedure Read_Image (Filename : String; Tex : GL.Textures.Texture; Header : in out BMP_Header; Data : out System.Storage_Elements.Storage_Array);

end;
