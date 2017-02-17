with GL.C;


package body Home_Pictures.BMP_Images.GL_Compatible is

   procedure Read_Image (Filename : String; Tex : GL.Textures.Texture; Header : in out BMP_Header; Data : out System.Storage_Elements.Storage_Array) is
      use Ada.Streams.Stream_IO;
      Format : GL.Textures.Pixel_Format := GL.Textures.RGB_Pixel_Format;
      Pixel_Kind : GL.Textures.Pixel_Type;
      Width : GL.C.GLsizei;
      Height : GL.C.GLsizei;
      File : File_Type;
      Streamer : Stream_Access;
   begin
      Open (File, In_File, Filename);
      Streamer := Stream (File);
      BMP_Header'Read (Streamer, Header);

      Width := GL.C.GLsizei (Header.Information.Width);
      Height := GL.C.GLsizei (Header.Information.Height);

      case Header.Information.Pixel_Size is
      when 8 =>
         Pixel_Kind := GL.Textures.Byte_Pixel_Type;
      when others =>
         raise Program_Error with "Unsupported Pixel_Size";
      end case;

      Assert (Header.Information.Compress = None_Compression, "Unsupported compression");

--        case Header.Information.Color_Count is
--        when 1 =>
--           Format := GL.Textures.Red_Pixel_Format;
--        when 3 =>
--           Format := GL.Textures.RGB_Pixel_Format;
--        when 4 =>
--           Format := GL.Textures.RGBA_Pixel_Format;
--        when others =>
--           raise Program_Error with "Unsupported Color_Count";
--           null;
--        end case;

      GL.Textures.Load_3D (Tex, 0, 0, 0, Width, Height, 1, Format, Pixel_Kind, Data'Address);

   end;

end;
