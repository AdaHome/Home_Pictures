with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Interfaces;

package body Home_Pictures.PNG_Surfaces.Puts is


   procedure Put_Lines (Surface : PNG_Surface) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Fixed;
      use Interfaces;
      package Unsigned_32_Text_IO is new Modular_IO (Unsigned_32);
      package Unsigned_16_Text_IO is new Modular_IO (Unsigned_16);
      package Unsigned_8_Text_IO is new Modular_IO (Unsigned_8);
      use Unsigned_32_Text_IO;
      use Unsigned_16_Text_IO;
      use Unsigned_8_Text_IO;
      Column_1_Width : constant := 25;
      Column_2_Width : constant := 20;
   begin
      Put (Head ("Signature", Column_1_Width));
      for E : Unsigned_8 of Surface.Signature loop
        Put (Standard_Output, E, 0, 10); Put (" ");
      end loop;
      New_Line;

      Put (Head ("Chunk_Kind", Column_1_Width));
      for E : Unsigned_8 of Surface.Chunk_IHDR.Chunk_Kind loop
        Put (Standard_Output, E, 0, 10); Put (" ");
      end loop;
      New_Line;

      Put (Head("Width ", Column_1_Width));
      Put (Surface.Chunk_IHDR.Width, Column_2_Width);
      New_Line;

      Put (Head("Height", Column_1_Width));
      Put (Surface.Chunk_IHDR.Height, Column_2_Width);
      New_Line;

      Put (Head("Bit_Depth", Column_1_Width));
      Put (Tail (Surface.Chunk_IHDR.Bit_Depth'Img, Column_2_Width));
      New_Line;

      Put (Head("Color_Kind", Column_1_Width));
      Put (Tail (Surface.Chunk_IHDR.Color_Kind'Img, Column_2_Width));
      New_Line;



   end;



end;
