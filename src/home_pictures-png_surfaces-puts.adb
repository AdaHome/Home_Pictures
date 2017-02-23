with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with Interfaces;

package body Home_Pictures.PNG_Surfaces.Puts is

   package Unsigned_32_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_32);
   package Unsigned_16_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_16);
   package Unsigned_8_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_8);



   procedure Put_Stream_Element_Array (Item : Stream_Element_Array; Width : Positive) is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
   begin
      Put ((Width - Item'Length) * " ");
      for E of Item loop
         Put (Character'Val (E));
      end loop;
   end;

   procedure Put (Item : PNG_Chunk) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Fixed;
      use Interfaces;
      use Unsigned_32_Text_IO;
      use Unsigned_16_Text_IO;
      use Unsigned_8_Text_IO;
      Column_1_Width : constant := 25;
      Column_2_Width : constant := 20;
   begin
      Put (Head("Length ", Column_1_Width));
      Put (Item.Length, Column_2_Width);
      New_Line;
      Put (Head("Kind ", Column_1_Width));
      Put_Stream_Element_Array (Item.Kind, Column_2_Width);
      New_Line;
   end Put;

   procedure Put (Item : PNG_Chunk_Vector) is
      use Ada.Text_IO;
   begin
      for E of Item loop
         Put (E);
         New_Line;
      end loop;
   end Put;

   procedure Put_Lines (Surface : PNG_Surface) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Fixed;
      use Interfaces;
      use Unsigned_32_Text_IO;
      use Unsigned_16_Text_IO;
      use Unsigned_8_Text_IO;
      Column_1_Width : constant := 25;
      Column_2_Width : constant := 20;
   begin
      Put (Head("Chunk count ", Column_1_Width));
      Put (Integer (Surface.Chunk_List.Length), Column_2_Width);
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

      Put (Surface.Chunk_List);
      New_Line;
   end;



end;
