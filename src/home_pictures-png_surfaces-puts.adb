with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with Interfaces;
with LZ77;


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

      Put (Head("Compression", Column_1_Width));
      Put (Tail (Surface.Chunk_IHDR.Compression'Img, Column_2_Width));
      New_Line;

      Put (Head("Filter", Column_1_Width));
      Put (Tail (Surface.Chunk_IHDR.Filter'Img, Column_2_Width));
      New_Line;

      Put (Head("Interlace", Column_1_Width));
      Put (Tail (Surface.Chunk_IHDR.Interlace'Img, Column_2_Width));
      New_Line (3);

      declare
         S : Unsigned_32;
         --I : Integer := PNG_Bit_Depth'Enum_Rep (Surface.Chunk_IHDR.Bit_Depth);
      begin
         S := Surface.Chunk_IHDR.Width * Surface.Chunk_IHDR.Height * 4;
         Put (Head("size", Column_1_Width));
         Put (Tail (S'Img, Column_2_Width));
         New_Line (3);
      end;


      Put (Surface.Chunk_List);
      New_Line;
   end;

   procedure Put_Image (Item : Stream_Element_Array; Width, Height : Unsigned_32) is
      use Ada.Text_IO;
      use Unsigned_8_Text_IO;
      use Ada.Integer_Text_IO;
   begin
      for E of Item loop
         Put (Standard_Output, Integer (E), 0); Put (" ");
      end loop;
   end;

   procedure Put_IDAT (Item : Stream_Element_Array) is
      use Unsigned_8_Text_IO;
      use Ada.Text_IO;
      I : Stream_Element_Offset := Item'First - 1;
      function Read_Byte return LZ77.Byte is
         R : LZ77.Byte;
      begin
         I := I + 1;
         R := LZ77.Byte (Item (I));
         return R;
      end;

      function More_Bytes return Boolean is
      begin
         return I < Item'Last;
      end;

      procedure Write_Literal (Item : LZ77.Byte) is
      begin
         Put (Item);
      end;

      procedure Write_DL_code (distance, length: Integer) is
      begin
         Put ("distance" & distance'Img);
         New_Line;
         Put ("length" & length'Img);
      end;

      procedure Encode is new LZ77.Encode (Method => LZ77.LZHuf, Read_byte => Read_Byte, More_Bytes => More_Bytes, Write_Literal => Write_Literal, Write_DL_code => Write_DL_code);
   begin
      Encode;
      null;
   end Put_IDAT;


   procedure Put_Image (Item : PNG_Surface) is
      use Ada.Text_IO;
      Kind_IDAT : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IDAT");
      Kind_gAMA : constant PNG_Chunk_Kind := Create_Chunk_Kind ("gAMA");
      Kind_pHYs : constant PNG_Chunk_Kind := Create_Chunk_Kind ("pHYs");
   begin
      for E of Item.Chunk_List loop
         if E.Kind = Kind_IDAT then
            Put_Line ("IDAT");
            Put_Image (E.Data.all, Item.Chunk_IHDR.Width, Item.Chunk_IHDR.Height);
            New_Line;
            Put_IDAT (E.Data.all);
            New_Line;
         elsif E.Kind = Kind_gAMA then
            Put_Line ("gAMA");
            Put_Image (E.Data.all, Item.Chunk_IHDR.Width, Item.Chunk_IHDR.Height);
            New_Line;
         elsif E.Kind = Kind_pHYs then
            Put_Line ("pHYs");
            Put_Image (E.Data.all, Item.Chunk_IHDR.Width, Item.Chunk_IHDR.Height);
            New_Line;
         end if;
      end loop;
   end;

end;
