with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;

with Interfaces;

with Zip;
with Zip_Streams;

with Home_Streams.Memory_Overlays;

package body Home_Pictures.PNG.Puts is

   package Unsigned_32_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_32);
   package Unsigned_16_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_16);
   package Unsigned_8_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_8);
   package PNG_Pixel_Count_Text_IO is new Ada.Text_IO.Modular_IO (PNG_Pixel_Count);
   package PNG_Bit_Depth_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Bit_Depth);
   package PNG_Color_Kind_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Color_Kind);
   package PNG_Interlace_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Interlace);
   package PNG_Filter_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Filter);
   package PNG_Compression_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Compression);



   procedure Put_Kinds is
      Chunk_Kind_IDAT : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IDAT");
      Chunk_Kind_IEND : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IEND");
      Chunk_Kind_IDAT_32 : Unsigned_32 with Address => Chunk_Kind_IDAT'Address;
      Chunk_Kind_IEND_32 : Unsigned_32 with Address => Chunk_Kind_IEND'Address;
   begin
      Put_Line ("Chunk_Kind_IEND_32 " & Chunk_Kind_IDAT_32'Img);
      Put_Line ("Chunk_Kind_IEND_32 " & Chunk_Kind_IEND_32'Img);
   end;

   procedure Put (Item : Stream_Element_Array) is
      use Ada.Text_IO;
   begin
      for E of Item loop
         Put (Character'Val (E));
      end loop;
   end Put;

   procedure Put_IDAT (Item : Stream_Element_Array) is
      -- I'm trying to inflate uncompress IDAT data here.
      -- Then print out the uncompressed data.
      -- If the values look right I will add code to render the uncompressed data as OpenGL texture.
      --use Home_Streams.Memory_Overlays;
      --O : Overlay_Memory_Stream := Create (Item'Address, Item'Length);
      --Z : Zip.Zip_info;
   begin

      --Zip.Load (Z, Zip_Streams.Root_Zipstream_Type (Stream (O).all));
      --Put_Line ("Is_loaded " & Zip.Is_loaded (Z)'Img);
      null;
   end;

   procedure Put_Column (Item : String) is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
   begin
      Put (Head (Item, 20));
      Put ("| ");
   end;

   procedure Put_Line_Title (Item : String) is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
   begin
      Put_Line ("=== " & Item & " ===");
   end;

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
      Chunk_Kind_IDAT : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IDAT");
   begin
      Put_Column ("Length ");
      Put (Item.Length, Column_2_Width);
      New_Line;
      Put_Column ("Kind ");
      Put_Stream_Element_Array (Item.Kind, Column_2_Width);
      New_Line;

      if Item.Kind = Chunk_Kind_IDAT then
         Put_IDAT (Item.Data.all);
      end if;
   end Put;

   procedure Put (Item : PNG_Chunk_Vector) is
      use Ada.Text_IO;
   begin
      for E of Item loop
         Put (E);
         New_Line;
      end loop;
   end Put;

   procedure Put_Lines (Item : PNG_Information) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Fixed;
      use Interfaces;
      use Unsigned_32_Text_IO;
      use Unsigned_16_Text_IO;
      use Unsigned_8_Text_IO;
      use PNG_Color_Kind_Text_IO;
      use PNG_Bit_Depth_Text_IO;
      use PNG_Pixel_Count_Text_IO;
      use PNG_Interlace_Text_IO;
      use PNG_Filter_Text_IO;
      use PNG_Compression_Text_IO;
   begin

      Ada.Integer_Text_IO.Default_Width := 20;
      Unsigned_32_Text_IO.Default_Width := 20;
      Unsigned_16_Text_IO.Default_Width := 20;
      Unsigned_8_Text_IO.Default_Width := 20;
      PNG_Color_Kind_Text_IO.Default_Width := 20;
      PNG_Bit_Depth_Text_IO.Default_Width := 20;

      Put_Column ("Chunk count ");
      Put (Item.Chunk_Count);
      New_Line;

      Put_Column ("Width ");
      Put (Item.Chunk_Data_IHDR.Width);
      New_Line;

      Put_Column ("Height");
      Put (Item.Chunk_Data_IHDR.Height);
      New_Line;

      Put_Column ("Bit_Depth");
      Put (Item.Chunk_Data_IHDR.Bit_Depth);
      New_Line;

      Put_Column ("Color_Kind");
      Put (Item.Chunk_Data_IHDR.Color_Kind);
      New_Line;

      Put_Column ("Compression");
      Put (Item.Chunk_Data_IHDR.Compression);
      New_Line;

      Put_Column ("Filter");
      Put (Item.Chunk_Data_IHDR.Filter);
      New_Line;

      Put_Column ("Interlace");
      Put (Item.Chunk_Data_IHDR.Interlace);
      New_Line (3);

      Put_Line_Title ("Chunk_IDAT_List");
      Put (Item.Chunk_IDAT_List);

      Put_Line_Title ("Chunk_Unkown_List");
      Put (Item.Chunk_Unkown_List);

      New_Line;
   end;

   procedure Put_Image (Item : Stream_Element_Array) is
      use Ada.Text_IO;
      use Unsigned_8_Text_IO;
      use Ada.Integer_Text_IO;
   begin
      for E of Item loop
         Put (Standard_Output, Integer (E), 0); Put (" ");
      end loop;
   end;




   procedure Put_Image (Item : PNG_Information) is
      use Ada.Text_IO;
      Chunk_Kind_IDAT : constant PNG_Chunk_Kind := Create_Chunk_Kind ("IDAT");
      Chunk_Kind_gAMA : constant PNG_Chunk_Kind := Create_Chunk_Kind ("gAMA");
      Chunk_Kind_pHYs : constant PNG_Chunk_Kind := Create_Chunk_Kind ("pHYs");
   begin
      for E of Item.Chunk_Unkown_List loop
         if E.Kind = Chunk_Kind_IDAT then
            Put_Line ("IDAT:");
            Put_Image (E.Data.all);
            New_Line;
            Put_IDAT (E.Data.all);
            New_Line;
         elsif E.Kind = Chunk_Kind_gAMA then
            Put_Line ("gAMA");
            Put_Image (E.Data.all);
            New_Line;
         elsif E.Kind = Chunk_Kind_pHYs then
            Put_Line ("pHYs");
            Put_Image (E.Data.all);
            New_Line;
         end if;
      end loop;
   end;

end;
