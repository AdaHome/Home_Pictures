with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Containers.Vectors;
with Interfaces;

with Zip;
with Zip_Streams;

with Home_Streams.Memory_Overlays;

package body Home_Pictures.PNG.Puts is

   package Unsigned_32_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_32);
   package Unsigned_16_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_16);
   package Unsigned_8_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned_8);
   package PNG_Bit_Depth_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Bit_Depth);
   package PNG_Color_Kind_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Color_Kind);
   package PNG_Interlace_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Interlace);
   package PNG_Filter_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Filter);
   package PNG_Compression_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Compression);
   package PNG_Chunk_Kind_Text_IO is new Ada.Text_IO.Enumeration_IO (PNG_Chunk_Kind);



   procedure Put_Kind_Static_Expression is
      function "<" (A, B : PNG_Chunk_Kind_String) return Boolean is
      begin
         return Create_Chunk_Kind_32 (A) < Create_Chunk_Kind_32 (B);
      end "<";
      package PNG_Chunk_Kind_String_Vectors is new Ada.Containers.Vectors (Natural, PNG_Chunk_Kind_String);
      package PNG_Chunk_Kind_String_Vectors_Sorting is new PNG_Chunk_Kind_String_Vectors.Generic_Sorting;

      Pre_Name : constant String := "PNG_Chunk_Kind";

      List : PNG_Chunk_Kind_String_Vectors.Vector;

   begin

      for E of Chunk_Kind_List loop
         PNG_Chunk_Kind_String_Vectors.Append (List, E);
      end loop;


      PNG_Chunk_Kind_String_Vectors_Sorting.Sort (List);

      Put ("type ");
      Put (Pre_Name);
      Put (" is ");
      New_Line;
      Put_Line ("(");
      for E of List loop
         Put (Pre_Name);
         Put ("_");
         Put (E);
         exit when E = List.Last_Element;
         Put (", ");
         New_Line;
      end loop;
      New_Line;
      Put (");");

      New_Line;

      Put ("for ");
      Put (Pre_Name);
      Put (" use");
      New_Line;
      Put_Line ("(");
      for E of List loop
         Put (Pre_Name);
         Put ("_");
         Put (E);
         Put (" =>");
         Put (Create_Chunk_Kind_32 (E)'Img);
         exit when E = List.Last_Element;
         Put (",");
         New_Line;
      end loop;
      New_Line;
      Put_Line (");");
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
      use PNG_Chunk_Kind_Text_IO;
      Column_1_Width : constant := 25;
      Column_2_Width : constant := 20;
      --Chunk_Kind_IDAT : constant PNG_Chunk_Kind_Sequence := Create_Chunk_Kind ("IDAT");
   begin
      Put_Column ("Length ");
      Put (Item.Length, Column_2_Width);
      New_Line;
      Put (Item.Kind);
--        if Item.Kind = Chunk_Kind_IDAT then
--           Put_IDAT (Item.Data.all);
--        end if;
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
      Put (Item.Data_IHDR.Width);
      New_Line;

      Put_Column ("Height");
      Put (Item.Data_IHDR.Height);
      New_Line;

      Put_Column ("Bit_Depth");
      Put (Item.Data_IHDR.Bit_Depth);
      New_Line;

      Put_Column ("Color_Kind");
      Put (Item.Data_IHDR.Color_Kind);
      New_Line;

      Put_Column ("Compression");
      Put (Item.Data_IHDR.Compression);
      New_Line;

      Put_Column ("Filter");
      Put (Item.Data_IHDR.Filter);
      New_Line;

      Put_Column ("Interlace");
      Put (Item.Data_IHDR.Interlace);
      New_Line (3);

      Put_Line_Title ("Chunk_IDAT_List");
      Put (Item.Data_IDAT_List);

      Put_Line_Title ("Chunk_Unkown_List");
      Put (Item.Data_Unkown_List);

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

   procedure Put (Item : PNG_Small_Chunk) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Unsigned_32_Text_IO;
      use PNG_Chunk_Kind_Text_IO;
   begin
      Put (Item.Length);
      Put ("|");
      Put (Item.Kind);
   end Put;

   procedure Put_Lines (Item : PNG_Small_Chunk_Vector) is
   begin
      for E of Item loop
         Put (E);
         New_Line;
      end loop;
   end Put_Lines;

   procedure Put_Image (Item : PNG_Information) is
      use Ada.Text_IO;
--        Chunk_Kind_IDAT : constant PNG_Chunk_Kind_Sequence := Create_Chunk_Kind ("IDAT");
--        Chunk_Kind_gAMA : constant PNG_Chunk_Kind_Sequence := Create_Chunk_Kind ("gAMA");
--        Chunk_Kind_pHYs : constant PNG_Chunk_Kind_Sequence := Create_Chunk_Kind ("pHYs");
   begin
--        for E of Item.Chunk_Unkown_List loop
--           if E.Kind = Chunk_Kind_IDAT then
--              Put_Line ("IDAT:");
--              Put_Image (E.Data.all);
--              New_Line;
--              Put_IDAT (E.Data.all);
--              New_Line;
--           elsif E.Kind = Chunk_Kind_gAMA then
--              Put_Line ("gAMA");
--              Put_Image (E.Data.all);
--              New_Line;
--           elsif E.Kind = Chunk_Kind_pHYs then
--              Put_Line ("pHYs");
--              Put_Image (E.Data.all);
--              New_Line;
--           end if;
--        end loop;
      null;
   end;

end;
