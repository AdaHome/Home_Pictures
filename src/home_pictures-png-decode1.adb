with Interfaces.C;
with Ada.Text_IO;

package body Home_Pictures.PNG.Decode1 is

   function Reconstruction_Function (Filter_Type : PNG_Filter_Type; X, A, B, C : Stream_Element) return Stream_Element is
   begin
      case Filter_Type is
      when PNG_Filter_Type_None =>
         return X;
      when PNG_Filter_Type_Sub =>
         return X + A;
      when PNG_Filter_Type_Up =>
         return X + B;
      when PNG_Filter_Type_Average =>
         return X + ((A + B) / 2);
      when PNG_Filter_Type_Paeth =>
         declare
            P, PA, PB, PC, PR : Stream_Element;
         begin
            P := A + B - C;
            PA := abs (P - A);
            PB := abs (P - B);
            PC := abs (P - C);
            if PA <= PB and PA <= PC then
               PR := A;
            elsif PB <= PC then
               PR := B;
            else
               PR := C;
            end if;
            return PR;
         end;
      end case;
   end;

   procedure Reconstruction_Procedure (Filter_Type : PNG_Filter_Type; Pixel_Depth_Byte : Stream_Element_Offset; Previous : in Stream_Element_Array; Current : in out Stream_Element_Array) is
   begin
      for I in Current'First .. Current'First + Pixel_Depth_Byte loop
         declare
            X : constant Stream_Element := Current (I);
            A : constant Stream_Element := 0;
            B : constant Stream_Element := Previous (I);
            C : constant Stream_Element := 0;
         begin
            Current (I) := Reconstruction_Function (Filter_Type, X, A, B, C);
         end;
      end loop;

      for I in Current'First + Pixel_Depth_Byte .. Current'Last loop
         declare
            X : constant Stream_Element := Current (I);
            A : constant Stream_Element := Current (I - Pixel_Depth_Byte);
            B : constant Stream_Element := Previous (I);
            C : constant Stream_Element := Previous (I - Pixel_Depth_Byte);
         begin
            Current (I) := Reconstruction_Function (Filter_Type, X, A, B, C);
         end;
      end loop;
   end;


   procedure Inflate_All
     (Z : in out ztest.Z_Native_Stream;
      Width : PNG_Width;
      Height : PNG_Height;
      Pixel_Depth_Byte : PNG_Pixel_Byte_Depth;
      Pixmap : out Stream_Element_Array)
   is
      use Ada.Assertions;
      use type Interfaces.C.unsigned;
      type Variant_Pixel is array (1 .. Pixel_Depth_Byte) of Stream_Element;
      type Variant_Row is array (1 .. Width) of Variant_Pixel;
      type Variant_Row_Array is array (1 .. Height) of Variant_Row;

      procedure Inflate_Filter_Type (Filter_Type : out PNG_Filter_Type) is
         use ztest;
         Status : Z_Status;
      begin
         Z.Output_Next := Filter_Type'Address;
         Z.Output_Available := 1;
         Status := Inflate (Z, Z_Flush_None);
         Assert (Status = Z_Status_Ok, Status'Img);
      end;

      procedure Inflate_Row (Row : in out Variant_Row) is
         use ztest;
         Status : Z_Status;
      begin
         Z.Output_Next := Row'Address;
         Z.Output_Available := Variant_Row'Size / Stream_Element'Size;
         Status := Inflate (Z, Z_Flush_None);
         Assert (Status = Z_Status_Ok, Status'Img);
      end;

      procedure Inflate_Last_Row (Row : in out Variant_Row) is
         use ztest;
         Status : Z_Status;
      begin
         Z.Output_Next := Row'Address;
         Z.Output_Available := Variant_Row'Size / Stream_Element'Size;
         Status := Inflate (Z, Z_Flush_None);
         Assert (Status = Z_Status_Stream_End, Status'Img);
      end;

      procedure Reconstruct_Row (F : PNG_Filter_Type; Previous : Variant_Row; Next : in out Variant_Row) is
         Data_Previous : Stream_Element_Array (1 .. Variant_Row'Size / Stream_Element'Size) with Address => Previous'Address;
         Data_Next : Stream_Element_Array (1 .. Variant_Row'Size / Stream_Element'Size) with Address => Next'Address;
      begin
         Reconstruction_Procedure (F, Stream_Element_Offset (Pixel_Depth_Byte), Data_Previous, Data_Next);
      end;

      Zero_Row : constant Variant_Row := (others => (others => 0));
      Filter : PNG_Filter_Type;
      Row_Array : Variant_Row_Array with Address => Pixmap'Address;

   begin
      Assert (Row_Array'Size = Pixmap'Size);

      Inflate_Filter_Type (Filter);
      Ada.Text_IO.Put_Line (Filter'Img);
      Inflate_Row (Row_Array (1));
      Reconstruct_Row (Filter, Zero_Row, Row_Array (1));

      for I in Row_Array'First + 1 .. Row_Array'Last - 1 loop
         Inflate_Filter_Type (Filter);
         Ada.Text_IO.Put_Line (Filter'Img);
         Inflate_Row (Row_Array (I));
         Reconstruct_Row (Filter, Row_Array (I - 1), Row_Array (I));
      end loop;

      Inflate_Filter_Type (Filter);
      Ada.Text_IO.Put_Line (Filter'Img);
      Inflate_Last_Row (Row_Array (Row_Array'Last));
      Reconstruct_Row (Filter, Row_Array (Row_Array'Last - 1), Row_Array (Row_Array'Last));
   end;


   procedure Inflate_All (IDAT : Stream_Element_Array; Width : PNG_Width; Height : PNG_Height; Pixel_Byte_Depth : PNG_Pixel_Byte_Depth; Pixmap : out Stream_Element_Array) is
      use ztest;
      Z : Z_Native_Stream;
   begin
      Initialize_Inflate (Z, 15);
      Set_Next_Input (Z, IDAT);
      Inflate_All (Z, Width, Height, Pixel_Byte_Depth, Pixmap);
   end;

end Home_Pictures.PNG.Decode1;
