with Interfaces.C;
with Ada.Text_IO;

package body Home_Pictures.PNG.Decode1 is


   procedure Put_Base (Value : PNG_Byte; Width : Natural; Base : Positive) is
      Hex : constant array (PNG_Byte range 0 .. 15) of Character := "0123456789ABCDEF";
      B : PNG_Byte := Value;
      Buffer : String (1 .. Width) := (others => '0');
   begin
      for E of reverse Buffer loop
         E := Hex (B mod PNG_Byte (Base));
         B := B / PNG_Byte (Base);
         exit when B = 0;
      end loop;
      Ada.Text_IO.Put (Buffer);
      null;
   end;


   procedure Put_Base_Array (Item : Ada.Streams.Stream_Element_Array; Width : Natural; Base : Positive; Column : Positive) is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      for I in Item'Range loop
         Ada.Text_IO.Put (" ");
         Put_Base (PNG_Byte (Item (I)), Width, Base);
         if Natural (I - Item'First + 1) mod Column = 0 then
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end;


   procedure Initialize (Z : in out ztest.Z_Native_Stream; IDAT : Stream_Element_Array) is
      use ztest;
   begin
      Initialize_Inflate (Z, 15);
      Set_Next_Input (Z, IDAT);
   end;


   function Run_Filter_Method_Zero (Filter_Type : PNG_Filter_Type; X, A, B, C : PNG_Byte) return PNG_Byte is
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
            P, PA, PB, PC, PR : PNG_Byte;
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


   procedure Run_Filter_Method_Zero (Filter : PNG_Filter_Type; Pixel_Depth : PNG_Pixel_Byte_Depth; Previous : in PNG_Byte_Array; Current : in out PNG_Byte_Array) is
   begin
      for I in Current'First .. Current'First + Pixel_Depth loop
         declare
            X : constant PNG_Byte := Current (I);
            A : constant PNG_Byte := 0;
            B : constant PNG_Byte := Previous (I);
            C : constant PNG_Byte := 0;
         begin
            Current (I) := Run_Filter_Method_Zero (Filter, X, A, B, C);
         end;
      end loop;
      for I in Current'First + Pixel_Depth .. Current'Last loop
         declare
            X : constant PNG_Byte := Current (I);
            A : constant PNG_Byte := Current (I - Pixel_Depth);
            B : constant PNG_Byte := Previous (I);
            C : constant PNG_Byte := Previous (I - Pixel_Depth);
         begin
            Current (I) := Run_Filter_Method_Zero (Filter, X, A, B, C);
         end;
      end loop;
   end;


   generic
      type Index is (<>);
      type Pixel is private;
      type Row is array (Index) of Pixel;
   procedure Generic_Reconstruct (Filter : PNG_Filter_Type; Previous : in Row; Next : in out Row);

   procedure Generic_Reconstruct (Filter : PNG_Filter_Type; Previous : in Row; Next : in out Row) is
      subtype Byte_Array is PNG_Byte_Array (1 .. Row'Size / PNG_Byte'Size);
      Byte_Array_Previous : Byte_Array with Address => Previous'Address;
      Byte_Array_Next : Byte_Array with Address => Next'Address;
   begin
      Run_Filter_Method_Zero (Filter, Pixel'Size / PNG_Byte'Size, Byte_Array_Previous, Byte_Array_Next);
   end;


   procedure Generic_Decode_Row (Z : in out ztest.Z_Native_Stream; Previous : in Row; Current : out Row) is
      use ztest;
      function Inflate is new ztest.Generic_Inflate (Row);
      procedure Reconstruct is new Generic_Reconstruct (Index, Pixel, Row);
      function Inflate is new ztest.Generic_Inflate (PNG_Filter_Type);
      Status : Z_Status;
      Filter : PNG_Filter_Type;
   begin
      Status := Inflate (Z, Filter, Z_Flush_None);
      Assert (Status = Z_Status_Ok, "Inflate filter. Status = " & Status'Img);
      Status := Inflate (Z, Current, Z_Flush_None);
      Assert (Status = Z_Status_Ok, "Inflate current row. Status = " & Status'Img);
      Reconstruct (Filter, Previous, Current);
   end;


end Home_Pictures.PNG.Decode1;
