with ztest;
with Home_Pictures.PNG.Decode1;

generic
   Pixel_Byte_Depth : PNG_Pixel_Byte_Depth;
   Width : PNG_Width;
   Height : PNG_Height;
package Home_Pictures.PNG.Generic_Decode2 is


   type Column_Index is new PNG_Pixel_Count range 1 .. Width;
   type Row_Index is new PNG_Pixel_Count range 1 .. Height;

   type Pixel is array (PNG_Byte_Count range 1 .. Pixel_Byte_Depth) of PNG_Byte;
   type Row is array (Column_Index) of Pixel;
   type Row_Array is array (Row_Index) of Row;

   Zero_Pixel : constant Pixel := (others => 0);
   Zero_Row : constant Row := (others => Zero_Pixel);

   type Decode_Context is record
      Z : ztest.Z_Native_Stream;
      Last_Row : Row_Index := 1;
   end record;

   procedure Initialize (Context : in out Decode_Context; Buffer : Ada.Streams.Stream_Element_Array);

   procedure Decode_Row (Context : in out Decode_Context; Item : in out Row_Array);




end Home_Pictures.PNG.Generic_Decode2;
