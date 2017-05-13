with ztest;

package Home_Pictures.PNG.Decode1 is

   procedure Put_Base (Value : PNG_Byte; Width : Natural; Base : Positive);
   procedure Put_Base_Array (Item : Ada.Streams.Stream_Element_Array; Width : Natural; Base : Positive; Column : Positive);


   procedure Initialize (Z : in out ztest.Z_Native_Stream; IDAT : Stream_Element_Array);

   generic
      type Index is (<>);
      type Pixel is private;
      type Row is array (Index) of Pixel;
   procedure Generic_Decode_Row (Z : in out ztest.Z_Native_Stream; Previous : in Row; Current : out Row);

end Home_Pictures.PNG.Decode1;
