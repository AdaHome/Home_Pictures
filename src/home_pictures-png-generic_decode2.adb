with ztest;
with Ada.Text_IO;

package body Home_Pictures.PNG.Generic_Decode2 is


   procedure Initialize (Context : in out Decode_Context; Buffer : Ada.Streams.Stream_Element_Array) is
   begin
      Home_Pictures.PNG.Decode1.Initialize (Context.Z, Buffer);
   end;

   procedure Decode_Row is new Home_Pictures.PNG.Decode1.Generic_Decode_Row (Column_Index, Pixel, Row);

   procedure Decode_Row (Context : in out Decode_Context; Item : in out Row_Array) is
   begin
      if Context.Last_Row = 1 then
         Decode_Row (Context.Z, Zero_Row, Item (Context.Last_Row));
      else
         Decode_Row (Context.Z, Item (Context.Last_Row - 1), Item (Context.Last_Row));
      end if;
      Context.Last_Row := Row_Index'Succ (Context.Last_Row);
   end;

end Home_Pictures.PNG.Generic_Decode2;
