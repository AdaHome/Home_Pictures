with Home_Pictures.PNG.Decode1;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body Home_Pictures.PNG.Dev1 is


   procedure Devtool_Put_Info (Context : Dev_Context) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
   begin
      Put ("Width         "); Put (Integer (Context.Width)); New_Line;
      Put ("Height        "); Put (Integer (Context.Height)); New_Line;
      Put ("Channel_Count "); Put (Integer (Context.Channel_Count)); New_Line;
      Put ("Sample_Depth  "); Put (Integer (Context.Sample_Depth)); New_Line;
      Put ("Row_Size      "); Put (Integer (Context.Row_Size)); New_Line;
   end;

   procedure Initialize (Context : in out Dev_Context; Filename : String) is
      use ztest;
      Data_IHDR : PNG_Data_IHDR;
   begin
      Open (Context.File, In_File, Filename);
      Read_Signature (Stream (Context.File));
      Initialize_Inflate (Context.Zip_Stream, 15);
      Read_Whole_Chunk_IHDR (Stream (Context.File), Data_IHDR);
      Context.Width := Data_IHDR.Width;
      Context.Height := Data_IHDR.Height;
      Context.Channel_Count := Find_Channel_Count (Data_IHDR.Color_Kind);
      Context.Sample_Depth := Convert (Data_IHDR.Bit_Depth);
      Context.Row_Size := Calculate_Row_Size (Context.Width, Context.Channel_Count, Context.Sample_Depth);
   end;




   procedure Decode (Context : in out Dev_Context; Pixmap : in out PNG_Pixmap) is
      use ztest;
      use Home_Pictures.PNG.Decode1;
      subtype V_Row is PNG_Byte_Array (1 .. Context.Row_Size);
      function Inflate is new ztest.Generic_Inflate (PNG_Filter_Type);
      function Inflate is new ztest.Generic_Inflate (V_Row);
      Current : V_Row with Address => Pixmap (Context.Last_Row, Pixmap'First (2), Pixmap'First (3), Pixmap'First (4))'Address;
      Zero_Row : PNG_Byte_Array (Current'Range) := (others => 0);
      Status : Z_Status;
      Filter : PNG_Filter_Type;
   begin
      Status := Inflate (Context.Zip_Stream, Filter, Z_Flush_None);
      Assert (Status = Z_Status_Ok, "Inflate filter. Status = " & Status'Img);
      Status := Inflate (Context.Zip_Stream, Current, Z_Flush_None);
      Assert (Status = Z_Status_Ok, "Inflate current row. Status = " & Status'Img);

      if Context.Last_Row = 1 then
         Run_Filter_Method_Zero (Filter, Pixmap'Length (PNG_Pixmap_Channel_Count) * Pixmap'Length (PNG_Pixmap_Sample_Depth), Zero_Row, Current);
      else
         declare
            Previous : V_Row with Address => Pixmap (Context.Last_Row - 1, Pixmap'First (2), Pixmap'First (3), Pixmap'First (4))'Address;
         begin
            Run_Filter_Method_Zero (Filter, Pixmap'Length (PNG_Pixmap_Channel_Count) * Pixmap'Length (PNG_Pixmap_Sample_Depth), Previous, Current);
         end;
      end if;
   end;

end;
