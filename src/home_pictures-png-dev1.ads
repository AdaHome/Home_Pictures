with ztest;

package Home_Pictures.PNG.Dev1 is


   type Dev_Context is record
      File : Ada.Streams.Stream_IO.File_Type;
      Zip_Stream : ztest.Z_Native_Stream;
      Last_Row : PNG_Row_Index := 1;
      Kind : PNG_Chunk_Kind;
      Width : PNG_Width;
      Height : PNG_Height;
      Channel_Count : PNG_Channel_Count;
      Sample_Depth : PNG_Sample_Depth;
      Row_Size : PNG_Row_Size;
   end record;

   procedure Initialize (Context : in out Dev_Context; Filename : String);

   procedure Decode (Context : in out Dev_Context; Pixmap : in out PNG_Pixmap);

   procedure Devtool_Put_Info (Context : Dev_Context);

end;
