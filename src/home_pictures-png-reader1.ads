package Home_Pictures.PNG.Reader1 is

   type PNG_Reader_Context is record
      File : File_Type;
      Kind : PNG_Chunk_Kind;
      Header : PNG_Complete_Fixed_Header;
   end record;

   procedure Initialize (Context : in out PNG_Reader_Context; Filename : String);

   procedure Read (Context : in out PNG_Reader_Context; Buffer : out Stream_Element_Array; Last : out Stream_Element_Offset);

end Home_Pictures.PNG.Reader1;
