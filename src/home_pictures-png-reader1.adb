with Ada.Text_IO;

package body Home_Pictures.PNG.Reader1 is

   procedure Initialize (Context : in out PNG_Reader_Context; Filename : String) is
      Length : Unsigned_32;
      Checksum : GNAT.CRC32.CRC32;
   begin
      Open (Context.File, In_File, Filename);
      Read_Signature (Stream (Context.File));
      Read_Chunk_Begin (Stream (Context.File), Length, Context.Kind, Checksum);
      Assert (Context.Kind = PNG_Chunk_Kind_IHDR);
      Read_Chunk_End_Header (Stream (Context.File), Context.Header, Length, Context.Kind, Checksum);
   end Initialize;


   procedure Read (Context : in out PNG_Reader_Context; Buffer : out Stream_Element_Array; Last : out Stream_Element_Offset) is
      Length : Unsigned_32;
      Checksum : GNAT.CRC32.CRC32;
   begin
      Assert (Context.Kind /= PNG_Chunk_Kind_IEND);
      Read_Chunk_Begin (Stream (Context.File), Length, Context.Kind, Checksum);
      Ada.Text_IO.Put_Line (Length'Img & "|" & Context.Kind'Img);
      case Context.Kind is
         when PNG_Chunk_Kind_IDAT =>
            Last := Buffer'First + Stream_Element_Count (Length) - 1;
            Read_Chunk_End_Arbitrary (Stream (Context.File), Buffer (Buffer'First .. Last), Checksum);
         when others =>
            Read_Chunk_End_Header (Stream (Context.File), Context.Header, Length, Context.Kind, Checksum);
      end case;
   end;

end Home_Pictures.PNG.Reader1;
