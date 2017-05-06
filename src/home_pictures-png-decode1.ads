with ztest;

package Home_Pictures.PNG.Decode1 is

   procedure Inflate_All
     (Z : in out ztest.Z_Native_Stream;
      Width : PNG_Width;
      Height : PNG_Height;
      Pixel_Depth_Byte : PNG_Pixel_Byte_Depth;
      Pixmap : out Stream_Element_Array);


   procedure Inflate_All (IDAT : Stream_Element_Array; Width : PNG_Width; Height : PNG_Height; Pixel_Byte_Depth : PNG_Pixel_Byte_Depth; Pixmap : out Stream_Element_Array);
   -- Creates a zstream and uncompresses the IDAT to pixmap.
   -- No data is allocated.

end Home_Pictures.PNG.Decode1;
