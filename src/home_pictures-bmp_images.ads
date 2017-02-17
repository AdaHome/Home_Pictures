with Interfaces;
with System;
with Ada.Streams.Stream_IO;
with Ada.Assertions;

package Home_Pictures.BMP_Images is

   use Ada.Streams.Stream_IO;
   use Ada.Assertions;
   use Interfaces;
   use System;

   type Compression is (None_Compression, Run_Length_Encoding_8_Compression);
   for Compression'Size use 32;
   for Compression use (None_Compression => 0, Run_Length_Encoding_8_Compression => 1);

   type Signature is (BM_Signature);
   for Signature'Size use 16;
   for Signature use (BM_Signature => 16#4D42#);

   type File_Header is record
      Sign       : Signature;
      Size       : Unsigned_32; -- File size in bytes
      Reserved_1 : Unsigned_16;
      Reserved_2 : Unsigned_16;
      Offset     : Unsigned_32; -- Offset bytes to pixel data.
   end record;



   -- DIB header
   -- BITMAPINFOHEADER structure
   -- https://msdn.microsoft.com/en-us/library/windows/desktop/dd183376(v=vs.85).aspx
   type Information_Header is record
      Header_Size           : Unsigned_32; -- Number of bytes in the DIB header (from this point)
      Width                 : Unsigned_32; -- Image width in pixels
      Height                : Unsigned_32; -- Image hieght in pixels
      Plane_Count           : Unsigned_16; -- Number of color planes being used
      Pixel_Size            : Unsigned_16; -- Bits per pixel. must be in (0 | 1 | 2 | 4 | 8 | 16 | 24 | 32).
      Compress              : Compression;
      Image_Size            : Unsigned_32; -- Size of the image data in bytes
      Horizontal_Resolution : Unsigned_32; -- Pixels per meter in horizontal axis
      Vertical_Resolution   : Unsigned_32; -- Pixels per meter in vertical axis
      Color_Count           : Unsigned_32; -- Number of colors used
      Important_Color_Count : Unsigned_32; -- Number of important colors. 0 means all colors are important
   end record;

   -- Assert (Information_Header'Size / Storage_Unit = 40);



   type Information_Header_V5 is record
      Header_Size           : Unsigned_32; -- Number of bytes in the DIB header (from this point)
      Width                 : Unsigned_32; -- Image width in pixels
      Height                : Unsigned_32; -- Image hieght in pixels
      Plane_Count           : Unsigned_16; -- Number of color planes being used
      Pixel_Size            : Unsigned_16; -- Bits per pixel
      Compress              : Compression;
      Image_Size            : Unsigned_32; -- Size of the image data in bytes
      Horizontal_Resolution : Unsigned_32; -- Pixels per meter in horizontal axis
      Vertical_Resolution   : Unsigned_32; -- Pixels per meter in vertical axis
      Color_Count           : Unsigned_32; -- Number of colors used
      Important_Color_Count : Unsigned_32; -- Number of important colors. 0 means all colors are important
      Red_Channel_Bitmask   : Unsigned_32;
      Green_Channel_Bitmask : Unsigned_32;
      Blue_Channel_Bitmask  : Unsigned_32;
      Alpha_Channel_Bitmask : Unsigned_32;
      Color_Space_Type      : Unsigned_32;
      Red_X                 : Unsigned_32;
      Red_Y                 : Unsigned_32;
      Red_Z                 : Unsigned_32;
      Green_X               : Unsigned_32;
      Green_Y               : Unsigned_32;
      Green_Z               : Unsigned_32;
      Blue_X                : Unsigned_32;
      Blue_Y                : Unsigned_32;
      Blue_Z                : Unsigned_32;
      Gamma_Red             : Unsigned_32;
      Gamma_Green           : Unsigned_32;
      Gamma_Blue            : Unsigned_32;
      Intent                : Unsigned_32;
      Profile_Data          : Unsigned_32;
      Profile_Size          : Unsigned_32;
      Reserved              : Unsigned_32;
   end record;


   type BMP_Header is record
      File        : File_Header;
      Information : Information_Header;
   end record with Pack;

   type BMP_Header_V5 is record
      File        : File_Header;
      Information : Information_Header_V5;
   end record;

   type Pixel_RGB is (Red, Green, Blue);
   type Pixel_RGBA is (Red, Green, Blue, Alpha);
   subtype Component_8 is Unsigned_8;

   -- 0 .. 255 = black .. white
   subtype Pixel_8_Grayscale is Component_8;

   type Pixel_24_RGB is array (Pixel_RGB) of Component_8;
   type Pixel_32_RGBA is array (Pixel_RGBA) of Component_8;


   procedure Read_Image (Streamer : Stream_Access; Data : out BMP_Header);

end;
