with Interfaces;
with System;
with Ada.Streams.Stream_IO;
with Ada.Assertions;
with Ada.Unchecked_Conversion;
with Home_Pictures.Swaps;

package Home_Pictures.PNG_Surfaces is

   --All integers that require more than one byte shall be in network byte order.

   use Ada.Streams.Stream_IO;
   use Ada.Assertions;
   use Interfaces;
   use System;


   type PNG_Signature is array (0 .. 7) of Unsigned_8 with Pack;
   type PNG_Chunk_Kind_Index is range 0 .. 3;
   type PNG_Chunk_Kind is array (PNG_Chunk_Kind_Index) of Unsigned_8 with Pack;
   type PNG_Color_Kind is (PNG_Greyscale, PNG_Truecolour, PNG_Indexed_Colour, Greyscale_With_Alpha, Truecolour_With_Alpha);
   type PNG_Bit_Depth is (PNG_Bit_Depth_1, PNG_Bit_Depth_2, PNG_Bit_Depth_4, PNG_Bit_Depth_8, PNG_Bit_Depth_16);



   type PNG_Chunk_IHDR is record
      Chunk_Kind        : PNG_Chunk_Kind;
      Width             : Unsigned_32;
      Height            : Unsigned_32;
      Bit_Depth         : PNG_Bit_Depth;
      Color_Kind        : PNG_Color_Kind;
      Compression       : Unsigned_8;
      Filter            : Unsigned_8;
      Interlace         : Unsigned_8;
   end record;


   type PNG_Surface is record
      Signature : PNG_Signature;
      Chunk_IHDR : PNG_Chunk_IHDR;
   end record;

   procedure Read_Image (Streamer : Stream_Access; Surface : in out PNG_Surface);

private

   for PNG_Color_Kind'Size use 8;
   for PNG_Color_Kind use
     (
      PNG_Greyscale => 0,
      PNG_Truecolour => 2,
      PNG_Indexed_Colour => 3,
      Greyscale_With_Alpha => 4,
      Truecolour_With_Alpha => 6
     );

   for PNG_Bit_Depth'Size use 8;
   for PNG_Bit_Depth use
     (
      PNG_Bit_Depth_1 => 1,
      PNG_Bit_Depth_2 => 2,
      PNG_Bit_Depth_4 => 4,
      PNG_Bit_Depth_8 => 8,
      PNG_Bit_Depth_16 => 16
     );


end;
