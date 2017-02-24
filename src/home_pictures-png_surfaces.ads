with Interfaces;
with System;
with Ada.Streams.Stream_IO;
with Ada.Assertions;
with Ada.Unchecked_Conversion;
with Home_Pictures.Swaps;
with System.Storage_Elements;
with Ada.Streams;
with Ada.Containers.Vectors;

package Home_Pictures.PNG_Surfaces is

   --All integers that require more than one byte shall be in network byte order.

   use System.Storage_Elements;
   use Ada.Streams;
   use Ada.Streams.Stream_IO;
   use Ada.Assertions;
   use Interfaces;
   use System;

   -- A 4-byte chunk type code. For convenience in description and in examining PNG files,
   -- type codes are restricted to consist of uppercase and lowercase ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
   -- However, encoders and decoders must treat the codes as fixed binary values, not character strings.
   -- For example, it would not be correct to represent the type code IDAT by the EBCDIC equivalents of those letters.
   -- Additional naming conventions for chunk types are discussed in the next section.
   subtype PNG_Chunk_Kind is Stream_Element_Array (0 .. 3);
   function Create_Chunk_Kind (Item : String) return PNG_Chunk_Kind;


   -- Color type is a single-byte integer that describes the interpretation of the image data.
   -- Color type codes represent sums of the following values:
   -- 1 (palette used), 2 (color used), and 4 (alpha channel used).
   -- Valid values are 0, 2, 3, 4, and 6.
   --
   -- * An indexed-color pixel is represented by a single sample that is an index into a supplied palette.
   --   The image bit depth determines the maximum number of palette entries, but not the color precision within the palette.
   -- * A grayscale pixel is represented by a single sample that is a grayscale level,
   --   where zero is black and the largest value for the bit depth is white.
   -- * A truecolor pixel is represented by three samples: red (zero = black, max = red) appears first,
   --   then green (zero = black, max = green), then blue (zero = black, max = blue).
   --   The bit depth specifies the size of each sample, not the total pixel size.
   type PNG_Color_Kind is (PNG_Greyscale, PNG_Truecolour, PNG_Indexed_Colour, PNG_Greyscale_With_Alpha, PNG_Truecolour_With_Alpha);

   -- Bit depth is a single-byte integer giving the number of bits
   -- per sample or per palette index (not per pixel).
   -- Valid values are 1, 2, 4, 8, and 16, although not all values are allowed for all color types.
   type PNG_Bit_Depth is (PNG_Bit_Depth_1, PNG_Bit_Depth_2, PNG_Bit_Depth_4, PNG_Bit_Depth_8, PNG_Bit_Depth_16);

   --     Color    Allowed    Interpretation
   --     Type    Bit Depths
   --     0       1,2,4,8,16  Each pixel is a grayscale sample.
   --     2       8,16        Each pixel is an R,G,B triple.
   --     3       1,2,4,8     Each pixel is a palette index;
   --                         a PLTE chunk must appear.
   --     4       8,16        Each pixel is a grayscale sample,
   --                         followed by an alpha sample.
   --     6       8,16        Each pixel is an R,G,B triple,
   --                         followed by an alpha sample.

   -- All implementations must understand and successfully render the standard critical chunks.
   -- A valid PNG image must contain an IHDR chunk, one or more IDAT chunks, and an IEND chunk.
   type PNG_Chunk is record
      -- A 4-byte unsigned integer giving the number of bytes in the chunk's data field.
      -- The length counts only the data field, not itself, the chunk type code, or the CRC.
      -- Zero is a valid length.
      -- Although encoders and decoders should treat the length as unsigned, its value must not exceed 231-1 bytes.
      Length : Unsigned_32;

      -- A 4-byte chunk type code. For convenience in description and in examining PNG files,
      -- type codes are restricted to consist of uppercase and lowercase ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
      -- However, encoders and decoders must treat the codes as fixed binary values, not character strings.
      -- For example, it would not be correct to represent the type code IDAT by the EBCDIC equivalents of those letters.
      -- Additional naming conventions for chunk types are discussed in the next section.
      Kind : PNG_Chunk_Kind;

      -- The data bytes appropriate to the chunk type, if any. This field can be of zero length.
      Data : access Stream_Element_Array;

      -- A 4-byte CRC (Cyclic Redundancy Check) calculated on the preceding bytes in the chunk,
      -- including the chunk type code and chunk data fields, but not including the length field.
      -- The CRC is always present, even for chunks containing no data. See CRC algorithm.
      -- Chunk CRCs are calculated using standard CRC methods with pre and post conditioning,
      -- as defined by ISO 3309 [ISO-3309] or ITU-T V.42 [ITU-T-V42].
      Checksum : Unsigned_32;
   end record with Pack;

   package PNG_Chunk_Vectors is new Ada.Containers.Vectors (Positive, PNG_Chunk);
   subtype PNG_Chunk_Vector is PNG_Chunk_Vectors.Vector;


   type PNG_Color_RGB_8 is array (0 .. 2) of Unsigned_8;
   type PNG_Color_RGB_8_Map is array (Unsigned_32 range <>, Unsigned_32 range <>) of PNG_Color_RGB_8;
   type PNG_Color_RGBA_8 is array (0 .. 3) of Unsigned_8;
   type PNG_Color_RGBA_8_Map is array (Unsigned_32 range <>, Unsigned_32 range <>) of PNG_Color_RGBA_8;

   --  The IHDR chunk must appear FIRST. It contains:
   --     Width:              4 bytes
   --     Height:             4 bytes
   --     Bit depth:          1 byte
   --     Color type:         1 byte
   --     Compression method: 1 byte
   --     Filter method:      1 byte
   --     Interlace method:   1 byte
   type PNG_Chunk_IHDR is record
      -- Width and height give the image dimensions in pixels.
      -- They are 4-byte integers. Zero is an invalid value.
      -- The maximum for each is 231-1 in order to accommodate languages that have difficulty with unsigned 4-byte values.
      Width             : Unsigned_32;
      Height            : Unsigned_32;
      Bit_Depth         : PNG_Bit_Depth;
      Color_Kind        : PNG_Color_Kind;
      -- Compression method is a single-byte integer that indicates the method used to compress the image data.
      -- At present, only compression method 0 (deflate/inflate compression with a sliding window of at most 32768 bytes) is defined.
      -- All standard PNG images must be compressed with this scheme.
      -- The compression method field is provided for possible future expansion or proprietary variants.
      -- Decoders must check this byte and report an error if it holds an unrecognized code. See Deflate/Inflate Compression for details.
      Compression       : Unsigned_8;
      -- Filter method is a single-byte integer that indicates the preprocessing method applied to the image data before compression.
      -- At present, only filter method 0 (adaptive filtering with five basic filter types) is defined.
      -- As with the compression method field, decoders must check this byte and report an error if it holds an unrecognized code.
      Filter            : Unsigned_8;
      -- Interlace method is a single-byte integer that indicates the transmission order of the image data.
      -- Two values are currently defined: 0 (no interlace) or 1 (Adam7 interlace). See Interlaced data order for details.
      Interlace         : Unsigned_8;
   end record;


   type PNG_Surface is record
      Chunk_IHDR : PNG_Chunk_IHDR;
      Chunk_List : PNG_Chunk_Vector;
   end record;


   procedure Read (Streamer : Stream_Access; Item : in out PNG_Surface);

private

   for PNG_Color_Kind'Size use 8;
   for PNG_Color_Kind use
     (
      PNG_Greyscale => 0,
      PNG_Truecolour => 2,
      PNG_Indexed_Colour => 3,
      PNG_Greyscale_With_Alpha => 4,
      PNG_Truecolour_With_Alpha => 6
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
