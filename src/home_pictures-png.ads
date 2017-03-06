with Interfaces;

with System;
with System.Storage_Elements;

with Ada.Streams.Stream_IO;
with Ada.Assertions;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Ada.Containers.Vectors;

with GNAT.CRC32;

with Home_Pictures.Swaps;


package Home_Pictures.PNG is

   use System.Storage_Elements;
   use Ada.Streams;
   use Ada.Streams.Stream_IO;
   use Ada.Assertions;
   use Interfaces;
   use System;

   subtype PNG_Chunk_Kind_String is String (1 .. 4);
   -- PNG chunk kind names consist of string characters.

   subtype PNG_Chunk_Kind is Stream_Element_Array (0 .. 3);
   -- A 4-byte chunk type code. For convenience in description and in examining PNG files,
   -- type codes are restricted to consist of uppercase and lowercase ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
   -- However, encoders and decoders must treat the codes as fixed binary values, not character strings.
   -- For example, it would not be correct to represent the type code IDAT by the EBCDIC equivalents of those letters.
   -- Additional naming conventions for chunk types are discussed in the next section.

   type PNG_Color_Kind is (PNG_Greyscale, PNG_Truecolour, PNG_Indexed_Colour, PNG_Greyscale_With_Alpha, PNG_Truecolour_With_Alpha);
   -- Color type is a single-byte integer that describes the interpretation of the image data.
   -- * An indexed-color pixel is represented by a single sample that is an index into a supplied palette.
   --   The image bit depth determines the maximum number of palette entries, but not the color precision within the palette.
   -- * A grayscale pixel is represented by a single sample that is a grayscale level,
   --   where zero is black and the largest value for the bit depth is white.
   -- * A truecolor pixel is represented by three samples: red (zero = black, max = red) appears first,
   --   then green (zero = black, max = green), then blue (zero = black, max = blue).
   --   The bit depth specifies the size of each sample, not the total pixel size.
   -- Color Type                    | Allowed Bit Depths  | Interpretation
   -- PNG_Greyscale             = 0 | 1, 2, 4, 8, 16      | Each pixel is a grayscale sample.
   -- PNG_Truecolour            = 2 |          8, 16      | Each pixel is an R,G,B triple.
   -- PNG_Indexed_Colour        = 3 | 1, 2, 4, 8          | Each pixel is a palette index; a PLTE chunk must appear.
   -- PNG_Greyscale_With_Alpha  = 4 | 8,          16      | Each pixel is a grayscale sample, followed by an alpha sample.
   -- PNG_Truecolour_With_Alpha = 6 | 8,          16      | Each pixel is an R,G,B triple,followed by an alpha sample.

   type PNG_Bit_Depth is (PNG_Bit_Depth_1, PNG_Bit_Depth_2, PNG_Bit_Depth_4, PNG_Bit_Depth_8, PNG_Bit_Depth_16);
   -- Bit depth is a single-byte integer giving the number of bits
   -- per sample or per palette index (not per pixel).
   -- Valid values are 1, 2, 4, 8, and 16, although not all values are allowed for all color types.
   -- Color Type                    | Allowed Bit Depths  | Interpretation
   -- PNG_Greyscale             = 0 | 1, 2, 4, 8, 16      | Each pixel is a grayscale sample.
   -- PNG_Truecolour            = 2 |          8, 16      | Each pixel is an R,G,B triple.
   -- PNG_Indexed_Colour        = 3 | 1, 2, 4, 8          | Each pixel is a palette index; a PLTE chunk must appear.
   -- PNG_Greyscale_With_Alpha  = 4 | 8,          16      | Each pixel is a grayscale sample, followed by an alpha sample.
   -- PNG_Truecolour_With_Alpha = 6 | 8,          16      | Each pixel is an R,G,B triple,followed by an alpha sample.

   type PNG_Pixel_Count is new Unsigned_32;
   subtype PNG_Width is PNG_Pixel_Count;
   subtype PNG_Height is PNG_Pixel_Count;
      -- Width and height give the image dimensions in pixels.
      -- They are 4-byte integers. Zero is an invalid value.

   type PNG_Interlace is (PNG_Interlace_None, PNG_Interlace_Adam7);
   -- Interlace method is a single-byte integer that indicates the transmission order of the image data.
   -- Two values are currently defined: 0 (no interlace) or 1 (Adam7 interlace). See Interlaced data order for details.

   type PNG_Filter is (PNG_Filter_Method_0);
   -- Filter method is a single-byte integer that indicates the preprocessing method applied to the image data before compression.
   -- At present, only filter method 0 (adaptive filtering with five basic filter types) is defined.
   -- As with the compression method field, decoders must check this byte and report an error if it holds an unrecognized code.

   type PNG_Compression is (PNG_Compression_Method_0);
   -- Compression method is a single-byte integer that indicates the method used to compress the image data.
   -- At present, only compression method 0 (deflate/inflate compression with a sliding window of at most 32768 bytes) is defined.
   -- All standard PNG images must be compressed with this scheme.
   -- The compression method field is provided for possible future expansion or proprietary variants.
   -- Decoders must check this byte and report an error if it holds an unrecognized code. See Deflate/Inflate Compression for details.
   -- http://www.libpng.org/pub/png/spec/1.2/PNG-Compression.html
   -- For PNG compression method 0,
   -- the zlib compression method/flags code must specify method code 8 ("deflate" compression) and
   -- an LZ77 window size of not more than 32768 bytes.
   -- Note that the zlib compression method number is not the same as the PNG compression method number.
   -- The additional flags must not specify a preset dictionary.
   -- A PNG decoder must be able to decompress any valid zlib datastream that satisfies these additional constraints.

   type PNG_Chunk is record
      Length : Unsigned_32;
      -- A 4-byte unsigned integer giving the number of bytes in the chunk's data field.
      -- The length counts only the data field, not itself, the chunk type code, or the CRC.
      -- Zero is a valid length.
      -- Although encoders and decoders should treat the length as unsigned, its value must not exceed 231-1 bytes.

      Kind : PNG_Chunk_Kind;
      -- A 4-byte chunk type code. For convenience in description and in examining PNG files,
      -- type codes are restricted to consist of uppercase and lowercase ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
      -- However, encoders and decoders must treat the codes as fixed binary values, not character strings.
      -- For example, it would not be correct to represent the type code IDAT by the EBCDIC equivalents of those letters.
      -- Additional naming conventions for chunk types are discussed in the next section.

      Data : access Stream_Element_Array;
      -- The data bytes appropriate to the chunk type, if any. This field can be of zero length.

      Checksum : Unsigned_32;
      -- A 4-byte CRC (Cyclic Redundancy Check) calculated on the preceding bytes in the chunk,
      -- including the chunk type code and chunk data fields, but not including the length field.
      -- The CRC is always present, even for chunks containing no data.
      -- Chunk CRCs are calculated using standard CRC methods with pre and post conditioning,
      -- as defined by ISO 3309 [ISO-3309] or ITU-T V.42 [ITU-T-V42].
      -- The CRC polynomial employed is x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1

   end record with Pack;


   package PNG_Chunk_Vectors is new Ada.Containers.Vectors (Positive, PNG_Chunk);
   subtype PNG_Chunk_Vector is PNG_Chunk_Vectors.Vector;

   type PNG_Color_RGB_8 is array (0 .. 2) of Unsigned_8;
   type PNG_Color_RGB_8_Map is array (Unsigned_32 range <>, Unsigned_32 range <>) of PNG_Color_RGB_8;
   type PNG_Color_RGBA_8 is array (0 .. 3) of Unsigned_8;
   type PNG_Color_RGBA_8_Map is array (Unsigned_32 range <>, Unsigned_32 range <>) of PNG_Color_RGBA_8;

   type PNG_Chunk_Data_IHDR is record
      Width             : PNG_Width;
      Height            : PNG_Height;
      Bit_Depth         : PNG_Bit_Depth;
      Color_Kind        : PNG_Color_Kind;
      Compression       : PNG_Compression;
      Filter            : PNG_Filter;
      Interlace         : PNG_Interlace;
   end record;
   --  The IHDR chunk must appear FIRST. It contains:
   --     Width:              4 bytes
   --     Height:             4 bytes
   --     Bit depth:          1 byte
   --     Color type:         1 byte
   --     Compression method: 1 byte
   --     Filter method:      1 byte
   --     Interlace method:   1 byte

   type PNG_Information is record
      Chunk_Count : Natural := 0;

      Chunk_Data_IHDR : PNG_Chunk_Data_IHDR;
      -- IHDR must appear first and IEND must appear last;
      -- thus the IEND chunk serves as an end-of-file marker

      Chunk_IDAT_List : PNG_Chunk_Vector;
      -- There can be multiple IDAT chunks; if so, they must appear consecutively with no other intervening chunks.
      -- The compressed datastream is then the concatenation of the contents of all the IDAT chunks.
      -- The encoder can divide the compressed datastream into IDAT chunks however it wishes.
      -- (Multiple IDAT chunks are allowed so that encoders can work in a fixed amount of memory;
      -- typically the chunk size will correspond to the encoder's buffer size.)
      -- It is important to emphasize that IDAT chunk boundaries have no semantic significance and can occur at any point in the compressed datastream.
      -- A PNG file in which each IDAT chunk contains only one data byte is valid, though remarkably wasteful of space.
      -- (For that matter, zero-length IDAT chunks are valid, though even more wasteful.)

      Chunk_Unkown_List : PNG_Chunk_Vector;
   end record;
   -- All implementations must understand and successfully render the standard critical chunks.
   -- A valid PNG image must contain an IHDR chunk, one or more IDAT chunks, and an IEND chunk.

   function Create_Chunk_Kind (Item : PNG_Chunk_Kind_String) return PNG_Chunk_Kind;
   -- PNG chunk kind names consist of string characters.
   -- This function converts 4 string characters to 4 bytes.

   procedure Read (Streamer : Stream_Access; Item : in out PNG_Information);

   procedure Read (Streamer : Stream_Access; Item : out Unsigned_32);
   -- Read a Unsigned_32 and convert it to network byte order.

   procedure Read_Chunk_Begin (Streamer : Stream_Access; Length : out Unsigned_32; Kind : out PNG_Chunk_Kind; Calculated_Checksum : out GNAT.CRC32.CRC32);
   -- Read chunk length and chunk kind and begin crc32 calculation.
   -- All integers read that require more than one byte are converted to network byte order.
   -- This procedure is used to clarify that the beginning of the chunk is being read.

   procedure Read_Chunk_End (Streamer : Stream_Access; Calculated_Checksum : GNAT.CRC32.CRC32);
   -- Read checksum and assert calculated checksum with checksum.
   -- This procedure is used to clarify that the end of the chunk is being read.

   procedure Read_Signature (Streamer : Stream_Access);
   -- Signature indicates that the remainder of the file contains a single PNG image.
   -- http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html#PNG-file-signature
   -- Exception is raised if signature does not match.

   procedure Read_First_Chunk (Streamer : Stream_Access; Item : in out PNG_Chunk_Data_IHDR);
   -- IHDR must appear first. This must be read directly after Read_Signature.

   procedure Read_Chunk (Streamer : Stream_Access; Chunk : in out PNG_Chunk);
   -- Read arbitrary chunk.
   -- New space is allocated then read for the chunk data.

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

   for PNG_Interlace'Size use 8;
   for PNG_Interlace use
     (
      PNG_Interlace_None => 0,
      PNG_Interlace_Adam7 => 1
     );

   for PNG_Filter'Size use 8;
   for PNG_Filter use
     (
      PNG_Filter_Method_0 => 0
     );

   for PNG_Compression'Size use 8;
   for PNG_Compression use
     (
      PNG_Compression_Method_0 => 0
     );


end;
