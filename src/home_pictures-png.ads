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


   type PNG_Information;


   type PNG_Data_IHDR;
   -- * https://www.w3.org/TR/PNG/#11IHDR


   type PNG_Data_sRGB;
   -- https://www.w3.org/TR/PNG/#11sRGB
   -- If the sRGB chunk is present, the image samples conform to the sRGB colour space [IEC 61966-2-1]
   -- and should be displayed using the specified rendering intent defined by the International Color Consortium [ICC-1] and [ICC-1A].


   type PNG_Chunk;
   -- This is a arbitrary chunk.


   type PNG_Small_Chunk;
   -- This type does not contain any chunk data.
   -- It can be used find out which types of chunks the PNG datastream has.
   -- Length for each chunk is stored also.


   subtype PNG_Chunk_Kind_String is String (1 .. 4);
   -- PNG chunk kind names consist of string characters.
   -- ========================================
   -- # https://www.w3.org/TR/PNG/#5Chunk-layout
   -- There are 18 chunk types defined in this International Standard.
   -- Chunk types are four-byte sequences chosen so that they correspond to readable labels when interpreted in the ISO 646.IRV:1991 character set.
   -- A 4-byte chunk type code. For convenience in description and in examining PNG files,
   -- type codes are restricted to consist of uppercase and lowercase ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
   -- However, encoders and decoders must treat the codes as fixed binary values, not character strings.
   -- For example, it would not be correct to represent the type code IDAT by the EBCDIC equivalents of those letters.
   -- Additional naming conventions for chunk types are discussed in the next section.


   type PNG_Chunk_Kind is
     (
      PNG_Chunk_Kind_gAMA,
      PNG_Chunk_Kind_sRGB,
      PNG_Chunk_Kind_bKGD,
      PNG_Chunk_Kind_IEND,
      PNG_Chunk_Kind_tIME,
      PNG_Chunk_Kind_PLTE,
      PNG_Chunk_Kind_cHRM,
      PNG_Chunk_Kind_iCCP,
      PNG_Chunk_Kind_IHDR,
      PNG_Chunk_Kind_tRNS,
      PNG_Chunk_Kind_IDAT,
      PNG_Chunk_Kind_sBIT,
      PNG_Chunk_Kind_sPLT,
      PNG_Chunk_Kind_hIST,
      PNG_Chunk_Kind_pHYs,
      PNG_Chunk_Kind_tEXt,
      PNG_Chunk_Kind_iTXt,
      PNG_Chunk_Kind_zTXt
     );
   for PNG_Chunk_Kind use
     (
      PNG_Chunk_Kind_gAMA => 1095582055,
      PNG_Chunk_Kind_sRGB => 1111970419,
      PNG_Chunk_Kind_bKGD => 1145523042,
      PNG_Chunk_Kind_IEND => 1145980233,
      PNG_Chunk_Kind_tIME => 1162692980,
      PNG_Chunk_Kind_PLTE => 1163152464,
      PNG_Chunk_Kind_cHRM => 1297238115,
      PNG_Chunk_Kind_iCCP => 1346585449,
      PNG_Chunk_Kind_IHDR => 1380206665,
      PNG_Chunk_Kind_tRNS => 1397641844,
      PNG_Chunk_Kind_IDAT => 1413563465,
      PNG_Chunk_Kind_sBIT => 1414087283,
      PNG_Chunk_Kind_sPLT => 1414287475,
      PNG_Chunk_Kind_hIST => 1414744424,
      PNG_Chunk_Kind_pHYs => 1935231088,
      PNG_Chunk_Kind_tEXt => 1951942004,
      PNG_Chunk_Kind_iTXt => 1951945833,
      PNG_Chunk_Kind_zTXt => 1951945850
     );
   for PNG_Chunk_Kind'Size use 32;
   -- This designate the type of chunk.
   -- A PNG datastream consist of different types of chunks.
   --
   -- Sidenote:
   -- Enumeration need to be sorted and values must be static values therefore this is generated and sorted by a precompiler.
   -- Unsigned 32 instead of array of bytes is used for perfomence reason, libpng is doing the same.
   -- Enum values are generated by little endian byte order machine.
   -- This does not work on a big endian byte order machine.
   -- This is is pre calculated from 4 character string to unsigned 32.


   type PNG_Color_Kind is
     (
      PNG_Color_Kind_Greyscale,
      PNG_Color_Kind_Truecolour,
      PNG_Color_Kind_Indexed_Colour,
      PNG_Color_Kind_Greyscale_With_Alpha,
      PNG_Color_Kind_Truecolour_With_Alpha
     );
   for PNG_Color_Kind'Size use 8;
   for PNG_Color_Kind use
     (
      PNG_Color_Kind_Greyscale             => 0,
      PNG_Color_Kind_Truecolour            => 2,
      PNG_Color_Kind_Indexed_Colour        => 3,
      PNG_Color_Kind_Greyscale_With_Alpha  => 4,
      PNG_Color_Kind_Truecolour_With_Alpha => 6
     );
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
   for PNG_Bit_Depth'Size use 8;
   for PNG_Bit_Depth use
     (
      PNG_Bit_Depth_1  => 1,
      PNG_Bit_Depth_2  => 2,
      PNG_Bit_Depth_4  => 4,
      PNG_Bit_Depth_8  => 8,
      PNG_Bit_Depth_16 => 16
     );
   -- Bit depth is a single-byte integer giving the number of bits
   -- per sample or per palette index (not per pixel).
   -- Valid values are 1, 2, 4, 8, and 16, although not all values are allowed for all color types.
   -- Color Type                    | Allowed Bit Depths  | Interpretation
   -- PNG_Greyscale             = 0 | 1, 2, 4, 8, 16      | Each pixel is a grayscale sample.
   -- PNG_Truecolour            = 2 |          8, 16      | Each pixel is an R,G,B triple.
   -- PNG_Indexed_Colour        = 3 | 1, 2, 4, 8          | Each pixel is a palette index; a PLTE chunk must appear.
   -- PNG_Greyscale_With_Alpha  = 4 | 8,          16      | Each pixel is a grayscale sample, followed by an alpha sample.
   -- PNG_Truecolour_With_Alpha = 6 | 8,          16      | Each pixel is an R,G,B triple,followed by an alpha sample.


   subtype PNG_Pixel_Count is Unsigned_32;
   subtype PNG_Width is PNG_Pixel_Count;
   subtype PNG_Height is PNG_Pixel_Count;
   -- Width and height give the image dimensions in pixels.
   -- They are 4-byte integers. Zero is an invalid value.


   subtype PNG_Chunk_Size_Byte is Unsigned_32;
   -- A 4-byte unsigned integer giving the number of bytes in the chunk's data field.
   -- The length counts only the data field, not itself, the chunk type code, or the CRC.
   -- Zero is a valid length.
   -- Although encoders and decoders should treat the length as unsigned, its value must not exceed 231-1 bytes.


   type PNG_Interlace is (PNG_Interlace_None, PNG_Interlace_Adam7);
   for PNG_Interlace'Size use 8;
   for PNG_Interlace use
     (
      PNG_Interlace_None => 0,
      PNG_Interlace_Adam7 => 1
     );
   -- Interlace method is a single-byte integer that indicates the transmission order of the image data.
   -- Two values are currently defined: 0 (no interlace) or 1 (Adam7 interlace). See Interlaced data order for details.


   type PNG_Filter is (PNG_Filter_Method_0);
   for PNG_Filter'Size use 8;
   for PNG_Filter use
     (
      PNG_Filter_Method_0 => 0
     );
   -- Filter method is a single-byte integer that indicates the preprocessing method applied to the image data before compression.
   -- At present, only filter method 0 (adaptive filtering with five basic filter types) is defined.
   -- As with the compression method field, decoders must check this byte and report an error if it holds an unrecognized code.


   type PNG_Compression is (PNG_Compression_Method_0);
   for PNG_Compression'Size use 8;
   for PNG_Compression use
     (
      PNG_Compression_Method_0 => 0
     );
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


   subtype PNG_Checksum is Unsigned_32;
   -- A 4-byte CRC (Cyclic Redundancy Check) calculated on the preceding bytes in the chunk,
   -- including the chunk type code and chunk data fields, but not including the length field.
   -- The CRC is always present, even for chunks containing no data.
   -- Chunk CRCs are calculated using standard CRC methods with pre and post conditioning,
   -- as defined by ISO 3309 [ISO-3309] or ITU-T V.42 [ITU-T-V42].
   -- The CRC polynomial employed is x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1


   type PNG_Filter_Type is
     (
      PNG_Filter_Type_None, --Filt(x) = Orig(x)
      PNG_Filter_Type_Sub,  --Filt(x) = Orig(x) - Orig(a)
      PNG_Filter_Type_Up,   --Filt(x) = Orig(x) - Orig(b)
      PNG_Filter_Type_Average, --Filt(x) = Orig(x) - floor((Orig(a) + Orig(b)) / 2)
      PNG_Filter_Type_Paeth --Filt(x) = Orig(x) - PaethPredictor(Orig(a), Orig(b), Orig(c))
     );
   for PNG_Filter_Type'Size use 8;
   for PNG_Filter_Type use
     (
      PNG_Filter_Type_None => 0,
      PNG_Filter_Type_Sub => 1,
      PNG_Filter_Type_Up => 2,
      PNG_Filter_Type_Average => 3,
      PNG_Filter_Type_Paeth => 4
     );


   subtype PNG_Pixel_Per_Unit is Unsigned_32;
   subtype PNG_Unit_Specifier is Unsigned_8;
   subtype PNG_Image_Gamma is Unsigned_32;

   type PNG_Rendering_Intent is
     (
      PNG_Rendering_Intent_Perceptual,
      PNG_Rendering_Intent_Relative_Colorimetric,
      PNG_Rendering_Intent_Saturation,
      PNG_Rendering_Intent_Absolute_Colorimetric
     );
   for PNG_Rendering_Intent'Size use 8;
   for PNG_Rendering_Intent use
     (
      PNG_Rendering_Intent_Perceptual            => 0,
      PNG_Rendering_Intent_Relative_Colorimetric => 1,
      PNG_Rendering_Intent_Saturation            => 2,
      PNG_Rendering_Intent_Absolute_Colorimetric => 3
     );


   subtype PNG_Channel8 is Unsigned_8;
   type PNG_Pixel_RGBA8 is array (0 .. 3) of PNG_Channel8;
   type PNG_Pixel_RGBA8_Row is array (Integer range <>) of PNG_Channel8;


   type PNG_Chunk is record
      Length : PNG_Chunk_Size_Byte;
      Kind : PNG_Chunk_Kind;
      Data : access Stream_Element_Array;
      -- The data bytes appropriate to the chunk type, if any. This field can be of zero length.

      Checksum : PNG_Checksum;
   end record with Pack;


   type PNG_Small_Chunk is record
      Length : PNG_Chunk_Size_Byte;
      Kind : PNG_Chunk_Kind;
   end record;


   type PNG_Data_IHDR is record
      Width             : PNG_Width;
      Height            : PNG_Height;
      Bit_Depth         : PNG_Bit_Depth;
      Color_Kind        : PNG_Color_Kind;
      Compression       : PNG_Compression;
      Filter            : PNG_Filter;
      Interlace         : PNG_Interlace;
   end record;


   type PNG_Data_sRGB is record
      Rendering_Intent : PNG_Rendering_Intent;
   end record;


   type PNG_Data_pHYs is record
      Pixel_Per_Unit_X : PNG_Pixel_Per_Unit;
      Pixel_Per_Unit_Y : PNG_Pixel_Per_Unit;
      Unit_Specifier : PNG_Unit_Specifier;
   end record;


   type PNG_Data_gAMA is record
      Image_Gamma : PNG_Image_Gamma;
   end record;


   package PNG_Small_Chunk_Vectors is new Ada.Containers.Vectors (Positive, PNG_Small_Chunk);
   subtype PNG_Small_Chunk_Vector is PNG_Small_Chunk_Vectors.Vector;


   package PNG_Chunk_Vectors is new Ada.Containers.Vectors (Positive, PNG_Chunk);
   subtype PNG_Chunk_Vector is PNG_Chunk_Vectors.Vector;


   type PNG_Information is record
      Chunk_Count : Natural := 0;
      -- TODO: Is chunk count necessary?

      Channel_Count : Natural;
      Pixel_Depth : Natural;
      Row_Size_Byte : Natural;

      Data_IHDR : PNG_Data_IHDR;
      -- IHDR must appear first and IEND must appear last;
      -- thus the IEND chunk serves as an end-of-file marker

      Data_sRGB : PNG_Data_sRGB;
      -- IHDR must appear first and IEND must appear last;
      -- thus the IEND chunk serves as an end-of-file marker

      Data_IDAT_List : PNG_Chunk_Vector;
      -- TODO: This is experimental.
      -- How to store IDAT?
      -- How to interact with IDAT storage.
      -- (Read everthing then uncompress) or (read little then uncompress then repeat).
      -- ===
      -- There can be multiple IDAT chunks; if so, they must appear consecutively with no other intervening chunks.
      -- The compressed datastream is then the concatenation of the contents of all the IDAT chunks.
      -- The encoder can divide the compressed datastream into IDAT chunks however it wishes.
      -- (Multiple IDAT chunks are allowed so that encoders can work in a fixed amount of memory;
      -- typically the chunk size will correspond to the encoder's buffer size.)
      -- It is important to emphasize that IDAT chunk boundaries have no semantic significance and can occur at any point in the compressed datastream.
      -- A PNG file in which each IDAT chunk contains only one data byte is valid, though remarkably wasteful of space.
      -- (For that matter, zero-length IDAT chunks are valid, though even more wasteful.)

      Data_Unkown_List : PNG_Chunk_Vector;
      -- TODO: This is experimental.
   end record;
   -- All implementations must understand and successfully render the standard critical chunks.
   -- A valid PNG image must contain an IHDR chunk, one or more IDAT chunks, and an IEND chunk.





   function Create_Chunk_Kind_32 (Item : PNG_Chunk_Kind_String) return Unsigned_32;


   procedure Read (Streamer : Stream_Access; Item : in out PNG_Information);
   -- Raises exception on PNG datastream corruption.


   procedure Read_Chunk_Kind (Streamer : Stream_Access; Kind : out PNG_Chunk_Kind; Calculated_Checksum : in out GNAT.CRC32.CRC32);
   -- Read chunk kind and update checksum.


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
   -- Raises exception if signature does not match PNG signature.
   -- ================================================
   -- # https://www.w3.org/TR/PNG/#5PNG-file-signature
   -- This signature indicates that the remainder of the datastream contains a single PNG image,
   -- consisting of a series of chunks beginning with an IHDR chunk and ending with an IEND chunk.


   procedure Read_First_Chunk (Streamer : Stream_Access; Item : in out PNG_Data_IHDR);
   -- IHDR must appear first. This must be read directly after Read_Signature.
   -- Raises exception on PNG datastream corruption.


   procedure Read_Chunk (Streamer : Stream_Access; Chunk : in out PNG_Chunk);
   -- Read arbitrary chunk.
   -- New space is allocated for the chunk data.
   -- Raises exception on PNG datastream corruption.


   procedure Read_Chunk (Streamer : Stream_Access; Item : out PNG_Small_Chunk);
   -- No space for data is allocated.
   -- Raises exception on PNG datastream corruption.


   procedure Read_Complete (File_Name : String; Item : out PNG_Small_Chunk_Vector);
   -- Read PNG datastream completly
   -- No space is allocated.
   -- Raises exception on PNG datastream corruption.


   procedure Read_Complete (Streamer : Stream_Access; Item : out PNG_Small_Chunk_Vector);
   -- Read PNG datastream completly.
   -- No space is allocated.
   -- Raises exception on PNG datastream corruption.


   function Find_Channel_Count (Item : PNG_Color_Kind) return Natural;


   procedure Swap_Byte_Order (Item : in out PNG_Data_IHDR);


private


   Chunk_Kind_List : constant array (PNG_Chunk_Kind) of PNG_Chunk_Kind_String :=
     (
      PNG_Chunk_Kind_IHDR => "IHDR",
      PNG_Chunk_Kind_PLTE => "PLTE",
      PNG_Chunk_Kind_IDAT => "IDAT",
      PNG_Chunk_Kind_IEND => "IEND",
      PNG_Chunk_Kind_cHRM => "cHRM",
      PNG_Chunk_Kind_gAMA => "gAMA",
      PNG_Chunk_Kind_iCCP => "iCCP",
      PNG_Chunk_Kind_sBIT => "sBIT",
      PNG_Chunk_Kind_sRGB => "sRGB",
      PNG_Chunk_Kind_bKGD => "bKGD",
      PNG_Chunk_Kind_hIST => "hIST",
      PNG_Chunk_Kind_tRNS => "tRNS",
      PNG_Chunk_Kind_pHYs => "pHYs",
      PNG_Chunk_Kind_sPLT => "sPLT",
      PNG_Chunk_Kind_tIME => "tIME",
      PNG_Chunk_Kind_iTXt => "iTXt",
      PNG_Chunk_Kind_tEXt => "tEXt",
      PNG_Chunk_Kind_zTXt => "zTXt"
     );

end;
