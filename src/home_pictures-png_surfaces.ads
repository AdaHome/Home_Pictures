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


   type PNG_Signature is array (0 .. 7) of Unsigned_8 with Pack;


   -- A 4-byte chunk type code. For convenience in description and in examining PNG files,
   -- type codes are restricted to consist of uppercase and lowercase ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
   -- However, encoders and decoders must treat the codes as fixed binary values, not character strings.
   -- For example, it would not be correct to represent the type code IDAT by the EBCDIC equivalents of those letters.
   -- Additional naming conventions for chunk types are discussed in the next section.
   subtype PNG_Chunk_Kind is Stream_Element_Array (0 .. 3);

   type PNG_Color_Kind is (PNG_Greyscale, PNG_Truecolour, PNG_Indexed_Colour, PNG_Greyscale_With_Alpha, PNG_Truecolour_With_Alpha);
   type PNG_Bit_Depth is (PNG_Bit_Depth_1, PNG_Bit_Depth_2, PNG_Bit_Depth_4, PNG_Bit_Depth_8, PNG_Bit_Depth_16);


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


   type PNG_Chunk_IHDR is record
      Width             : Unsigned_32;
      Height            : Unsigned_32;
      Bit_Depth         : PNG_Bit_Depth;
      Color_Kind        : PNG_Color_Kind;
      Compression       : Unsigned_8;
      Filter            : Unsigned_8;
      Interlace         : Unsigned_8;
   end record;


   type PNG_Surface is record
      Chunk_IHDR : PNG_Chunk_IHDR;
      Chunk_List : PNG_Chunk_Vector;
   end record;

--     procedure Read_First (Streamer : Stream_Access; Item : in out PNG_Chunk_IHDR);
--     procedure Read_Chunk (Streamer : Stream_Access; Chunk : in out PNG_Chunk);
--     procedure Read_Signature (Streamer : Stream_Access);
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
