# Home_Pictures

## TODO
* [X] Experimental BMP Headers.
* [X] Experimental PNG Headers.
* [X] Experimental chunk reader.
      How to store IDAT?
	  How to interact with IDAT storage.
	  (Read everthing then uncompress) or (read little then uncompress then repeat).
* [X] Have `PNG_Chunk_Kind` as a unsigned 32 discrete type instead of an array of 4 bytes.
      This allow the use of enumeration instead of local constants array of 4 bytes.
	  Might be faster to compare one unsigned 32 instead of array of 4 bytes.
* [ ] Inflate uncompress PNG IDAT chunks. http://unzip-ada.sourceforge.net/
* [ ] Deflate compress PNG IDAT chunks. http://unzip-ada.sourceforge.net/
* [ ] Common picture type that can be used for decoding any supported picture.
* [ ] Implement ability to detect courruption in a PNG stream.
      The PNG decoding checks if PNG stream is corrupted by using assertion.
	  To avoid exception handling check if PNG is non corrupted before decoding.
* [ ] Turning of CRC32 checks when assertion is turned off.
      There is no reason to calculate CRC32 if it is not evaluated.