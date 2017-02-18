with Interfaces.C;
with Interfaces;
with System.Storage_Elements;
with System;

package Home_Pictures.Swaps is

   use Interfaces;
   use Interfaces.C;
   use System.Storage_Elements;
   use System;

   function Bswap_32 (X : Unsigned_32) return Unsigned_32;
   pragma Import (Intrinsic, Bswap_32, "__builtin_bswap32");

   function Swap (Item : Unsigned_32) return Unsigned_32;


   --function htonl (hostlong  : Unsigned_32) return Unsigned_32;
   --pragma Import (C, htonl, "htonl");

end;
