package body Home_Pictures.Swaps is



   function Swap (Item : Unsigned_32) return Unsigned_32 is
      R : Unsigned_32;
   begin
      R :=
        Shift_Right (Item, 24) or
        Shift_Right (Item and 16#00FF0000#, 8) or
        Shift_Left (Item and 16#0000FF00#, 8) or
        Shift_Left (Item, 24);
      return R;
   end;


end;
