package body GlobalVariable is

   procedure Swap (X, Y : in out Float) is
   begin
      T := X;
      X := Y;
      Y := T;
   end Swap;
end GlobalVariable;
