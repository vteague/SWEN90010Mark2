package body WeirdSwap  with
     SPARK_Mode => On
is

   procedure Swap (X, Y : in out Float) is
      T : Float;
   begin
      if X < Y then
	 T := X;
      end if;
      X := Y;
      Y := T;
   end Swap;
end WeirdSwap;
