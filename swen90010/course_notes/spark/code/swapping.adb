package body Swapping with
     SPARK_Mode => On
is

   procedure Swap (X, Y : in out Float) is
      T : Float;
   begin
      T := X;
      X := Y;
      Y := T;
   end Swap;
end Swapping;
