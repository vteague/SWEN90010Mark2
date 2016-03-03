package body InitialiseLocal  with
     SPARK_Mode => On
is

   procedure Swap (X, Y : in out Float) is
      T : Float := 0.0;
   begin
      T := X;
      X := Y;
      Y := T;
   end Swap;
end InitialiseLocal;
