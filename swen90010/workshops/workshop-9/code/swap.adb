package body Swap with
     SPARK_Mode => On
is

   procedure SwapProcedure(X, Y : in out Integer) is
      T : Integer;
   begin
      T := X;
      X := Y;
      Y := T;
   end SwapProcedure;

end Swap;
