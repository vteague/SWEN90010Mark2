package body ConditionalSwap with
     SPARK_Mode => On
is

   procedure SwapProcedure(X, Y : in out Integer) is
      T : Integer;
   begin
      if X < Y then
         T := X;
         X := Y;
         Y := T;
      end if;
   end SwapProcedure;

end ConditionalSwap;
