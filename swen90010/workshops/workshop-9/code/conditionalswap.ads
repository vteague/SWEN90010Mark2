package ConditionalSwap with
     SPARK_Mode => On
is

   procedure SwapProcedure(X, Y : in out Integer)
     with Post => (X >= Y);

end ConditionalSwap;

