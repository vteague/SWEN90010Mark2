package Swap with
     SPARK_Mode => On
is

   procedure SwapProcedure(X, Y : in out Integer)
     with Post => (X = Y'Old and Y = X'Old);

end Swap;

