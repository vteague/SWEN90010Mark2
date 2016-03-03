package ArithSolution with
     SPARK_Mode => On
is
   
   procedure Divide(M, N: in Integer; Q, R: out Integer)
     with Pre => (M >= 0) and (N > 0),
          Post => (M = Q * N + R) and (R < N) and (R >= 0);

   procedure UseDivide(M, N : in Integer; Q, R : out Integer)
     with Pre => (N > 0 and M >= 0),
          Post => M = Q * N + R;

end ArithSolution;

