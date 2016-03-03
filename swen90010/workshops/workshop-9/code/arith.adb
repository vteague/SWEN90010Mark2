package body Arith with
     SPARK_Mode => On
is
   
   procedure Divide(M, N: in Integer; Q, R: out Integer) is
   begin
      Q := 0;
      R := M;
      loop
         pragma Loop_Invariant ((M = Q * N + R) and (R >= 0));
         exit when R < N;
         Q := Q + 1;
         R := R - N;
      end loop;
   end Divide;

   --procedure UseDivide(M, N : in Natural; Q, R : out Natural) is
   --begin
   --   Divide(M, N, Q, R);
   --end UseDivide;

end Arith;
