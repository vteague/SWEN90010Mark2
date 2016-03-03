package body Factorial with
     SPARK_Mode => On
is


   procedure Factorial(N : in Natural; F : out Natural)
   is
      I : Natural;
   begin
      F := 1;
      I := 0;
      while I /= N loop
         pragma Loop_Invariant( Fact(I) = F and I + 1 > 0  );
         I := I + 1;
         F := F * I;
      end loop;
   end Factorial;
end Factorial;
