package Factorial with
     SPARK_Mode => On
is


   function Fact(N : in Natural) return Natural
     is
     (if N <= 0 then 1 else N*Fact(N-1));

   procedure Factorial(N : in Natural; F : out Natural)
	with Pre => (N >= 0 and Fact(N) <= Integer'Last),
   	 Post => (Fact(N) = F);

 end Factorial;
