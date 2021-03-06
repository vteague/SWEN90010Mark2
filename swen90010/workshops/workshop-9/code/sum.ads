package Sum
  with SPARK_Mode
is

   subtype ElementType is Natural range 0 .. 1000;
   subtype IndexType is Positive range 1 .. 100;
   type ArrayType is array (IndexType) of ElementType;
   subtype SumType is Natural range 0 .. 100000;

   function SumArray (A : in ArrayType) return SumType;

end Sum;
