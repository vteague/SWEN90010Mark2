package List is
   
   subtype Index is Integer range 1 .. 10;
   type FloatArray is array(Index) of Float;
   
   --# function Summation(N : Natural; A : FloatArray) return Float;
   
   procedure ListAverage(AList : in out FloatArray; Result : out Float);
   --# derives Result from AList & AList from AList;
   --# post Result = Summation(AList~'Length, AList) / Float(AList~'Length) and AList(1) = Result;
end List;
