package Task4Solution with
     SPARK_Mode => On
is

   subtype Index is Integer range 1 .. 10;
   type MyArray is array(Index) of Integer;

   -- Returns the element of AnArray at the specified index, AnIndex.
   procedure Task4Procedure(AnArray : in out MyArray; AnIndex : in Index)
     with Pre => AnArray(AnIndex) < Integer'Last,
     Post => AnArray(AnIndex) = AnArray'Old(AnIndex) + 1;

end Task4Solution;

