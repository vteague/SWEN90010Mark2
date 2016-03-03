package body Task4Solution with
     SPARK_Mode => On
is

   procedure Task4Procedure(AnArray : in out MyArray; AnIndex : in Index) is
   begin
      if AnArray(AnIndex) < Integer'Last then
	 AnArray(AnIndex) := AnArray(AnIndex + 1);
      end if;
   end Task4Procedure;
end Task4Solution;

