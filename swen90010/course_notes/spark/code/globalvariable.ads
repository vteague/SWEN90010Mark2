package GlobalVariable
  --# own T;
is

   procedure Swap (X, Y : in out Float);
   --# global T;
   --# derives X from Y & Y from X;

private
   T : Float;

end GlobalVariable;

