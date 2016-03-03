package Stats is

   type Sample is record
      N       : Natural := 0;
      Mean    : Float := 0.0;
      Squares : Float := 0.0;
   end record;

   -- Add an element to the sample
   procedure AddData (Data : in out Sample; Point : Float);

   -- Return the mean of the sample
   function Mean ( Data : in Sample ) return Float;

   -- Return the standard deviation of the sample
   function Deviation (Data : in Sample) return Float;

   -- Return the 95% confidence interval
   procedure CI (Data : in Sample; Low : out Float; High : out Float);

end Stats;
