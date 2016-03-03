with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;

package body Stats is

   -- 95% confidence interval (95% of the area of a normal
   -- distribution curve lie with 1.96 standard deviations of the
   -- mean)
   CONFINT : constant Float := 1.96;

   procedure AddData (Data : in out Sample; Point : Float) is
   begin
      Data.N       := Data.N + 1;
      Data.Mean    := Data.Mean + Point;
      Data.Squares := Data.Squares + Point ** 2;
   end AddData;

   function Mean (Data: in Sample) return Float is
   begin
      return Data.Mean / Float(Data.N);
   end Mean;

   function Deviation (Data : in Sample) return Float is
   begin
      return Sqrt ((Data.Squares / Float (Data.N)) - (Data.Mean ** 2));
   end Deviation;

   -- Return the 95% confidence interval
   procedure CI (Data : in Sample; Low : out Float; High : out Float) is
      SMean : Float;
      SDeviation : Float;
   begin
      SMean := Mean(Data);
      SDeviation := Deviation(Data);
      Low := SMean - CONFINT * (SDeviation / Sqrt(Float(Data.N)));
      High := SMean + CONFINT * (SDeviation / Sqrt(Float(Data.N)));
   end CI;

end Stats;
