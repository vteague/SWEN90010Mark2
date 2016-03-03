package Measures is
   -- A type for blood pressure: millimetres of mercury.
   -- The value -1.0 is used to represent a null reading.
   subtype MmHg is Float range -1.0 .. 300.0;
   
   -- The type for a dosage amount.
   subtype Dosage is Float range 0.0 .. 6.0;
   
   -- A function to limit floats
   function Limit(Input : in Float; Fst : in Float; Lst : in Float) return Float;
   --# pre Fst <= Lst;
   --# return Output => (Fst <= Output and Output <= Lst);
   
   -- A function to limit MmHg measures
   function LimitMmHg(Input : in Float) return MmHg;
   --# return Output => (MmHg'First <= Output and Output <= MmHg'Last);
   
   -- A function to limit Dosage measures
   function LimitDosage(Input : in Float) return Dosage;
   --# return Output => (Dosage'First <= Output and Output <= MmHg'Last);
      
end Measures;
