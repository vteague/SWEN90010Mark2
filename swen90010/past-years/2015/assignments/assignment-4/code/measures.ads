-- This package provides some basic measures and limit functions for
--  the ICD case study.
package Measures 
  with SPARK_Mode
is
   
   MIN_JOULES : constant Integer := 0;
   MAX_JOULES : constant Integer := 45;
   
   -- The type for joules (unit of energy used in generator)
   subtype Joules is Integer range MIN_JOULES .. MAX_JOULES;
   
   MIN_BPM : constant Integer := -1;
   MAX_BPM : constant Integer := 300;
   
   -- The type for heart rate: beats per minute
   subtype BPM is Integer range MIN_BPM .. MAX_BPM;
   
   -- A function to limit floats
   function Limit(Input : in Integer; Fst : in Integer; Lst : in Integer) 
                  return Integer
     with Pre => (Fst <= Lst),
          Post => (Fst <= Limit'Result and Limit'Result <= Lst);
   
   -- A function to limit BPM measures
   function LimitBPM(Input : in Integer) return BPM
     with Post => (BPM'First <= LimitBPM'Result and LimitBPM'Result <= BPM'Last);
   
   -- A function to limit Joules measures
   function LimitJoules(Input : in Integer) return Joules
     with Post => (Joules'First <= LimitJoules'Result and LimitJoules'Result <= Joules'Last);
      
end Measures;
