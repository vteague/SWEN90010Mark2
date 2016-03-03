with Measures;
with RandomNumber;

-- This package defines a very simple and crude simulation of a heart.
--  The default behaviour is for the heart to speed up, unless it
--  receives a small shock, in which case it will slow down, or a
--  large shock, in which case it will cease working
package Heart
  with SPARK_Mode
is
   -- A type representing a heart
   type HeartType is
      record
	 -- The heart rate for the patient
	 Rate : Measures.BPM;
	 
	 -- The impulse to be administered in the next tick
	 Impulse : Measures.Joules;
	 
	 -- The amount to add to the heart rate to simulate rising rate
	 DefaultChange : Measures.BPM;
      end record;
   
   -- Initialise the heart, setting a heart rate from a normal
   -- probability distribution.
   procedure Init(Hrt : out HeartType) 
     with Post => (Hrt.Impulse = 0 and 
                   Measures.BPM'First <= Hrt.Rate and 
                   Hrt.Rate <= Measures.BPM'Last and
                   Hrt.DefaultChange = 1);
   
   -- Set the amount of joules to be administered at the next clock tick.
   -- This should only be called by the impulse generator.
   procedure SetImpulse(Hrt : in out HeartType; Joules : in Measures.Joules)
     with Post => (Hrt.Impulse = Joules);
   
   -- Read the amount of joules to be administered at the next clock tick.
   function GetImpulse(Hrt : in HeartType) return Measures.Joules
     with Post => (GetImpulse'Result = Hrt.Impulse);
   
   -- Access the heart's real BPM.
   -- This should only be called by the HRM.
   procedure GetRate(Hrt : in HeartType;
                     Rate : out Measures.BPM) 
     with Post => (Rate = Hrt.Rate);
   
   -- Tick the clock, providing an impulse to the heart.
   procedure Tick(Hrt : in out HeartType);
   
end Heart;

