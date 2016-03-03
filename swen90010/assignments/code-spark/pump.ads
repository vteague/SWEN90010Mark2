with Measures; 
with Patient;

--# inherit Measures,
--#         RandomFloat,
--#         Patient;
package Pump is
   
   -- The Pump type
   type PumpType is
      record
	 -- The current dosage; to be administered to the patient a
	 --  the next clock tick
	 CurrentDosage : Measures.Dosage;  
   
	 -- Indicates whether the pump has been turned on.
	 IsOn : Boolean;
      end record;
   
   -- Create and initialise a new pump.
   procedure Init(P : out PumpType);
   --# derives P from ;
   --# post P.IsOn = False and P.CurrentDosage = 0.0;
   
   -- Turn on the pump, but do not administer any dosages yet.
   procedure On(P : in out PumpType);
   --# derives P from P;
   --# post P.IsOn = True;
   
   -- Turn off the pump;
   procedure Off(P : in out PumpType);
   --# derives P from P;
   --# post P.IsOn = False;
   
   -- Query the status of the pump (on/off)
   function IsOn(P : in PumpType) return Boolean;
   --# return P.IsOn;
   
   -- Set the current dosage
   procedure SetDosage(P : in out PumpType; D : in Measures.Dosage);
   --# derives P from P, D;
   --# post (P.IsOn -> P.CurrentDosage = D) and
   --#      (not P.IsOn -> P = P~);
   
   -- Tick the clock, providing a dosage to the patient.
   procedure Tick(P : in PumpType; Ptnt : in out Patient.PatientType);
   --# derives Ptnt from P, Ptnt;
   --# post not P.IsOn -> Ptnt = Ptnt~;
   
end Pump;

