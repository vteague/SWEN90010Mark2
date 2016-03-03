with Measures;
with Patient;

--# inherit Measures,
--#         RandomFloat,
--#         Patient;
package BPM is
   
   -- The blood pressure monitor type
   type BPMType is
      record
	 -- The measured blood pressue for the patient: a top and
	 --  bottom reading
	 SystolicPressure : Measures.MmHg;
	 DiastolicPressure : Measures.MmHg;
	 
	 IsOn : Boolean;  -- Indicates whether the BPM has been turned on.
      end record;
   
   -- Create and initialise a pump.
   procedure Init(B : out BPMType);
   --# derives B from ;
   --# post B.IsOn = False and B.SystolicPressure = Measures.MmHg'First and
   --#      B.DiastolicPressure = Measures.MmHg'First;
   
   -- Turn on the BPM and get a first reading from the patient.
   procedure On(B : out BPMType; Ptnt : in Patient.PatientType);
   --# derives B from Ptnt;
   --# post B.IsOn = True;
      
   -- Turn off the BPM;
   procedure Off(B : in out BPMType);
   --# derives B from B;
   --# post B.IsOn = False;
   
   -- Get the status of the BPM (on/off)
   function IsOn(B : in BPMType) return Boolean;
   --# return B.IsOn;
   
   -- Access the patient's *measured* blood pressure
   procedure GetPressure(B : in BPMType;
			 Systolic : out Measures.MmHg;
			 Diastolic : out Measures.MmHg);
   --# derives Systolic from B & Diastolic from B;
   --# post (B.IsOn -> 
   --#          (Systolic = B.SystolicPressure and Diastolic = B.DiastolicPressure))
   --#      and
   --#      (not B.IsOn -> 
   --#          (Systolic = Measures.MmHg'First and Diastolic =  Measures.MmHg'First));
   
   -- Tick the clock, reading the blood pressure from the patient.
   procedure Tick(B : in out BPMType; Ptnt : in Patient.PatientType);
   --# derives B from B, Ptnt;
   --# post not B.IsOn -> 
   --#      (B.SystolicPressure = Measures.MmHg'First and 
   --#       B.DiastolicPressure = Measures.MmHg'First);
end BPM;
