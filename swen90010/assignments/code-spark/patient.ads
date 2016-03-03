with Measures;

--# inherit Measures,
--#         RandomFloat;
package Patient is

   -- A type representing a single patient
   type PatientType is
      record
	 -- The blood pressue for the patient: a top and bottom
	 --  reading
	 SystolicPressure : Measures.MmHg;
	 DiastolicPressure : Measures.MmHg;
	 
	 -- The current dosage, to be administered in the next tick
	 CurrentDosage : Measures.Dosage;
	 
	 -- The approximate dosage the allows this patient to remain
	 --  stable
	 StableDosage : Measures.Dosage;
      end record;
   
   -- Initialise the patient, setting a blood pressure from a normal
   -- probability distribution.
   procedure Init(P : out PatientType);
   --# derives P from ;
   --# post P.CurrentDosage = 0.0 and 
   --#      Measures.Dosage'First <= P.StableDosage and P.StableDosage <= Measures.Dosage'Last;
   
   -- Set the dosage amount, to be administered at the next clock tick.
   -- This should only be called by the Pump.
   procedure AdministerDosage(P : in out PatientType; D : in Measures.Dosage);
   --# derives P from D, P;
   --# post P.CurrentDosage = D;
   
   function GetDosage(P : in PatientType) return Measures.Dosage;
   --# return P.CurrentDosage;
   
   -- Access the patient's real blood pressure.
   -- This should only be called by the BPM.
   procedure GetPressure(P : in PatientType;
			 Systolic : out Measures.MmHg; 
			 Diastolic : out Measures.MmHg);
   --# derives Systolic, Diastolic from P;
   --# post Systolic = P.SystolicPressure and Diastolic = P.DiastolicPressure;
   
   -- Tick the clock, providing a dosage to the patient.
   procedure Tick(P : in out PatientType);
   --# derives P from P;
   --# pre P.SystolicPressure > 0.0 and P.DiastolicPressure > 0.0;

 end Patient;

