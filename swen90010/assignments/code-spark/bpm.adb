with Patient;
with RandomFloat; 

package body BPM is
   
   Error : constant Float := 0.02;  -- The error margin of the pump readings
   
   procedure Init(B : out BPMType) is
   begin
      B.IsOn := False;
      B.SystolicPressure := Measures.MmHg'First;
      B.DiastolicPressure := Measures.MmHg'First;
   end Init;

   procedure On(B : out BPMType; Ptnt : in Patient.PatientType) is
   begin
     -- Get an initial reading for the patient
      B.IsOn := True;
      Patient.GetPressure(Ptnt, B.SystolicPressure, B.DiastolicPressure);
   end On;
      
   procedure Off(B : in out BPMType) is
   begin
      B.IsOn := False;
   end Off;
   
   function IsOn(B : in BPMType) return Boolean is
   begin
      return B.IsOn;
   end IsOn;
   
   procedure GetPressure(B : in BPMType;
			 Systolic : out Measures.MmHg; 
			 Diastolic : out Measures.MmHg) is
   begin
      if B.IsOn then
	 Systolic := B.SystolicPressure;
	 Diastolic := B.DiastolicPressure;
      else
	 Systolic := Measures.MmHg'First;
	 Diastolic := Measures.MmHg'First;
      end if;
   end GetPressure;
   
   procedure Tick(B : in out BPMType; Ptnt : in Patient.PatientType) is
   begin
      if B.IsOn then
	 -- read the blood pressure from the patient
	 Patient.GetPressure(Ptnt, 
			     B.SystolicPressure, 
			     B.DiastolicPressure);
	 
	 -- Insert some random variation
	 B.SystolicPressure := 
	   Measures.LimitMmHg(RandomFloat.UniformError(B.SystolicPressure, Error));
	 B.DiastolicPressure := 
	   Measures.LimitMmHg(RandomFloat.UniformError(B.DiastolicPressure, Error));
      else
	 -- If the monitor is not on, return -1.0 for both values
	 B.SystolicPressure := Measures.MmHg'First;
	 B.DiastolicPressure := Measures.MmHg'First;
      end if; 
      
   end Tick;
end BPM;
