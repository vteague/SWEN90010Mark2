with Measures; 
with RandomFloat;

package body Patient is
     
   -- Parameters for generating random blood pressure upon initialisation
   SystolicPressureMu : constant Measures.MmHg := 80.0;
   SystolicPressureSigma : constant Measures.MmHg := 10.0;
   
   -- Parameters for generating random stable dosage upon initialisation
   StableDosageMu : constant Measures.Dosage := 2.0;
   StableDosageSigma : constant Measures.Dosage := 0.5;
   
   -- The default fall (as a percentage) if no dosage is provided
   DefaultFall : constant Float := 0.002;
   
   -- The ratio of systolic to diastolic, used for simplicity
   PressureRatio : constant Float := 0.75;
   
   -- Used to simulate volatility of the patient's response to treatment
   Volatility : constant Float := 0.002;
   
   -- A local function to limit Dosage measures
   procedure Init(P : out PatientType) is
   begin
      -- Generate a random systolic pressure
      P.SystolicPressure := 
	Measures.LimitMmHG(RandomFloat.
			     NormalDistribution(SystolicPressureMu,
						SystolicPressureSigma));
      
      -- For simplicity, just generate a diastolic pressure that is
      --  'PressureRatio' mmHg less than the diastolic pressure
      P.DiastolicPressure := 
	Measures.LimitMmHg(P.SystolicPressure * PressureRatio);
      
      -- Generate a random stable dosage
      P.StableDosage := 
	Measures.LimitDosage(RandomFloat.NormalDistribution(StableDosageMu,
							    StableDosageSigma));
      
      P.CurrentDosage := 0.0;
   end Init;
   
   procedure AdministerDosage(P : in out PatientType; D : in Measures.Dosage) is
   begin
      P.CurrentDosage := D;
   end AdministerDosage;
   
   procedure GetPressure(P : in PatientType;
			 Systolic : out Measures.MmHg; 
			 Diastolic : out Measures.MmHg) is
   begin
      Systolic := P.SystolicPressure;
      Diastolic := P.DiastolicPressure;
   end GetPressure;
   
   function GetDosage(P : in PatientType) return Measures.Dosage is
   begin
      return P.CurrentDosage;
   end GetDosage;
   
   procedure Tick(P : in out PatientType) is
      Change : Float;
   begin
      if (P.CurrentDosage <= 0.0) then
	 -- This patient is in hypotension, so the default is that the
	 --  blood pressure should slowly edge down.
	 P.SystolicPressure := 
	   Measures.LimitMmHg(P.SystolicPressure - (P.SystolicPressure * DefaultFall));
      else
	 -- This patient responds well to dosages > 'StableDosage' L/Hour
	 Change := (P.CurrentDosage - P.StableDosage) * 0.2;
	 P.SystolicPressure := Measures.LimitMmHg(P.SystolicPressure + Change);
      end if;
      
      -- The diastolic pressure is just a ratio of the systolic pressure
      P.DiastolicPressure := P.SystolicPressure * PressureRatio;
      
      -- Insert some random volatility
      P.SystolicPressure := 
	Measures.LimitMmHg(RandomFloat.UniformError(P.SystolicPressure,
						    Volatility));
      P.DiastolicPressure := 
	Measures.LimitMmHg(RandomFloat.UniformError(P.DiastolicPressure,
						    Volatility));
   end Tick;
   
end Patient;
