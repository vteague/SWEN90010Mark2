with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Measures; use Measures;
with Patient;
with BPM;
with Pump;

procedure ManualOperationExample is
   Ptnt : Patient.Patient;   -- The simulated patient
   BlPrm : BPM.BPM;          -- The simulated blood pressure monitor
   Pmp : Pump.Pump;          -- The simulated pump
   SystolicPressure : MmHg;
   DiastolicPressure : MmHg;
begin
   -- Initialise the patient and turn the machines on
   Patient.Init(Ptnt);
   BPM.Init(BlPrm);
   Pump.Init(Pmp);
   
   BPM.On(BlPrm, Ptnt);
   Pump.On(Pmp);
   
   -- Loop 100 times with no dosage
   for I in Integer range 0..100 loop
      
      -- Set the dosage and read the blood pressure
      BPM.GetPressure(BlPrm, SystolicPressure, DiastolicPressure);
      
      -- Print the blood pressure
      Put("Blood pressure = ");
      Put(Item => SystolicPressure, Fore => 0, Aft => 0, Exp => 0);
      Put("/");
      Put(Item => DiastolicPressure, Fore => 0, Aft => 0, Exp => 0);
      New_Line;
      
      -- Set the new dosage
      Pump.SetDosage(Pmp, 0.0); 
      
      -- Tick all components
      Pump.Tick(Pmp, Ptnt);
      BPM.Tick(BlPrm, Ptnt);
      Patient.Tick(Ptnt);
      delay 0.1;
   end loop;
   
   -- Turn off the machine: should return -1.0 for the next readings
   BPM.Off(BlPrm);
   
   BPM.GetPressure(BlPrm, SystolicPressure, DiastolicPressure);
   Pump.Tick(Pmp, Ptnt);
   BPM.Tick(BlPrM, Ptnt);
   Patient.Tick(Ptnt);
   
   Put("Blood pressure = ");
   Put(Item => SystolicPressure, Fore => 0, Aft => 0, Exp => 0);
   Put("/");
   Put(Item => DiastolicPressure, Fore => 0, Aft => 0, Exp => 0);
   New_Line;
   
   -- Turn the machine back on
   BPM.On(BlPrm, Ptnt);
   
   -- Ramp up the dosage to the maximum
   for I in Integer range 0..100 loop
      Pump.SetDosage(Pmp, 6.0);
      BPM.GetPressure(BlPrm, SystolicPressure, DiastolicPressure);
      
      Put("Blood pressure = ");
      Put(Item => SystolicPressure, Fore => 0, Aft => 0, Exp => 0);
      Put("/");
      Put(Item => DiastolicPressure, Fore => 0, Aft => 0, Exp => 0);
      New_Line;     
      
      Pump.Tick(Pmp, Ptnt);
      BPM.Tick(BlPrm, Ptnt);
      Patient.Tick(Ptnt);
      delay 0.1;
   end loop; 
end ManualOperationExample;
