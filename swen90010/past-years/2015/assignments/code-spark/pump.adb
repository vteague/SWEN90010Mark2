with RandomFloat;
with Patient;

package body Pump is
   
   procedure Init(P : out PumpType) is
   begin
      P.IsOn := False;
      P.CurrentDosage := 0.0;
   end Init;
   
   procedure On(P : in out PumpType) is
   begin
      P.IsOn := True;
   end On;
      
   procedure Off(P : in out PumpType) is
   begin
      P.IsOn := False;
   end Off;
   
   function IsOn(P : in PumpType) return Boolean is
   begin
      return P.IsOn;
   end IsOn;
   
   procedure SetDosage(P : in out PumpType; D : in Measures.Dosage) is
   begin
      -- Only set the dosage if the machine is on
      if P.IsOn then
	 P.CurrentDosage := D;
      end if;
   end SetDosage;
   
   procedure Tick(P : in PumpType; Ptnt : in out Patient.PatientType) is
      PtntVariable : Patient.PatientType;
   begin
      -- Administer the dosage if the machine is on
      if P.IsOn then
	 -- For an 'out' variable, we must create a new variable for
	 --  the call, and the copy the output value from
	 --  AdministerDosage back to Ptnt
	 PtntVariable := Ptnt;
	 Patient.AdministerDosage(PtntVariable, P.CurrentDosage);
	 Ptnt := PtntVariable;
      end if;
   end Tick;
   
end Pump;
