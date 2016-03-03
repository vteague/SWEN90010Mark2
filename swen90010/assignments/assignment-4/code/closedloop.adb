package body ClosedLoop
   with SPARK_Mode
is

   procedure SwitchMode(icd : in out ICDType; person : in PersonType) is
   begin
      if not Is_Patient(icd, person) then
         if icd.mode = On then
            icd.mode := Off;
         else
            icd.mode := On;
         end if;
      end if;
   end;

   procedure ReadLast50Shocks(icd : in ICDType; person : in PersonType; last50Shocks : out Integer) is
   begin
      if person in icd.roleAssigned'range then
         last50Shocks := icd.last50Shocks;
      else
         last50Shocks := -1;
      end if;
   end ReadLast50Shocks;

   procedure ReadSettings(icd : in ICDType; person : in PersonType; upperBound, numberOfJoules : out Integer) is
   begin
      if not Is_Patient(icd, person) then
         upperBound := icd.upperBound;
         numberOfJoules := icd.numberOfJoulesToDeliver;
      else
         upperBound := -1;
         numberOfJoules := -1;
      end if;
   end ReadSettings;

   procedure ChangeSettings(icd : in out ICDType; person : in PersonType; upperBound, numberOfJoules : in Integer) is
      pragma Warnings (Off, "unused initial value of ""person""",
                       Reason => "Parameter is used by the precondition");
   begin
      if Is_Cardiologist(icd, person) then
         icd.upperBound := upperBound;
         icd.numberOfJoulesToDeliver := numberOfJoules;
      end if;
   end ChangeSettings;
end ClosedLoop;
