package ClosedLoop
   with SPARK_Mode
is
   -- The mode of the ICD
   type ModeType is (On, Off);

   -- The three different roles in the ICD systems
   type RoleType is (Cardiologist, ClinicalAssistant, Patient);
   type PersonType is new Integer;

   type RoleAssignment is array(PersonType) of RoleType;

   type ICDType is
      record
         owner : PersonType;
         roleAssigned : RoleAssignment;
         upperBound : Integer;
         numberOfJoulesToDeliver : Integer;
         last50Shocks : Integer; -- don't care about the values for this solution
         mode : ModeType;
      end record;

   function Is_Patient(icd : ICDType; person : in PersonType) return Boolean is
     (icd.roleAssigned(person) = Patient);

   function Is_Cardiologist(icd : ICDType; person : in PersonType) return Boolean is
      (icd.roleAssigned(person) = Cardiologist);

   procedure SwitchMode(icd : in out ICDType; person : in PersonType) with
     Post => (if not Is_Patient(icd, person) then
                (if icd'Old.mode = On then icd.mode = Off else icd.mode = On)
              else icd = icd'Old);

   procedure ReadLast50Shocks(icd : in ICDType; person : in PersonType; last50Shocks : out Integer) with
     Post => (if person in icd.roleAssigned'range then last50Shocks = icd.last50Shocks else last50Shocks = -1);

   procedure ReadSettings(icd : in ICDType; person : in PersonType; upperBound, numberOfJoules : out Integer) with
     Pre => icd.mode = Off,
     Post => (if not Is_Patient(icd, person) then
                  upperBound = icd.upperBound and
                  numberOfJoules = icd.numberOfJoulesToDeliver
              else numberOfJoules = -1 and upperBound = -1);

   procedure ChangeSettings(icd : in out ICDType; person : in PersonType; upperBound, numberOfJoules : in Integer) with
     Pre => icd.mode = Off,
     Post => (if Is_Cardiologist(icd, person) then
                  icd.upperBound = upperBound and
                    icd.numberOfJoulesToDeliver = numberOfJoules
              else icd = icd'Old);
end ClosedLoop;
