sig Role {}
sig Person {}

sig Cardiologist extends Role {}
sig ClinicalAssistant extends Role {}
sig Patient extends Role {}

abstract sig Mode {}
sig On extends Mode {}
sig Off extends Mode {}

sig Shocks {}

sig ICD {
	---The person whose ICD this is.
    owner : Person,

	---A relation describing which people are assigned to 
	---particular roles for this ICD.
	roleAssigned: Person->Role,

	---ICD state variables.
	upperBound: Int,
	numberOfJoulesToDeliver: Int,
    last50Shocks: Shocks,
	mode: Mode

}

/** Part A  **/

pred Is_Patient(icd : ICD, person : Person) {
	person.(icd.roleAssigned) = Patient
}

pred Is_Cardiologist(icd : ICD, person : Person) {
	person.(icd.roleAssigned) = Cardiologist
}

pred SwitchMode (icd, icd' : ICD, person : Person) {
	not Is_Patient[icd, person] => {icd.mode = On => icd'.mode = Off} &&  { icd.mode = Off => icd'.mode = On} 
    Is_Patient[icd, person] => {icd = icd'}
} run SwitchMode for 3 expect 1

-- last50Shocks is the output variable
pred ReadLast50Shocks (icd : ICD, person : Person, newLast50shocks : Shocks) {
	newLast50shocks = icd.last50Shocks
}

-- newUpperBound and newNumberOfJoules are the output variables
pred ReadSettings (icd : ICD, person : Person, newUpperBound : Int, numberOfJoules : Int) {
	icd.mode = Off
    not Is_Patient[icd, person] =>
		{newUpperBound = icd.upperBound && numberOfJoules = icd.numberOfJoulesToDeliver}
	Is_Patient[icd, person] =>
		{newUpperBound = -1 && numberOfJoules = -1}	
}

pred ChangeSettings (icd, icd' : ICD, person : Person, newUpperBound : Int, newNumberOfJoules : Int) {
	icd.mode = Off
	Is_Cardiologist[icd, person] => 
		{icd'.upperBound = newUpperBound} &&  { icd'.numberOfJoulesToDeliver = newNumberOfJoules}
   not Is_Cardiologist[icd, person] =>
		{icd' = icd}
}

/** Parts B and C  **/

/**
  Some things we thought of:  

  Example 1: R1.3 Says "An authorised Cardiologist or Clinical Assistant can switch between the modes."
Commonsense indicates this means ONLY an authorised Cardiologist or Clinical Assistant
can switch between modes.  An assertion that tests this is :
*/
assert noUnqualifiedModeSwitch {
	no icd, icd' : ICD, person : Person {
		SwitchMode[icd, icd', person] &&
			person.(icd.roleAssigned) = Patient 
   			icd != icd'
	}
} 
check noUnqualifiedModeSwitch for 5

/** So we can add a fact **/
fact noUnauthorisedSwitching {
	all icd, icd' : ICD, person : Person {
 		SwitchMode[icd, icd', person]  => {person.(icd.roleAssigned) != Patient ||  icd= icd'}
	}
} 

/** 
  Example 2: A person should not be their own cardiologist or clinical assistant (even if they are qualified). 
**/
assert noSelfCare {
	no icd : ICD {
		icd.roleAssigned[icd.owner] != Patient
	}
}  
check noSelfCare for 5

/** The corresponding fact: **/
fact noSelfCareFact {
	no icd : ICD {
		icd.roleAssigned[icd.owner] != Patient
	}
}

/** 
   It also seems sensible to declare that the owner and the patient are the same, 
   though this alone does not imply noSelfCare.  
**/
fact ownerIsPatient {
	all icd : ICD {
		icd.roleAssigned[icd.owner] = Patient
	}
}

/**
 A safety property: Settings cannot be read or changed in on mode
**/
assert OnMode {
   all icd, icd' : ICD, person : Person, u,l : Int {
		icd.mode = On => not ChangeSettings[icd, icd', person, u, l] and not ReadSettings[icd, person, u, l]
   }
}
check OnMode for 5
