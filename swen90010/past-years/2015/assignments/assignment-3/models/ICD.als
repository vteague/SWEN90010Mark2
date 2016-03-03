sig Role {}
sig Person {}

sig Cardiologist extends Role {}
sig ClinicalAssistant extends Role {}
sig Patient extends Role {}

abstract sig Mode {}
sig On extends Mode {}
sig Off extends Mode {}

-- A list of 50 shocks
sig Shocks {}

sig ICD {
	---The person whose ICD this is.
    owner : Person,

	---A relation describing which people are assigned to 
	---particular roles for this ICD.
	roleAssigned: Person->Role,

	--- ICD state variables.
	upperBound: Int,
	numberOfJoulesToDeliver: Int,
    last50Shocks: Shocks,
	mode: Mode
}

pred SwitchMode (icd, icd' : ICD, person : Person) {
}

-- last50Shocks is the output variable
pred ReadLast50Shocks (icd : ICD, person : Person, last50shocks : Shocks) {
}

-- upperBound and numberOfJoules are the output variables
pred ReadSettings (icd : ICD, person : Person, upperBound : Int, numberOfJoules : Int) {
}

pred ChangeSettings (icd, icd' : ICD, person : Person, upperBound : Int, numberOfJoules : Int) {
}
