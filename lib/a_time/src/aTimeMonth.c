/* -------------------------------------------------------------------
 *  @doc Arboreus ANSI time format handler
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 12/24/2018 at 18:17
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Application includes
#include "../../constants/aConstantsGeneral.h"
#include "headers/aTimeMonth.h"
#include "../../universal_c/src/headers/aConvert.h"


// Return integer month from numeric
int atmnNumericToInteger(char *Numeric){
	
	if (!strcmp(A_MONTH_NUMERIC_JANUARY,Numeric)){return A_MONTH_INT_JANUARY;}
	if (!strcmp(A_MONTH_NUMERIC_FEBRUARY,Numeric)){return A_MONTH_INT_FEBRUARY;}
	if (!strcmp(A_MONTH_NUMERIC_MARCH,Numeric)){return A_MONTH_INT_MARCH;}
	if (!strcmp(A_MONTH_NUMERIC_APRIL,Numeric)){return A_MONTH_INT_APRIL;}
	if (!strcmp(A_MONTH_NUMERIC_MAY,Numeric)){return A_MONTH_INT_MAY;}
	if (!strcmp(A_MONTH_NUMERIC_JUNE,Numeric)){return A_MONTH_INT_JUNE;}
	if (!strcmp(A_MONTH_NUMERIC_JULY,Numeric)){return A_MONTH_INT_JULY;}
	if (!strcmp(A_MONTH_NUMERIC_AUGUST,Numeric)){return A_MONTH_INT_AUGUST;}
	if (!strcmp(A_MONTH_NUMERIC_SEPTEMBER,Numeric)){return A_MONTH_INT_SEPTEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_OCTOBER,Numeric)){return A_MONTH_INT_OCTOBER;}
	if (!strcmp(A_MONTH_NUMERIC_NOVEMBER,Numeric)){return A_MONTH_INT_NOVEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_DECEMBER,Numeric)){return A_MONTH_INT_DECEMBER;}
	return A_MONTH_INT_NOTAMONTH;
}


// Return alpha3 month from numeric
const char *atmnNumericToAlpha3(char *Numeric){
	
	if (!strcmp(A_MONTH_NUMERIC_JANUARY,Numeric)){return A_MONTH_ALPHA3_JANUARY;}
	if (!strcmp(A_MONTH_NUMERIC_FEBRUARY,Numeric)){return A_MONTH_ALPHA3_FEBRUARY;}
	if (!strcmp(A_MONTH_NUMERIC_MARCH,Numeric)){return A_MONTH_ALPHA3_MARCH;}
	if (!strcmp(A_MONTH_NUMERIC_APRIL,Numeric)){return A_MONTH_ALPHA3_APRIL;}
	if (!strcmp(A_MONTH_NUMERIC_MAY,Numeric)){return A_MONTH_ALPHA3_MAY;}
	if (!strcmp(A_MONTH_NUMERIC_JUNE,Numeric)){return A_MONTH_ALPHA3_JUNE;}
	if (!strcmp(A_MONTH_NUMERIC_JULY,Numeric)){return A_MONTH_ALPHA3_JULY;}
	if (!strcmp(A_MONTH_NUMERIC_AUGUST,Numeric)){return A_MONTH_ALPHA3_AUGUST;}
	if (!strcmp(A_MONTH_NUMERIC_SEPTEMBER,Numeric)){return A_MONTH_ALPHA3_SEPTEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_OCTOBER,Numeric)){return A_MONTH_ALPHA3_OCTOBER;}
	if (!strcmp(A_MONTH_NUMERIC_NOVEMBER,Numeric)){return A_MONTH_ALPHA3_NOVEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_DECEMBER,Numeric)){return A_MONTH_ALPHA3_DECEMBER;}
	return A_MONTH_ALPHA3_NOTAMONTH;
}


// Return alpha2 month from numeric
const char *atmnNumericToAlpha2(char *Numeric){
	
	if (!strcmp(A_MONTH_NUMERIC_JANUARY,Numeric)){return A_MONTH_ALPHA2_JANUARY;}
	if (!strcmp(A_MONTH_NUMERIC_FEBRUARY,Numeric)){return A_MONTH_ALPHA2_FEBRUARY;}
	if (!strcmp(A_MONTH_NUMERIC_MARCH,Numeric)){return A_MONTH_ALPHA2_MARCH;}
	if (!strcmp(A_MONTH_NUMERIC_APRIL,Numeric)){return A_MONTH_ALPHA2_APRIL;}
	if (!strcmp(A_MONTH_NUMERIC_MAY,Numeric)){return A_MONTH_ALPHA2_MAY;}
	if (!strcmp(A_MONTH_NUMERIC_JUNE,Numeric)){return A_MONTH_ALPHA2_JUNE;}
	if (!strcmp(A_MONTH_NUMERIC_JULY,Numeric)){return A_MONTH_ALPHA2_JULY;}
	if (!strcmp(A_MONTH_NUMERIC_AUGUST,Numeric)){return A_MONTH_ALPHA2_AUGUST;}
	if (!strcmp(A_MONTH_NUMERIC_SEPTEMBER,Numeric)){return A_MONTH_ALPHA2_SEPTEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_OCTOBER,Numeric)){return A_MONTH_ALPHA2_OCTOBER;}
	if (!strcmp(A_MONTH_NUMERIC_NOVEMBER,Numeric)){return A_MONTH_ALPHA2_NOVEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_DECEMBER,Numeric)){return A_MONTH_ALPHA2_DECEMBER;}
	return A_MONTH_ALPHA2_NOTAMONTH;
}


// Return full month from numeric
const char *atmnNumericToFull(char *Numeric){
	
	if (!strcmp(A_MONTH_NUMERIC_JANUARY,Numeric)){return A_MONTH_FULL_JANUARY;}
	if (!strcmp(A_MONTH_NUMERIC_FEBRUARY,Numeric)){return A_MONTH_FULL_FEBRUARY;}
	if (!strcmp(A_MONTH_NUMERIC_MARCH,Numeric)){return A_MONTH_FULL_MARCH;}
	if (!strcmp(A_MONTH_NUMERIC_APRIL,Numeric)){return A_MONTH_FULL_APRIL;}
	if (!strcmp(A_MONTH_NUMERIC_MAY,Numeric)){return A_MONTH_FULL_MAY;}
	if (!strcmp(A_MONTH_NUMERIC_JUNE,Numeric)){return A_MONTH_FULL_JUNE;}
	if (!strcmp(A_MONTH_NUMERIC_JULY,Numeric)){return A_MONTH_FULL_JULY;}
	if (!strcmp(A_MONTH_NUMERIC_AUGUST,Numeric)){return A_MONTH_FULL_AUGUST;}
	if (!strcmp(A_MONTH_NUMERIC_SEPTEMBER,Numeric)){return A_MONTH_FULL_SEPTEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_OCTOBER,Numeric)){return A_MONTH_FULL_OCTOBER;}
	if (!strcmp(A_MONTH_NUMERIC_NOVEMBER,Numeric)){return A_MONTH_FULL_NOVEMBER;}
	if (!strcmp(A_MONTH_NUMERIC_DECEMBER,Numeric)){return A_MONTH_FULL_DECEMBER;}
	return A_MONTH_FULL_NOTAMONTH;
}


// Return integer month from alpha2
int atmnAlpha2ToInteger(char *Alpha2){
	
	if (!strcmp(A_MONTH_ALPHA2_JANUARY,Alpha2)){return A_MONTH_INT_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA2_FEBRUARY,Alpha2)){return A_MONTH_INT_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA2_MARCH,Alpha2)){return A_MONTH_INT_MARCH;}
	if (!strcmp(A_MONTH_ALPHA2_APRIL,Alpha2)){return A_MONTH_INT_APRIL;}
	if (!strcmp(A_MONTH_ALPHA2_MAY,Alpha2)){return A_MONTH_INT_MAY;}
	if (!strcmp(A_MONTH_ALPHA2_JUNE,Alpha2)){return A_MONTH_INT_JUNE;}
	if (!strcmp(A_MONTH_ALPHA2_JULY,Alpha2)){return A_MONTH_INT_JULY;}
	if (!strcmp(A_MONTH_ALPHA2_AUGUST,Alpha2)){return A_MONTH_INT_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA2_SEPTEMBER,Alpha2)){return A_MONTH_INT_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_OCTOBER,Alpha2)){return A_MONTH_INT_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA2_NOVEMBER,Alpha2)){return A_MONTH_INT_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_DECEMBER,Alpha2)){return A_MONTH_INT_DECEMBER;}
	return A_MONTH_INT_NOTAMONTH;
}


// Return alpha3 month from alpha2
const char *atmnAlpha2ToAlpha3(char *Alpha2){
	
	if (!strcmp(A_MONTH_ALPHA2_JANUARY,Alpha2)){return A_MONTH_ALPHA3_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA2_FEBRUARY,Alpha2)){return A_MONTH_ALPHA3_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA2_MARCH,Alpha2)){return A_MONTH_ALPHA3_MARCH;}
	if (!strcmp(A_MONTH_ALPHA2_APRIL,Alpha2)){return A_MONTH_ALPHA3_APRIL;}
	if (!strcmp(A_MONTH_ALPHA2_MAY,Alpha2)){return A_MONTH_ALPHA3_MAY;}
	if (!strcmp(A_MONTH_ALPHA2_JUNE,Alpha2)){return A_MONTH_ALPHA3_JUNE;}
	if (!strcmp(A_MONTH_ALPHA2_JULY,Alpha2)){return A_MONTH_ALPHA3_JULY;}
	if (!strcmp(A_MONTH_ALPHA2_AUGUST,Alpha2)){return A_MONTH_ALPHA3_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA2_SEPTEMBER,Alpha2)){return A_MONTH_ALPHA3_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_OCTOBER,Alpha2)){return A_MONTH_ALPHA3_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA2_NOVEMBER,Alpha2)){return A_MONTH_ALPHA3_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_DECEMBER,Alpha2)){return A_MONTH_ALPHA3_DECEMBER;}
	return A_MONTH_ALPHA3_NOTAMONTH;
}


// Return full month from alpha2
const char *atmnAlpha2ToFull(char *Alpha2){
	
	if (!strcmp(A_MONTH_ALPHA2_JANUARY,Alpha2)){return A_MONTH_FULL_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA2_FEBRUARY,Alpha2)){return A_MONTH_FULL_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA2_MARCH,Alpha2)){return A_MONTH_FULL_MARCH;}
	if (!strcmp(A_MONTH_ALPHA2_APRIL,Alpha2)){return A_MONTH_FULL_APRIL;}
	if (!strcmp(A_MONTH_ALPHA2_MAY,Alpha2)){return A_MONTH_FULL_MAY;}
	if (!strcmp(A_MONTH_ALPHA2_JUNE,Alpha2)){return A_MONTH_FULL_JUNE;}
	if (!strcmp(A_MONTH_ALPHA2_JULY,Alpha2)){return A_MONTH_FULL_JULY;}
	if (!strcmp(A_MONTH_ALPHA2_AUGUST,Alpha2)){return A_MONTH_FULL_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA2_SEPTEMBER,Alpha2)){return A_MONTH_FULL_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_OCTOBER,Alpha2)){return A_MONTH_FULL_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA2_NOVEMBER,Alpha2)){return A_MONTH_FULL_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_DECEMBER,Alpha2)){return A_MONTH_FULL_DECEMBER;}
	return A_MONTH_FULL_NOTAMONTH;
}


// Return numeric month from alpha2
const char *atmnAlpha2ToNumeric(char *Alpha2){
	
	if (!strcmp(A_MONTH_ALPHA2_JANUARY,Alpha2)){return A_MONTH_NUMERIC_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA2_FEBRUARY,Alpha2)){return A_MONTH_NUMERIC_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA2_MARCH,Alpha2)){return A_MONTH_NUMERIC_MARCH;}
	if (!strcmp(A_MONTH_ALPHA2_APRIL,Alpha2)){return A_MONTH_NUMERIC_APRIL;}
	if (!strcmp(A_MONTH_ALPHA2_MAY,Alpha2)){return A_MONTH_NUMERIC_MAY;}
	if (!strcmp(A_MONTH_ALPHA2_JUNE,Alpha2)){return A_MONTH_NUMERIC_JUNE;}
	if (!strcmp(A_MONTH_ALPHA2_JULY,Alpha2)){return A_MONTH_NUMERIC_JULY;}
	if (!strcmp(A_MONTH_ALPHA2_AUGUST,Alpha2)){return A_MONTH_NUMERIC_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA2_SEPTEMBER,Alpha2)){return A_MONTH_NUMERIC_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_OCTOBER,Alpha2)){return A_MONTH_NUMERIC_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA2_NOVEMBER,Alpha2)){return A_MONTH_NUMERIC_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA2_DECEMBER,Alpha2)){return A_MONTH_NUMERIC_DECEMBER;}
	return A_MONTH_NUMERIC_NOTAMONTH;
}


// Return month integer from alpha3
int atmnAlpha3ToInteger(char *Alpha3){
	
	if (!strcmp(A_MONTH_ALPHA3_JANUARY,Alpha3)){return A_MONTH_INT_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA3_FEBRUARY,Alpha3)){return A_MONTH_INT_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA3_MARCH,Alpha3)){return A_MONTH_INT_MARCH;}
	if (!strcmp(A_MONTH_ALPHA3_APRIL,Alpha3)){return A_MONTH_INT_APRIL;}
	if (!strcmp(A_MONTH_ALPHA3_MAY,Alpha3)){return A_MONTH_INT_MAY;}
	if (!strcmp(A_MONTH_ALPHA3_JUNE,Alpha3)){return A_MONTH_INT_JUNE;}
	if (!strcmp(A_MONTH_ALPHA3_JULY,Alpha3)){return A_MONTH_INT_JULY;}
	if (!strcmp(A_MONTH_ALPHA3_AUGUST,Alpha3)){return A_MONTH_INT_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA3_SEPTEMBER,Alpha3)){return A_MONTH_INT_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_OCTOBER,Alpha3)){return A_MONTH_INT_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA3_NOVEMBER,Alpha3)){return A_MONTH_INT_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_DECEMBER,Alpha3)){return A_MONTH_INT_DECEMBER;}
	return A_MONTH_INT_NOTAMONTH;
}


// Return month alpha2 from alpha3
const char *atmnAlpha3ToAlpha2(char *Alpha3){
	
	if (!strcmp(A_MONTH_ALPHA3_JANUARY,Alpha3)){return A_MONTH_ALPHA2_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA3_FEBRUARY,Alpha3)){return A_MONTH_ALPHA2_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA3_MARCH,Alpha3)){return A_MONTH_ALPHA2_MARCH;}
	if (!strcmp(A_MONTH_ALPHA3_APRIL,Alpha3)){return A_MONTH_ALPHA2_APRIL;}
	if (!strcmp(A_MONTH_ALPHA3_MAY,Alpha3)){return A_MONTH_ALPHA2_MAY;}
	if (!strcmp(A_MONTH_ALPHA3_JUNE,Alpha3)){return A_MONTH_ALPHA2_JUNE;}
	if (!strcmp(A_MONTH_ALPHA3_JULY,Alpha3)){return A_MONTH_ALPHA2_JULY;}
	if (!strcmp(A_MONTH_ALPHA3_AUGUST,Alpha3)){return A_MONTH_ALPHA2_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA3_SEPTEMBER,Alpha3)){return A_MONTH_ALPHA2_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_OCTOBER,Alpha3)){return A_MONTH_ALPHA2_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA3_NOVEMBER,Alpha3)){return A_MONTH_ALPHA2_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_DECEMBER,Alpha3)){return A_MONTH_ALPHA2_DECEMBER;}
	return A_MONTH_ALPHA2_NOTAMONTH;
}


// Return month full from alpha3
const char *atmnAlpha3ToFull(char *Alpha3){
	
	if (!strcmp(A_MONTH_ALPHA3_JANUARY,Alpha3)){return A_MONTH_FULL_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA3_FEBRUARY,Alpha3)){return A_MONTH_FULL_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA3_MARCH,Alpha3)){return A_MONTH_FULL_MARCH;}
	if (!strcmp(A_MONTH_ALPHA3_APRIL,Alpha3)){return A_MONTH_FULL_APRIL;}
	if (!strcmp(A_MONTH_ALPHA3_MAY,Alpha3)){return A_MONTH_FULL_MAY;}
	if (!strcmp(A_MONTH_ALPHA3_JUNE,Alpha3)){return A_MONTH_FULL_JUNE;}
	if (!strcmp(A_MONTH_ALPHA3_JULY,Alpha3)){return A_MONTH_FULL_JULY;}
	if (!strcmp(A_MONTH_ALPHA3_AUGUST,Alpha3)){return A_MONTH_FULL_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA3_SEPTEMBER,Alpha3)){return A_MONTH_FULL_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_OCTOBER,Alpha3)){return A_MONTH_FULL_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA3_NOVEMBER,Alpha3)){return A_MONTH_FULL_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_DECEMBER,Alpha3)){return A_MONTH_FULL_DECEMBER;}
	return A_MONTH_FULL_NOTAMONTH;
};


// Return month numeric from alpha3
const char *atmnAlpha3ToNumeric(char *Alpha3){
	
	if (!strcmp(A_MONTH_ALPHA3_JANUARY,Alpha3)){return A_MONTH_NUMERIC_JANUARY;}
	if (!strcmp(A_MONTH_ALPHA3_FEBRUARY,Alpha3)){return A_MONTH_NUMERIC_FEBRUARY;}
	if (!strcmp(A_MONTH_ALPHA3_MARCH,Alpha3)){return A_MONTH_NUMERIC_MARCH;}
	if (!strcmp(A_MONTH_ALPHA3_APRIL,Alpha3)){return A_MONTH_NUMERIC_APRIL;}
	if (!strcmp(A_MONTH_ALPHA3_MAY,Alpha3)){return A_MONTH_NUMERIC_MAY;}
	if (!strcmp(A_MONTH_ALPHA3_JUNE,Alpha3)){return A_MONTH_NUMERIC_JUNE;}
	if (!strcmp(A_MONTH_ALPHA3_JULY,Alpha3)){return A_MONTH_NUMERIC_JULY;}
	if (!strcmp(A_MONTH_ALPHA3_AUGUST,Alpha3)){return A_MONTH_NUMERIC_AUGUST;}
	if (!strcmp(A_MONTH_ALPHA3_SEPTEMBER,Alpha3)){return A_MONTH_NUMERIC_SEPTEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_OCTOBER,Alpha3)){return A_MONTH_NUMERIC_OCTOBER;}
	if (!strcmp(A_MONTH_ALPHA3_NOVEMBER,Alpha3)){return A_MONTH_NUMERIC_NOVEMBER;}
	if (!strcmp(A_MONTH_ALPHA3_DECEMBER,Alpha3)){return A_MONTH_NUMERIC_DECEMBER;}
	return A_MONTH_NUMERIC_NOTAMONTH;
};


// Return month alpha3 from integer
const char *atmnIntegerToAlpha3(int Month){
	
	if (Month == A_MONTH_INT_JANUARY) {return A_MONTH_ALPHA3_JANUARY;} else
	if (Month == A_MONTH_INT_FEBRUARY) {return A_MONTH_ALPHA3_FEBRUARY;} else
	if (Month == A_MONTH_INT_MARCH) {return A_MONTH_ALPHA3_MARCH;} else
	if (Month == A_MONTH_INT_APRIL) {return A_MONTH_ALPHA3_APRIL;} else
	if (Month == A_MONTH_INT_MAY) {return A_MONTH_ALPHA3_MAY;} else
	if (Month == A_MONTH_INT_JUNE) {return A_MONTH_ALPHA3_JUNE;} else
	if (Month == A_MONTH_INT_JULY) {return A_MONTH_ALPHA3_JULY;} else
	if (Month == A_MONTH_INT_AUGUST) {return A_MONTH_ALPHA3_AUGUST;} else
	if (Month == A_MONTH_INT_SEPTEMBER) {return A_MONTH_ALPHA3_SEPTEMBER;} else
	if (Month == A_MONTH_INT_OCTOBER) {return A_MONTH_ALPHA3_OCTOBER;} else
	if (Month == A_MONTH_INT_NOVEMBER) {return A_MONTH_ALPHA3_NOVEMBER;} else
	if (Month == A_MONTH_INT_DECEMBER) {return A_MONTH_ALPHA3_DECEMBER;} else
	{return "Not";}
};


// Return month alpha2 from integer
const char *atmnIntegerToAlpha2(int Month){
	
	if (Month == A_MONTH_INT_JANUARY) {return A_MONTH_ALPHA2_JANUARY;} else
	if (Month == A_MONTH_INT_FEBRUARY) {return A_MONTH_ALPHA2_FEBRUARY;} else
	if (Month == A_MONTH_INT_MARCH) {return A_MONTH_ALPHA2_MARCH;} else
	if (Month == A_MONTH_INT_APRIL) {return A_MONTH_ALPHA2_APRIL;} else
	if (Month == A_MONTH_INT_MAY) {return A_MONTH_ALPHA2_MAY;} else
	if (Month == A_MONTH_INT_JUNE) {return A_MONTH_ALPHA2_JUNE;} else
	if (Month == A_MONTH_INT_JULY) {return A_MONTH_ALPHA2_JULY;} else
	if (Month == A_MONTH_INT_AUGUST) {return A_MONTH_ALPHA2_AUGUST;} else
	if (Month == A_MONTH_INT_SEPTEMBER) {return A_MONTH_ALPHA2_SEPTEMBER;} else
	if (Month == A_MONTH_INT_OCTOBER) {return A_MONTH_ALPHA2_OCTOBER;} else
	if (Month == A_MONTH_INT_NOVEMBER) {return A_MONTH_ALPHA2_NOVEMBER;} else
	if (Month == A_MONTH_INT_DECEMBER) {return A_MONTH_ALPHA2_DECEMBER;} else
	{return A_MONTH_ALPHA2_NOTAMONTH;}
};


// Return month full from integer
const char *atmnIntegerToFull(int Month){
	
	if (Month == A_MONTH_INT_JANUARY) {return A_MONTH_FULL_JANUARY;} else
	if (Month == A_MONTH_INT_FEBRUARY) {return A_MONTH_FULL_FEBRUARY;} else
	if (Month == A_MONTH_INT_MARCH) {return A_MONTH_FULL_MARCH;} else
	if (Month == A_MONTH_INT_APRIL) {return A_MONTH_FULL_APRIL;} else
	if (Month == A_MONTH_INT_MAY) {return A_MONTH_FULL_MAY;} else
	if (Month == A_MONTH_INT_JUNE) {return A_MONTH_FULL_JUNE;} else
	if (Month == A_MONTH_INT_JULY) {return A_MONTH_FULL_JULY;} else
	if (Month == A_MONTH_INT_AUGUST) {return A_MONTH_FULL_AUGUST;} else
	if (Month == A_MONTH_INT_SEPTEMBER) {return A_MONTH_FULL_SEPTEMBER;} else
	if (Month == A_MONTH_INT_OCTOBER) {return A_MONTH_FULL_OCTOBER;} else
	if (Month == A_MONTH_INT_NOVEMBER) {return A_MONTH_FULL_NOVEMBER;} else
	if (Month == A_MONTH_INT_DECEMBER) {return A_MONTH_FULL_DECEMBER;} else
	{return A_MONTH_FULL_NOTAMONTH;}
};


// Return month numeric from integer
const char *atmnTntegerToNumeric(int Month){
	
	if (Month > A_MONTH_INT_DECEMBER){return A_MONTH_NUMERIC_NOTAMONTH;}
	if (Month < A_MONTH_INT_JANUARY){return A_MONTH_NUMERIC_NOTAMONTH;}
	
	char *Numeric;
	
	if (acnvIntegerToZString(Month + 1, &Numeric, 2) == EXIT_SUCCESS){
		return Numeric;
	} else {
		return A_MONTH_NUMERIC_NOTAMONTH;
	}
};