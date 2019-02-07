/* -------------------------------------------------------------------
 *  @doc Arboeus library testing procedures: weekday name testing
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/06/2019 at 17:49
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../constants/aConstantsGeneral.h"
#include "headers/aTestTimeWeekday.h"
#include "../../../a_time/src/headers/aTimeWeekday.h"


// Test weekday conversion from numeric
int atstWeekdayFromNumeric(void){
	
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_MONDAY) != A_WEEKDAY_INT_MONDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_TUESDAY) != A_WEEKDAY_INT_TUESDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_WEDNESDAY) != A_WEEKDAY_INT_WEDNESDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_THURSDAY) != A_WEEKDAY_INT_THURSDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_FRIDAY) != A_WEEKDAY_INT_FRIDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_SATURDAY) != A_WEEKDAY_INT_SATURDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_SUNDAY) != A_WEEKDAY_INT_SUNDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (atwdNumericToInteger(A_WEEKDAY_NUMERIC_NOTAWEEKDAY) != A_WEEKDAY_INT_NOTAWEEKDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA3_MONDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_MONDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_TUESDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_TUESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_THURSDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_THURSDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_FRIDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_FRIDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SATURDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_SATURDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SUNDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_SUNDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_NOTAWEEKDAY, atwdNumericToAlpha3(A_WEEKDAY_NUMERIC_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_MONDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_TUESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_THURSDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_FRIDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_SATURDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_SUNDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY, atwdNumericToAlpha2(A_WEEKDAY_NUMERIC_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_FULL_MONDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_MONDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_TUESDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_TUESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_WEDNESDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_THURSDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_THURSDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_FRIDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_FRIDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SATURDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_SATURDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SUNDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_SUNDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_NOTAWEEKDAY, atwdNumericToFull(A_WEEKDAY_NUMERIC_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from Full
int atstWeekdayFromFull(void){
	
	if (atwdFullToInteger(A_WEEKDAY_FULL_MONDAY) != A_WEEKDAY_INT_MONDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (atwdFullToInteger(A_WEEKDAY_FULL_TUESDAY) != A_WEEKDAY_INT_TUESDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (atwdFullToInteger(A_WEEKDAY_FULL_WEDNESDAY) != A_WEEKDAY_INT_WEDNESDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (atwdFullToInteger(A_WEEKDAY_FULL_THURSDAY) != A_WEEKDAY_INT_THURSDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (atwdFullToInteger(A_WEEKDAY_FULL_FRIDAY) != A_WEEKDAY_INT_FRIDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (atwdFullToInteger(A_WEEKDAY_FULL_SATURDAY) != A_WEEKDAY_INT_SATURDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (atwdFullToInteger(A_WEEKDAY_FULL_SUNDAY) != A_WEEKDAY_INT_SUNDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (atwdFullToInteger(A_WEEKDAY_FULL_NOTAWEEKDAY) != A_WEEKDAY_INT_NOTAWEEKDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA3_MONDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_MONDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_TUESDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_TUESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_THURSDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_THURSDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_FRIDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_FRIDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SATURDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_SATURDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SUNDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_SUNDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_NOTAWEEKDAY, atwdFullToAlpha3(A_WEEKDAY_FULL_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_MONDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_TUESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_THURSDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_FRIDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_SATURDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_SUNDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY, atwdFullToAlpha2(A_WEEKDAY_FULL_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_NUMERIC_MONDAY, atwdFullToNumeric(A_WEEKDAY_FULL_MONDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_TUESDAY, atwdFullToNumeric(A_WEEKDAY_FULL_TUESDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY, atwdFullToNumeric(A_WEEKDAY_FULL_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_THURSDAY, atwdFullToNumeric(A_WEEKDAY_FULL_THURSDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_FRIDAY, atwdFullToNumeric(A_WEEKDAY_FULL_FRIDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SATURDAY, atwdFullToNumeric(A_WEEKDAY_FULL_SATURDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SUNDAY, atwdFullToNumeric(A_WEEKDAY_FULL_SUNDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_NOTAWEEKDAY, atwdFullToNumeric(A_WEEKDAY_FULL_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from Alpha3
int atstWeekdayFromAlpha3(void){
	
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_MONDAY) != A_WEEKDAY_INT_MONDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_TUESDAY) != A_WEEKDAY_INT_TUESDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_WEDNESDAY) != A_WEEKDAY_INT_WEDNESDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_THURSDAY) != A_WEEKDAY_INT_THURSDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_FRIDAY) != A_WEEKDAY_INT_FRIDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_SATURDAY) != A_WEEKDAY_INT_SATURDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_SUNDAY) != A_WEEKDAY_INT_SUNDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (atwdAlpha3ToInteger(A_WEEKDAY_ALPHA3_NOTAWEEKDAY) != A_WEEKDAY_INT_NOTAWEEKDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_MONDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_TUESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_THURSDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_FRIDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_SATURDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_SUNDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY, atwdAlpha3ToAlpha2(A_WEEKDAY_ALPHA3_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_FULL_MONDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_MONDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_TUESDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_TUESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_WEDNESDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_THURSDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_THURSDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_FRIDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_FRIDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SATURDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_SATURDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SUNDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_SUNDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_NOTAWEEKDAY, atwdAlpha3ToFull(A_WEEKDAY_ALPHA3_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_NUMERIC_MONDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_MONDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_TUESDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_TUESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_THURSDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_THURSDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_FRIDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_FRIDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SATURDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_SATURDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SUNDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_SUNDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_NOTAWEEKDAY, atwdAlpha3ToNumeric(A_WEEKDAY_ALPHA3_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from integer
int atstWeekdayFromInteger(void){
	
	if (strcmp(A_WEEKDAY_ALPHA3_MONDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_TUESDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_THURSDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_FRIDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SATURDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SUNDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_NOTAWEEKDAY, atwdIntegerToAlpha3(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY, atwdIntegerToAlpha2(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");

	if (strcmp(A_WEEKDAY_FULL_MONDAY, atwdIntegerToFull(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_TUESDAY, atwdIntegerToFull(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_WEDNESDAY, atwdIntegerToFull(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_THURSDAY, atwdIntegerToFull(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_FRIDAY, atwdIntegerToFull(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SATURDAY, atwdIntegerToFull(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SUNDAY, atwdIntegerToFull(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_NOTAWEEKDAY, atwdIntegerToFull(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");

	printf("\n");
	
	if (strcmp(A_WEEKDAY_NUMERIC_MONDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_TUESDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_THURSDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_FRIDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SATURDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SUNDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_NOTAWEEKDAY, atwdIntegerToNumeric(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}
