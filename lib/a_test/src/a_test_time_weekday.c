// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboeus library testing procedures: weekday name testing
//
// @end
// Created : 01/06/2019 at 17:49
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_test_time_weekday.h"
#include "../../a_time/src/headers/a_time_weekday.h"


// Test weekday conversion from numeric
int atst_weekday_from_numeric(){
	
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_MONDAY) != A_WEEKDAY_INT_MONDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_TUESDAY) != A_WEEKDAY_INT_TUESDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_WEDNESDAY) != A_WEEKDAY_INT_WEDNESDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_THURSDAY) != A_WEEKDAY_INT_THURSDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_FRIDAY) != A_WEEKDAY_INT_FRIDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_SATURDAY) != A_WEEKDAY_INT_SATURDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_SUNDAY) != A_WEEKDAY_INT_SUNDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (atwd_numeric_to_integer(A_WEEKDAY_NUMERIC_NOTAWEEKDAY) != A_WEEKDAY_INT_NOTAWEEKDAY){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA3_MONDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_MONDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_TUESDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_TUESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_THURSDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_THURSDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_FRIDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_FRIDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SATURDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_SATURDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SUNDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_SUNDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_NOTAWEEKDAY,atwd_numeric_to_alpha3(A_WEEKDAY_NUMERIC_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_MONDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_TUESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_THURSDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_FRIDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_SATURDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_SUNDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY,atwd_numeric_to_alpha2(A_WEEKDAY_NUMERIC_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_FULL_MONDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_MONDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_TUESDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_TUESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_WEDNESDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_THURSDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_THURSDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_FRIDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_FRIDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SATURDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_SATURDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SUNDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_SUNDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_NOTAWEEKDAY,atwd_numeric_to_full(A_WEEKDAY_NUMERIC_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from Full
int atst_weekday_from_full(){
	
	if (atwd_full_to_integer(A_WEEKDAY_FULL_MONDAY) != A_WEEKDAY_INT_MONDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (atwd_full_to_integer(A_WEEKDAY_FULL_TUESDAY) != A_WEEKDAY_INT_TUESDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (atwd_full_to_integer(A_WEEKDAY_FULL_WEDNESDAY) != A_WEEKDAY_INT_WEDNESDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (atwd_full_to_integer(A_WEEKDAY_FULL_THURSDAY) != A_WEEKDAY_INT_THURSDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (atwd_full_to_integer(A_WEEKDAY_FULL_FRIDAY) != A_WEEKDAY_INT_FRIDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (atwd_full_to_integer(A_WEEKDAY_FULL_SATURDAY) != A_WEEKDAY_INT_SATURDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (atwd_full_to_integer(A_WEEKDAY_FULL_SUNDAY) != A_WEEKDAY_INT_SUNDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (atwd_full_to_integer(A_WEEKDAY_FULL_NOTAWEEKDAY) != A_WEEKDAY_INT_NOTAWEEKDAY){FAILURE;}
	printf("Done! Full to Integer weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA3_MONDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_MONDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_TUESDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_TUESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_THURSDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_THURSDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_FRIDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_FRIDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SATURDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_SATURDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SUNDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_SUNDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_NOTAWEEKDAY,atwd_full_to_alpha3(A_WEEKDAY_FULL_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_MONDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_TUESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_THURSDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_FRIDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_SATURDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_SUNDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY,atwd_full_to_alpha2(A_WEEKDAY_FULL_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_NUMERIC_MONDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_MONDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_TUESDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_TUESDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_THURSDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_THURSDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_FRIDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_FRIDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SATURDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_SATURDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SUNDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_SUNDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_NOTAWEEKDAY,atwd_full_to_numeric(A_WEEKDAY_FULL_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from Alpha3
int atst_weekday_from_alpha3(){
	
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_MONDAY) != A_WEEKDAY_INT_MONDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_TUESDAY) != A_WEEKDAY_INT_TUESDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_WEDNESDAY) != A_WEEKDAY_INT_WEDNESDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_THURSDAY) != A_WEEKDAY_INT_THURSDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_FRIDAY) != A_WEEKDAY_INT_FRIDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_SATURDAY) != A_WEEKDAY_INT_SATURDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_SUNDAY) != A_WEEKDAY_INT_SUNDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (atwd_alpha3_to_integer(A_WEEKDAY_ALPHA3_NOTAWEEKDAY) != A_WEEKDAY_INT_NOTAWEEKDAY){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_MONDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_TUESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_THURSDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_FRIDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_SATURDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_SUNDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY,atwd_alpha3_to_alpha2(A_WEEKDAY_ALPHA3_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_FULL_MONDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_MONDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_TUESDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_TUESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_WEDNESDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_THURSDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_THURSDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_FRIDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_FRIDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SATURDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_SATURDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SUNDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_SUNDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_NOTAWEEKDAY,atwd_alpha3_to_full(A_WEEKDAY_ALPHA3_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_NUMERIC_MONDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_MONDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_TUESDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_TUESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_THURSDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_THURSDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_FRIDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_FRIDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SATURDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_SATURDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SUNDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_SUNDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_NOTAWEEKDAY,atwd_alpha3_to_numeric(A_WEEKDAY_ALPHA3_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from integer
int atst_weekday_from_integer(){
	
	if (strcmp(A_WEEKDAY_ALPHA3_MONDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_TUESDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_THURSDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_FRIDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SATURDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_SUNDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA3_NOTAWEEKDAY,atwd_integer_to_alpha3(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	if (strcmp(A_WEEKDAY_ALPHA2_MONDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_TUESDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_WEDNESDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_THURSDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_FRIDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SATURDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_SUNDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_ALPHA2_NOTAWEEKDAY,atwd_integer_to_alpha2(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");

	if (strcmp(A_WEEKDAY_FULL_MONDAY,atwd_integer_to_full(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_TUESDAY,atwd_integer_to_full(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_WEDNESDAY,atwd_integer_to_full(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_THURSDAY,atwd_integer_to_full(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_FRIDAY,atwd_integer_to_full(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SATURDAY,atwd_integer_to_full(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_SUNDAY,atwd_integer_to_full(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_FULL_NOTAWEEKDAY,atwd_integer_to_full(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Full weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");

	printf("\n");
	
	if (strcmp(A_WEEKDAY_NUMERIC_MONDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_MONDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_MONDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_TUESDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_TUESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_TUESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_WEDNESDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_WEDNESDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_THURSDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_THURSDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_THURSDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_FRIDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_FRIDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_FRIDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SATURDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_SATURDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_SATURDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_SUNDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_SUNDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_SUNDAY".\n");
	if (strcmp(A_WEEKDAY_NUMERIC_NOTAWEEKDAY,atwd_integer_to_numeric(A_WEEKDAY_INT_NOTAWEEKDAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric weekday test passed for "A_WEEKDAY_FULL_NOTAWEEKDAY".\n");
	
	printf("\n");
	
	SUCCESS;
}
