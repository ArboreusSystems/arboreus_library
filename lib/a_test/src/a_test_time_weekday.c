// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboeus library testing procedures
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


// Test weekday conversion from Alpha3
int atst_weekday_from_alpha3(){
	SUCCESS;
}

// Test weekday conversion from integer
int atst_weekday_from_integer(){
	
	const char *Weekday_string;
	int Weekday_int = 0;
	
	Weekday_string = atwd_integer_to_alpha3(1);
	if (!strcmp("Mon",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Monday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha3(2);
	if (!strcmp("Tue",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha3(3);
	if (!strcmp("Wed",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha3(4);
	if (!strcmp("Thu",Weekday_string))
	{printf("Done! Integer to Alpha3 weekday test passed for Thursday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha3(5);
	if (!strcmp("Fri",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Friday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha3(6);
	if (!strcmp("Sat",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Saturday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha3(7);
	if (!strcmp("Sun",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Sunday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha3(0);
	if (!strcmp("Not",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}
	
	printf("\n");
	
	Weekday_string = atwd_integer_to_alpha2(1);
	if (!strcmp("Mo",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Monday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha2(2);
	if (!strcmp("Tu",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha2(3);
	if (!strcmp("We",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha2(4);
	if (!strcmp("Th",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Thursday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha2(5);
	if (!strcmp("Fr",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Friday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha2(6);
	if (!strcmp("Sa",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Saturday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha2(7);
	if (!strcmp("Su",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Sunday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_alpha2(0);
	if (!strcmp("No",Weekday_string)){
		printf("Done! Integer to Alpha3 weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}
	
	printf("\n");

	Weekday_string = atwd_integer_to_full(1);
	if (!strcmp("Monday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Monday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_full(2);
	if (!strcmp("Tuesday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_full(3);
	if (!strcmp("Wednesday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_full(4);
	if (!strcmp("Thursday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Thursday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_full(5);
	if (!strcmp("Friday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Friday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_full(6);
	if (!strcmp("Saturday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Saturday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_full(7);
	if (!strcmp("Sunday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Sunday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_full(0);
	if (!strcmp("Notaweekday",Weekday_string)){
		printf("Done! Integer to Full weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}

	printf("\n");
	
	Weekday_string = atwd_integer_to_numeric(1);
	if (!strcmp("01",Weekday_string)){
		printf("Done! Numeric weekday test passed for Monday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_numeric(2);
	if (!strcmp("02",Weekday_string)){
		printf("Done! Numeric weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_numeric(3);
	if (!strcmp("03",Weekday_string)){
		printf("Done! Numeric weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_numeric(4);
	if (!strcmp("04",Weekday_string)){
		printf("Done! Numeric weekday test passed for Thursday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_numeric(5);
	if (!strcmp("05",Weekday_string)){
		printf("Done! Numeric weekday test passed for Friday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_numeric(6);
	if (!strcmp("06",Weekday_string)){
		printf("Done! Numeric weekday test passed for Saturday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_numeric(7);
	if (!strcmp("07",Weekday_string)){
		printf("Done! Numeric weekday test passed for Sunday.\n");
	} else {FAILURE;}
	Weekday_string = atwd_integer_to_numeric(0);
	if (!strcmp("00",Weekday_string)){
		printf("Done! Numeric weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}
	
	printf("\n");
	
	SUCCESS;
}

