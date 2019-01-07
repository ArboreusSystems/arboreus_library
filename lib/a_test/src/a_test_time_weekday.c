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
	
	if (atwd_numeric_to_integer("01") != 1){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Monday.\n");
	if (atwd_numeric_to_integer("02") != 2){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Tuesday.\n");
	if (atwd_numeric_to_integer("03") != 3){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Wednesday.\n");
	if (atwd_numeric_to_integer("04") != 4){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Thursday.\n");
	if (atwd_numeric_to_integer("05") != 5){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Friday.\n");
	if (atwd_numeric_to_integer("06") != 6){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Saturday.\n");
	if (atwd_numeric_to_integer("07") != 7){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Sunday.\n");
	if (atwd_numeric_to_integer("00") >= 0){FAILURE;}
	printf("Done! Numeric to Integer weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("Mon",atwd_numeric_to_alpha3("01")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Monday.\n");
	if (strcmp("Tue",atwd_numeric_to_alpha3("02")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Tuesday.\n");
	if (strcmp("Wed",atwd_numeric_to_alpha3("03")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Wednesday.\n");
	if (strcmp("Thu",atwd_numeric_to_alpha3("04")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Thursday.\n");
	if (strcmp("Fri",atwd_numeric_to_alpha3("05")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Friday.\n");
	if (strcmp("Sat",atwd_numeric_to_alpha3("06")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Saturday.\n");
	if (strcmp("Sun",atwd_numeric_to_alpha3("07")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Sunday.\n");
	if (strcmp("Not",atwd_numeric_to_alpha3("00")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("Mo",atwd_numeric_to_alpha2("01")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Monday.\n");
	if (strcmp("Tu",atwd_numeric_to_alpha2("02")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Tuesday.\n");
	if (strcmp("We",atwd_numeric_to_alpha2("03")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Wednesday.\n");
	if (strcmp("Th",atwd_numeric_to_alpha2("04")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Thursday.\n");
	if (strcmp("Fr",atwd_numeric_to_alpha2("05")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Friday.\n");
	if (strcmp("Sa",atwd_numeric_to_alpha2("06")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Saturday.\n");
	if (strcmp("Su",atwd_numeric_to_alpha2("07")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Sunday.\n");
	if (strcmp("No",atwd_numeric_to_alpha2("00")) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("Monday",atwd_numeric_to_full("01")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Monday.\n");
	if (strcmp("Tuesday",atwd_numeric_to_full("02")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Tuesday.\n");
	if (strcmp("Wednesday",atwd_numeric_to_full("03")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Wednesday.\n");
	if (strcmp("Thursday",atwd_numeric_to_full("04")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Thursday.\n");
	if (strcmp("Friday",atwd_numeric_to_full("05")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Friday.\n");
	if (strcmp("Saturday",atwd_numeric_to_full("06")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Saturday.\n");
	if (strcmp("Sunday",atwd_numeric_to_full("07")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Sunday.\n");
	if (strcmp("Notaweekday",atwd_numeric_to_full("00")) != 0){FAILURE;}
	printf("Done! Numeric to Full weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from Full
int atst_weekday_from_full(){
	
	if (atwd_full_to_integer("Monday") != 1){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Monday.\n");
	if (atwd_full_to_integer("Tuesday") != 2){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Tuesday.\n");
	if (atwd_full_to_integer("Wednesday") != 3){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Wednesday.\n");
	if (atwd_full_to_integer("Thursday") != 4){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Thursday.\n");
	if (atwd_full_to_integer("Friday") != 5){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Friday.\n");
	if (atwd_full_to_integer("Saturday") != 6){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Saturday.\n");
	if (atwd_full_to_integer("Sunday") != 7){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Sunday.\n");
	if (atwd_full_to_integer("Not-a-weekday") >= 0){FAILURE;}
	printf("Done! Full to Integer weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("Mon",atwd_full_to_alpha3("Monday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Monday.\n");
	if (strcmp("Tue",atwd_full_to_alpha3("Tuesday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Tuesday.\n");
	if (strcmp("Wed",atwd_full_to_alpha3("Wednesday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Wednesday.\n");
	if (strcmp("Thu",atwd_full_to_alpha3("Thursday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Thursday.\n");
	if (strcmp("Fri",atwd_full_to_alpha3("Friday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Friday.\n");
	if (strcmp("Sat",atwd_full_to_alpha3("Saturday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Saturday.\n");
	if (strcmp("Sun",atwd_full_to_alpha3("Sunday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Sunday.\n");
	if (strcmp("Not",atwd_full_to_alpha3("Not-a-weekday")) != 0){FAILURE;}
	printf("Done! Full to Alpha3 weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("Mo",atwd_full_to_alpha2("Monday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Monday.\n");
	if (strcmp("Tu",atwd_full_to_alpha2("Tuesday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Tuesday.\n");
	if (strcmp("We",atwd_full_to_alpha2("Wednesday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Wednesday.\n");
	if (strcmp("Th",atwd_full_to_alpha2("Thursday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Thursday.\n");
	if (strcmp("Fr",atwd_full_to_alpha2("Friday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Friday.\n");
	if (strcmp("Sa",atwd_full_to_alpha2("Saturday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Saturday.\n");
	if (strcmp("Su",atwd_full_to_alpha2("Sunday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Sunday.\n");
	if (strcmp("No",atwd_full_to_alpha2("Not-a-weekday")) != 0){FAILURE;}
	printf("Done! Full to Alpha2 weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("01",atwd_full_to_numeric("Monday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Monday.\n");
	if (strcmp("02",atwd_full_to_numeric("Tuesday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Tuesday.\n");
	if (strcmp("03",atwd_full_to_numeric("Wednesday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Wednesday.\n");
	if (strcmp("04",atwd_full_to_numeric("Thursday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Thursday.\n");
	if (strcmp("05",atwd_full_to_numeric("Friday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Friday.\n");
	if (strcmp("06",atwd_full_to_numeric("Saturday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Saturday.\n");
	if (strcmp("07",atwd_full_to_numeric("Sunday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Sunday.\n");
	if (strcmp("00",atwd_full_to_numeric("Not-a-weekday")) != 0){FAILURE;}
	printf("Done! Full to Numeric weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from Alpha3
int atst_weekday_from_alpha3(){
	
	if (atwd_alpha3_to_integer("Mon") != 1){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Monday.\n");
	if (atwd_alpha3_to_integer("Tue") != 2){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Tuesday.\n");
	if (atwd_alpha3_to_integer("Wed") != 3){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Wednesday.\n");
	if (atwd_alpha3_to_integer("Thu") != 4){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Thursday.\n");
	if (atwd_alpha3_to_integer("Fri") != 5){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Friday.\n");
	if (atwd_alpha3_to_integer("Sat") != 6){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Saturday.\n");
	if (atwd_alpha3_to_integer("Sun") != 7){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Sunday.\n");
	if (atwd_alpha3_to_integer("NOT") != -1){FAILURE;}
	printf("Done! Alpha3 to Integer weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("Mo",atwd_alpha3_to_alpha2("Mon")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Monday.\n");
	if (strcmp("Tu",atwd_alpha3_to_alpha2("Tue")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Tuesday.\n");
	if (strcmp("We",atwd_alpha3_to_alpha2("Wed")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Wednesday.\n");
	if (strcmp("Th",atwd_alpha3_to_alpha2("Thu")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Thursday.\n");
	if (strcmp("Fr",atwd_alpha3_to_alpha2("Fri")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Friday.\n");
	if (strcmp("Sa",atwd_alpha3_to_alpha2("Sat")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Saturday.\n");
	if (strcmp("Su",atwd_alpha3_to_alpha2("Sun")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Sunday.\n");
	if (strcmp("No",atwd_alpha3_to_alpha2("NOT")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("Monday",atwd_alpha3_to_full("Mon")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Monday.\n");
	if (strcmp("Tuesday",atwd_alpha3_to_full("Tue")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Tuesday.\n");
	if (strcmp("Wednesday",atwd_alpha3_to_full("Wed")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Wednesday.\n");
	if (strcmp("Thursday",atwd_alpha3_to_full("Thu")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Thursday.\n");
	if (strcmp("Friday",atwd_alpha3_to_full("Fri")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Friday.\n");
	if (strcmp("Saturday",atwd_alpha3_to_full("Sat")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Saturday.\n");
	if (strcmp("Sunday",atwd_alpha3_to_full("Sun")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Sunday.\n");
	if (strcmp("Notaweekday",atwd_alpha3_to_full("NOT")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	if (strcmp("01",atwd_alpha3_to_numeric("Mon")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Monday.\n");
	if (strcmp("02",atwd_alpha3_to_numeric("Tue")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Tuesday.\n");
	if (strcmp("03",atwd_alpha3_to_numeric("Wed")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Wednesday.\n");
	if (strcmp("04",atwd_alpha3_to_numeric("Thu")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Thursday.\n");
	if (strcmp("05",atwd_alpha3_to_numeric("Fri")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Friday.\n");
	if (strcmp("06",atwd_alpha3_to_numeric("Sat")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Saturday.\n");
	if (strcmp("07",atwd_alpha3_to_numeric("Sun")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Sunday.\n");
	if (strcmp("00",atwd_alpha3_to_numeric("NOT")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric weekday test passed for Not-a-weekday.\n");
	
	printf("\n");
	
	SUCCESS;
}

// Test weekday conversion from integer
int atst_weekday_from_integer(){
	
	if (!strcmp("Mon",atwd_integer_to_alpha3(1))){
		printf("Done! Integer to Alpha3 weekday test passed for Monday.\n");
	} else {FAILURE;}
	if (!strcmp("Tue",atwd_integer_to_alpha3(2))){
		printf("Done! Integer to Alpha3 weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	if (!strcmp("Wed",atwd_integer_to_alpha3(3))){
		printf("Done! Integer to Alpha3 weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	if (!strcmp("Thu",atwd_integer_to_alpha3(4))){
		printf("Done! Integer to Alpha3 weekday test passed for Thursday.\n");
	} else {FAILURE;}
	if (!strcmp("Fri",atwd_integer_to_alpha3(5))){
		printf("Done! Integer to Alpha3 weekday test passed for Friday.\n");
	} else {FAILURE;}
	if (!strcmp("Sat",atwd_integer_to_alpha3(6))){
		printf("Done! Integer to Alpha3 weekday test passed for Saturday.\n");
	} else {FAILURE;}
	if (!strcmp("Sun",atwd_integer_to_alpha3(7))){
		printf("Done! Integer to Alpha3 weekday test passed for Sunday.\n");
	} else {FAILURE;}
	if (!strcmp("Not",atwd_integer_to_alpha3(0))){
		printf("Done! Integer to Alpha3 weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}
	
	printf("\n");
	
	if (!strcmp("Mo",atwd_integer_to_alpha2(1))){
		printf("Done! Integer to Alpha3 weekday test passed for Monday.\n");
	} else {FAILURE;}
	if (!strcmp("Tu",atwd_integer_to_alpha2(2))){
		printf("Done! Integer to Alpha3 weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	if (!strcmp("We",atwd_integer_to_alpha2(3))){
		printf("Done! Integer to Alpha3 weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	if (!strcmp("Th",atwd_integer_to_alpha2(4))){
		printf("Done! Integer to Alpha3 weekday test passed for Thursday.\n");
	} else {FAILURE;}
	if (!strcmp("Fr",atwd_integer_to_alpha2(5))){
		printf("Done! Integer to Alpha3 weekday test passed for Friday.\n");
	} else {FAILURE;}
	if (!strcmp("Sa",atwd_integer_to_alpha2(6))){
		printf("Done! Integer to Alpha3 weekday test passed for Saturday.\n");
	} else {FAILURE;}
	if (!strcmp("Su",atwd_integer_to_alpha2(7))){
		printf("Done! Integer to Alpha3 weekday test passed for Sunday.\n");
	} else {FAILURE;}
	if (!strcmp("No",atwd_integer_to_alpha2(0))){
		printf("Done! Integer to Alpha3 weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}
	
	printf("\n");

	if (!strcmp("Monday",atwd_integer_to_full(1))){
		printf("Done! Integer to Full weekday test passed for Monday.\n");
	} else {FAILURE;}
	if (!strcmp("Tuesday",atwd_integer_to_full(2))){
		printf("Done! Integer to Full weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	if (!strcmp("Wednesday",atwd_integer_to_full(3))){
		printf("Done! Integer to Full weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	if (!strcmp("Thursday",atwd_integer_to_full(4))){
		printf("Done! Integer to Full weekday test passed for Thursday.\n");
	} else {FAILURE;}
	if (!strcmp("Friday",atwd_integer_to_full(5))){
		printf("Done! Integer to Full weekday test passed for Friday.\n");
	} else {FAILURE;}
	if (!strcmp("Saturday",atwd_integer_to_full(6))){
		printf("Done! Integer to Full weekday test passed for Saturday.\n");
	} else {FAILURE;}
	if (!strcmp("Sunday",atwd_integer_to_full(7))){
		printf("Done! Integer to Full weekday test passed for Sunday.\n");
	} else {FAILURE;}
	if (!strcmp("Notaweekday",atwd_integer_to_full(0))){
		printf("Done! Integer to Full weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}

	printf("\n");
	
	if (!strcmp("01",atwd_integer_to_numeric(1))){
		printf("Done! Integer to Numeric weekday test passed for Monday.\n");
	} else {FAILURE;}
	if (!strcmp("02",atwd_integer_to_numeric(2))){
		printf("Done! Integer to  Numeric weekday test passed for Tuesday.\n");
	} else {FAILURE;}
	if (!strcmp("03",atwd_integer_to_numeric(3))){
		printf("Done! Integer to Numeric weekday test passed for Wednesday.\n");
	} else {FAILURE;}
	if (!strcmp("04",atwd_integer_to_numeric(4))){
		printf("Done! Integer to Numeric weekday test passed for Thursday.\n");
	} else {FAILURE;}
	if (!strcmp("05",atwd_integer_to_numeric(5))){
		printf("Done! Integer to Numeric weekday test passed for Friday.\n");
	} else {FAILURE;}
	if (!strcmp("06",atwd_integer_to_numeric(6))){
		printf("Done! Integer to Numeric weekday test passed for Saturday.\n");
	} else {FAILURE;}
	if (!strcmp("07",atwd_integer_to_numeric(7))){
		printf("Done! Integer to Numeric weekday test passed for Sunday.\n");
	} else {FAILURE;}
	if (!strcmp("00",atwd_integer_to_numeric(0))){
		printf("Done! Integer to Numeric weekday test passed for Not-a-weekday.\n");
	} else {FAILURE;}
	
	printf("\n");
	
	SUCCESS;
}

