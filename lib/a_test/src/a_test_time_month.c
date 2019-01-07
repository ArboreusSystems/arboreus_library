// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboeus library testing procedures: month name testing
//
// @end
// Created : 01/07/2019 at 12:38
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_test_time_month.h"
#include "../../a_time/src/headers/a_time_month.h"


// Test month name from numeric
int atmn_month_from_numeric(){
	SUCCESS;
};


// Test month name from alpha2
int atmn_month_from_alpha2(){
	SUCCESS;
};


// Test month name from alpha3
int atmn_month_from_alpha3(){
	
	if (strcmp("01",atmn_alpha3_to_numeric("Jan")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for January.\n");
	if (strcmp("02",atmn_alpha3_to_numeric("Feb")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for February.\n");
	if (strcmp("03",atmn_alpha3_to_numeric("Mar")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for March.\n");
	if (strcmp("04",atmn_alpha3_to_numeric("Apr")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for April.\n");
	if (strcmp("05",atmn_alpha3_to_numeric("May")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for May.\n");
	if (strcmp("06",atmn_alpha3_to_numeric("Jun")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for June.\n");
	if (strcmp("07",atmn_alpha3_to_numeric("Jul")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for July.\n");
	if (strcmp("08",atmn_alpha3_to_numeric("Aug")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for August.\n");
	if (strcmp("09",atmn_alpha3_to_numeric("Sep")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for September.\n");
	if (strcmp("10",atmn_alpha3_to_numeric("Oct")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for October.\n");
	if (strcmp("11",atmn_alpha3_to_numeric("Nov")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for November.\n");
	if (strcmp("12",atmn_alpha3_to_numeric("Dec")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for December.\n");
	if (strcmp("00",atmn_alpha3_to_numeric("Notamonth")) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for Notamonth.\n");
	
	printf("\n");
	
	if (strcmp("January",atmn_alpha3_to_full("Jan")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for January.\n");
	if (strcmp("February",atmn_alpha3_to_full("Feb")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for February.\n");
	if (strcmp("March",atmn_alpha3_to_full("Mar")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for March.\n");
	if (strcmp("April",atmn_alpha3_to_full("Apr")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for April.\n");
	if (strcmp("May",atmn_alpha3_to_full("May")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for May.\n");
	if (strcmp("June",atmn_alpha3_to_full("Jun")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for June.\n");
	if (strcmp("July",atmn_alpha3_to_full("Jul")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for July.\n");
	if (strcmp("August",atmn_alpha3_to_full("Aug")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for August.\n");
	if (strcmp("September",atmn_alpha3_to_full("Sep")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for September.\n");
	if (strcmp("October",atmn_alpha3_to_full("Oct")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for October.\n");
	if (strcmp("November",atmn_alpha3_to_full("Nov")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for November.\n");
	if (strcmp("December",atmn_alpha3_to_full("Dec")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for December.\n");
	if (strcmp("Notamonth",atmn_alpha3_to_full("Notamonth")) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for Notamonth.\n");
	
	printf("\n");
	
	if (strcmp("Ja",atmn_alpha3_to_alpha2("Jan")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for January.\n");
	if (strcmp("Fb",atmn_alpha3_to_alpha2("Feb")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for February.\n");
	if (strcmp("Mr",atmn_alpha3_to_alpha2("Mar")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for March.\n");
	if (strcmp("Ap",atmn_alpha3_to_alpha2("Apr")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for April.\n");
	if (strcmp("Ma",atmn_alpha3_to_alpha2("May")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for May.\n");
	if (strcmp("Jn",atmn_alpha3_to_alpha2("Jun")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for June.\n");
	if (strcmp("Jl",atmn_alpha3_to_alpha2("Jul")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for July.\n");
	if (strcmp("Ag",atmn_alpha3_to_alpha2("Aug")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for August.\n");
	if (strcmp("Sp",atmn_alpha3_to_alpha2("Sep")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for September.\n");
	if (strcmp("Oc",atmn_alpha3_to_alpha2("Oct")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for October.\n");
	if (strcmp("No",atmn_alpha3_to_alpha2("Nov")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for November.\n");
	if (strcmp("De",atmn_alpha3_to_alpha2("Dec")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for December.\n");
	if (strcmp("No",atmn_alpha3_to_alpha2("Notamonth")) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for Notamonth.\n");
	
	printf("\n");
	
	if (atmn_alpha3_to_integer("Jan") != 1){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for January.\n");
	if (atmn_alpha3_to_integer("Feb") != 2){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for February.\n");
	if (atmn_alpha3_to_integer("Mar") != 3){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for March.\n");
	if (atmn_alpha3_to_integer("Apr") != 4){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for April.\n");
	if (atmn_alpha3_to_integer("May") != 5){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for May.\n");
	if (atmn_alpha3_to_integer("Jun") != 6){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for June.\n");
	if (atmn_alpha3_to_integer("Jul") != 7){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for July.\n");
	if (atmn_alpha3_to_integer("Aug") != 8){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for August.\n");
	if (atmn_alpha3_to_integer("Sep") != 9){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for September.\n");
	if (atmn_alpha3_to_integer("Oct") != 10){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for October.\n");
	if (atmn_alpha3_to_integer("Nov") != 11){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for November.\n");
	if (atmn_alpha3_to_integer("Dec") != 12){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for December.\n");
	if (atmn_alpha3_to_integer("Not") != -1){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for Notamonth.\n");
	
	printf("\n");
	
	SUCCESS;
};


// Test month name from integer
int atmn_month_from_integer(){
	
	if (strcmp("January",atmn_integer_to_full(1)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for January.\n");
	if (strcmp("February",atmn_integer_to_full(2)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for February.\n");
	if (strcmp("March",atmn_integer_to_full(3)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for March.\n");
	if (strcmp("April",atmn_integer_to_full(4)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for April.\n");
	if (strcmp("May",atmn_integer_to_full(5)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for May.\n");
	if (strcmp("June",atmn_integer_to_full(6)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for June.\n");
	if (strcmp("July",atmn_integer_to_full(7)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for July.\n");
	if (strcmp("August",atmn_integer_to_full(8)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for August.\n");
	if (strcmp("September",atmn_integer_to_full(9)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for September.\n");
	if (strcmp("October",atmn_integer_to_full(10)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for October.\n");
	if (strcmp("November",atmn_integer_to_full(11)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for November.\n");
	if (strcmp("December",atmn_integer_to_full(12)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for December.\n");
	if (strcmp("Notamonth",atmn_integer_to_full(13)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for Notamonth.\n");
	
	printf("\n");
	
	if (strcmp("Jan",atmn_integer_to_alpha3(1)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for January.\n");
	if (strcmp("Feb",atmn_integer_to_alpha3(2)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for February.\n");
	if (strcmp("Mar",atmn_integer_to_alpha3(3)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for March.\n");
	if (strcmp("Apr",atmn_integer_to_alpha3(4)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for April.\n");
	if (strcmp("May",atmn_integer_to_alpha3(5)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for May.\n");
	if (strcmp("Jun",atmn_integer_to_alpha3(6)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for June.\n");
	if (strcmp("Jul",atmn_integer_to_alpha3(7)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for July.\n");
	if (strcmp("Aug",atmn_integer_to_alpha3(8)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for August.\n");
	if (strcmp("Sep",atmn_integer_to_alpha3(9)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for September.\n");
	if (strcmp("Oct",atmn_integer_to_alpha3(10)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for October.\n");
	if (strcmp("Nov",atmn_integer_to_alpha3(11)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for November.\n");
	if (strcmp("Dec",atmn_integer_to_alpha3(12)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for December.\n");
	if (strcmp("Not",atmn_integer_to_alpha3(13)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for Notamonth.\n");
	
	printf("\n");
	
	if (strcmp("Ja",atmn_integer_to_alpha2(1)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for January.\n");
	if (strcmp("Fb",atmn_integer_to_alpha2(2)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for February.\n");
	if (strcmp("Mr",atmn_integer_to_alpha2(3)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for March.\n");
	if (strcmp("Ap",atmn_integer_to_alpha2(4)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for April.\n");
	if (strcmp("Ma",atmn_integer_to_alpha2(5)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for May.\n");
	if (strcmp("Jn",atmn_integer_to_alpha2(6)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for June.\n");
	if (strcmp("Jl",atmn_integer_to_alpha2(7)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for July.\n");
	if (strcmp("Ag",atmn_integer_to_alpha2(8)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for August.\n");
	if (strcmp("Sp",atmn_integer_to_alpha2(9)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for September.\n");
	if (strcmp("Oc",atmn_integer_to_alpha2(10)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for October.\n");
	if (strcmp("No",atmn_integer_to_alpha2(11)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for November.\n");
	if (strcmp("De",atmn_integer_to_alpha2(12)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for December.\n");
	if (strcmp("No",atmn_integer_to_alpha2(13)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for Notamonth.\n");
	
	printf("\n");
	
	if (strcmp("01",atmn_integer_to_numeric(1)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for January.\n");
	if (strcmp("02",atmn_integer_to_numeric(2)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for February.\n");
	if (strcmp("03",atmn_integer_to_numeric(3)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for March.\n");
	if (strcmp("04",atmn_integer_to_numeric(4)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for April.\n");
	if (strcmp("05",atmn_integer_to_numeric(5)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for May.\n");
	if (strcmp("06",atmn_integer_to_numeric(6)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for June.\n");
	if (strcmp("07",atmn_integer_to_numeric(7)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for July.\n");
	if (strcmp("08",atmn_integer_to_numeric(8)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for August.\n");
	if (strcmp("09",atmn_integer_to_numeric(9)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for September.\n");
	if (strcmp("10",atmn_integer_to_numeric(10)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for October.\n");
	if (strcmp("11",atmn_integer_to_numeric(11)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for November.\n");
	if (strcmp("12",atmn_integer_to_numeric(12)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for December.\n");
	if (strcmp("00",atmn_integer_to_numeric(13)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for Notamonth.\n");
	
	printf("\n");
	
	
	SUCCESS;
};
