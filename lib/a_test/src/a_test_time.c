// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus a_time module test procedures
//
// @end
// Created : 01/06/2019 at 18:51
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_test_time.h"
#include "headers/a_test_time_weekday.h"
#include "headers/a_test_time_month.h"

int a_test_time_run(){
	
	if (atst_weekday_from_integer() != EXIT_SUCCESS){FAILURE;}
	if (atst_weekday_from_alpha3() != EXIT_SUCCESS){FAILURE;}
	if (atst_weekday_from_full() != EXIT_SUCCESS){FAILURE;}
	if (atst_weekday_from_numeric() != EXIT_SUCCESS){FAILURE;}
	
	if (atmn_month_from_integer() != EXIT_SUCCESS){FAILURE;}
	if (atmn_month_from_alpha3() != EXIT_SUCCESS){FAILURE;}
	if (atmn_month_from_alpha2() != EXIT_SUCCESS){FAILURE;}
	if (atmn_month_from_numeric() != EXIT_SUCCESS){FAILURE;}
	
	printf("*** DONE! Tests for a_time module passed.\n\n");
	
	SUCCESS;
}