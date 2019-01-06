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

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_test_time.h"
#include "headers/a_test_time_weekday.h"

int a_test_time_run(){
	
	if (atst_weekday_from_integer()){FAILURE;}
	if (atst_weekday_from_alpha3()){FAILURE;}
	printf("*** DONE! Tests for a_time module passed.\n\n");
	
	
	SUCCESS;
}