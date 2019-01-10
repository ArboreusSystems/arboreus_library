// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus C part test module
//
// @end
// Created : 01/06/2019 at 17:52
// -------------------------------------------------------------------

// System includes
#include <stdio.h>

// Application includes
#include "../../constants/constants_general.h"
#include "a_time/headers/a_test_time.h"


int main(int Number, char *Arguments[]) {
	
	if (a_test_time_run()){FAILURE;}
	
	printf("-----------------\n");
	printf("*** DONE! All tests passed.\n");
	
	SUCCESS;
}