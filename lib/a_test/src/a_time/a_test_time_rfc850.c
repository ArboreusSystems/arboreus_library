// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc
//
// @end
// Created : 01/10/2019 at 14:28
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../constants/constants_general.h"
#include "headers/a_test_time_rfc850.h"
#include "../../../a_time/src/headers/a_time_rfc850.h"


int atrfc_850(void){
	
	char *RFC_850; atrfc_850_from_timestamp(0,&RFC_850);
	char *RFC_850_ethalon = "Thursday, 01-Jan-70 03:00:00 GMT";
	if (strcmp(RFC_850_ethalon,RFC_850) != 0){FAILURE;}
	printf("Done! Generating RFC850 from UNIX-timestamp test passed.\n");
	
	SUCCESS;
}