// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboeus library testing procedures: RFC822 string testing for UTC 0300
//
// @end
// Created : 01/07/2019 at 16:45
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../constants/constants_general.h"
#include "headers/a_test_time_rfc822.h"
#include "../../../a_time/src/headers/a_time_rfc822.h"


int atrfc_822(){
	
	char *RFC_822; atrfc_822_from_timestamp(0,&RFC_822);
	char *RFC_822_ethalon = "Thu, 01 Jan 1970 03:00:00 GMT";
	if (strcmp(RFC_822_ethalon,RFC_822) != 0){FAILURE;}
	printf("Done! Generating RFC822 from UNIX-timestamp test passed.\n");
	
	SUCCESS;
}
