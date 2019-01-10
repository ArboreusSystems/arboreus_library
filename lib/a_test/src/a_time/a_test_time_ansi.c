// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc
//
// @end
// Created : 01/10/2019 at 15:54
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../constants/constants_general.h"
#include "headers/a_test_time_ansi.h"
#include "../../../a_time/src/headers/a_time_ansi.h"


int atansi(void){
	
	char *ANSI; atansi_from_timestamp(0,&ANSI);
	char *ANSI_ethalon = "Thu Jan 01 03:00:00 1970";
	if (strcmp(ANSI_ethalon,ANSI) != 0){FAILURE;}
	printf("Done! Generating ANSI from UNIX-timestamp test passed.\n");
	
	SUCCESS;
}