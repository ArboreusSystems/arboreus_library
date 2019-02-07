/* -------------------------------------------------------------------
 *  @doc Arboeus library testing procedures: ANSI string testing for UTC 0300
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/10/2019 at 15:54
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../constants/aConstantsGeneral.h"
#include "headers/aTestTimeANSI.h"
#include "../../../a_time/src/headers/aTimeANSI.h"


int atANSI(void){
	
	char *ANSI;
	atANSIFromTimestamp(0, &ANSI);
	char *ANSI_ethalon = "Thu Jan 01 03:00:00 1970";
	if (strcmp(ANSI_ethalon,ANSI) != 0){FAILURE;}
	printf("Done! Generating ANSI from UNIX-timestamp test passed.\n");
	
	SUCCESS;
}