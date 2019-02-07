/* -------------------------------------------------------------------
 *  @doc Arboeus library testing procedures: RFC850 string testing for UTC 0300
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/10/2019 at 14:28
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../constants/aConstantsGeneral.h"
#include "headers/aTestTimeRFC850.h"
#include "../../../a_time/src/headers/aTimeRFC850.h"


int atRFC850(void){
	
	char *RFC_850;
	atRFC850FromTimestamp(0, &RFC_850);
	char *RFC_850_ethalon = "Thursday, 01-Jan-70 03:00:00 GMT";
	if (strcmp(RFC_850_ethalon,RFC_850) != 0){FAILURE;}
	printf("Done! Generating RFC850 from UNIX-timestamp test passed.\n");
	
	SUCCESS;
}