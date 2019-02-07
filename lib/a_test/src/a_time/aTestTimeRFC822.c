/* -------------------------------------------------------------------
 *  @doc Arboeus library testing procedures: RFC822 string testing for UTC 0300
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/07/2019 at 16:45
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../constants/aConstantsGeneral.h"
#include "headers/aTestTimeRFC822.h"
#include "../../../a_time/src/headers/aTimeRFC822.h"


int atRFC822(void){
	
	char *RFC_822;
	atRFC822FromTimestamp(0, &RFC_822);
	char *RFC_822_ethalon = "Thu, 01 Jan 1970 03:00:00 GMT";
	if (strcmp(RFC_822_ethalon,RFC_822) != 0){FAILURE;}
	printf("Done! Generating RFC822 from UNIX-timestamp test passed.\n");
	
	SUCCESS;
}
