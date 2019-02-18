/* -------------------------------------------------------------------
 *  @doc Test C application
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/06/2019 at 17:52
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>

// Application includes
#include "../../constants/aConstantsGeneral.h"
#include "a_time/headers/aTestTime.h"
#include "universal_c/headers/aTestUniversalC.h"


// Application
int main(int Number, char *Arguments[]) {
	
	if (aTestTimeRun()) {FAILURE;}
	if (aTestUniversalCRun()) {FAILURE;}
	
	printf("-----------------\n");
	printf("*** DONE! All tests passed.\n");
	
	SUCCESS;
}