/* -------------------------------------------------------------------
 *  @doc Local test C application
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/24/2019 at 22:28
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/aConstantsGeneral.h"
#include "../../a_test/src/universal_c/headers/aTestUniversalC.h"


// Application
int main(int n,char *Arguments[]){
	
	if (aTestUniversalCRun()) {FAILURE;}
	
	printf("Local test C\n");
	
	SUCCESS;
}