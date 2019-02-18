/* -------------------------------------------------------------------
 *  @doc Testing Universal C library part header
 *  @notice
 *  
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 02/01/2019 at 16:42
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>

// Application includes
#include "../../../constants/aConstantsGeneral.h"
#include "headers/aTestUniversalC.h"
#include "headers/aTestSymbol.h"


int aTestUniversalCRun() {
	
	if (atucIsLatinLower() != EXIT_SUCCESS) {FAILURE;}
	if (atucIsLatinUpper() != EXIT_SUCCESS) {FAILURE;}
	if (atucIsLatin() != EXIT_SUCCESS) {FAILURE;}
	if (atucIsNumeric() != EXIT_SUCCESS) {FAILURE;}
	if (atucIsAt() != EXIT_SUCCESS) {FAILURE;}
	if (atucIsDot() != EXIT_SUCCESS) {FAILURE;}
	if (atucIsSpace() != EXIT_SUCCESS) {FAILURE;}
	
	printf("\n");
	printf("*** DONE! Tests for universal_c module passed.\n\n");
	SUCCESS;
};