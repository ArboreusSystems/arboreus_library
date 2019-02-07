/* -------------------------------------------------------------------
 *  @doc Arboreus a_time module test procedures
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/06/2019 at 18:51
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>

// Application includes
#include "../../../constants/aConstantsGeneral.h"
#include "headers/aTestTime.h"
#include "headers/aTestTimeWeekday.h"
#include "headers/aTestTimeMonth.h"
#include "headers/aTestTimeRFC822.h"
#include "headers/aTestTimeRFC850.h"
#include "headers/aTestTimeANSI.h"


int aTestTimeRun(){
	
	if (atstWeekdayFromInteger() != EXIT_SUCCESS){FAILURE;}
	if (atstWeekdayFromAlpha3() != EXIT_SUCCESS){FAILURE;}
	if (atstWeekdayFromFull() != EXIT_SUCCESS){FAILURE;}
	if (atstWeekdayFromNumeric() != EXIT_SUCCESS){FAILURE;}
	
	if (atmnMonthFromInteger() != EXIT_SUCCESS){FAILURE;}
	if (atmnMonthFromAlpha3() != EXIT_SUCCESS){FAILURE;}
	if (atmnMonthFromAlpha2() != EXIT_SUCCESS){FAILURE;}
	if (atmnMonthFromNumeric() != EXIT_SUCCESS){FAILURE;}

	if (atRFC822() != EXIT_SUCCESS){FAILURE;}
	if (atRFC850() != EXIT_SUCCESS){FAILURE;}
	if (atANSI() != EXIT_SUCCESS){FAILURE;}
	
	printf("\n");
	printf("*** DONE! Tests for a_time module passed.\n\n");
	
	SUCCESS;
}