/* -------------------------------------------------------------------
 *  @doc Arboreus C string handler
 *  @notice
 *  
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 12/27/2018 at 20:04
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../arboreus_library/c_src/constants/a_constants_general.h"
#include "headers/a_string.h"


// Reverse string
int astrReverse(char *String){
	
	int i = 0;
	size_t Length = strlen(String) - 1;
	char Output;
	
	for (; i < (int)Length; i++, Length--) {
		Output = String[i];
		String[i] = String[Length];
		String[Length] = Output;
	}
	
	SUCCESS;
}
