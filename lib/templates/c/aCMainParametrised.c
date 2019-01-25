/* -------------------------------------------------------------------
 *  @doc
 *  @notice
 *  
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/25/2019 at 13:37
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>

// Application includes
#include "../../constants/constants_general.h"


int main(int Number, char *Arguments[]) {
	
	int i = 0;
	
	if (Number > 1) {
		for (i = 0; i < Number; ++i) {
			printf("parameter %d: %c\n", i, Arguments[i])
		}
	} else {
		printf("no parameters");
	}
	
	SUCCESS;
}