#parse("Erlang Custom Template Variables.erl")
// -------------------------------------------------------------------
// @author ${FULLNAME}
// @copyright (C) ${YEAR}, ${COMPANY}
// @doc
//
// @end
// Created : ${MONTH}/${DAY}/${YEAR} at ${TIME}
// -------------------------------------------------------------------

// System includes
#include <stdio.h>

// Application includes
#include "../../constants/constants_general.h"


int main(int Number,char *Arguments[]){

	int i = 0;

	if (Number > 1) {
		for(i=0; i<Number; ++i){
			printf("parameter %d: %c\n",i,Arguments[i])
		}
	} else {
		printf("no parameters");
	}

	SUCCESS;
}