// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus C string handler
//
// @end
// Created : 12/27/2018 at 20:04
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_string.h"


// Reverse string
int astr_reverse(char *String){
	
	int i = 0;
	size_t Length = strlen(String) - 1;
	char Output;
	
	for (; i < Length; i++, Length--) {
		Output = String[i];
		String[i] = String[Length];
		String[Length] = Output;
	}
	
	SUCCESS;
}