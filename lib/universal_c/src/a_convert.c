// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus C data conversation handler
//
// @end
// Created : 12/27/2018 at 20:17
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_convert.h"
#include "headers/a_string.h"



// Convert integer to string
int acnv_integer_to_string(long long int Number, char **String){
	
	char Output[20];
	long long int Sign = 0;
	int Counter = 0;
	
	if ((Sign = Number) < 0){
		Sign = -Sign;
	}
	do {
		Output[Counter++] = acnv_cipher_to_char((int)(Sign % 10));
	} while ((Sign /= 10) > 0);
	if (Number < 0){
		Output[Counter++] = '-';
	}
	Output[Counter] = '\0';
	astr_reverse(Output);
	*String = malloc(Counter * sizeof(char));
	if (*String != NULL){
		strcpy(*String,Output);
		SUCCESS;
	} else {
		FAILURE;
	}
}


// Convert cipher to unicode char
char acnv_cipher_to_char(int Cipher){
	return (char)('0' + Cipher);
}


// Convert unicode char to cipher
int acnv_char_to_cipher(char Char){
	return (Char - '0');
}