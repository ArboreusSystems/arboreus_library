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

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_convert.h"



// Convert cipher to unicode char
char acnv_cipher_to_char(int Cipher){
	return (char)('0' + Cipher);
}


// Convert unicode char to cipher
int acnv_char_to_cipher(char Char){
	return (Char - '0');
}