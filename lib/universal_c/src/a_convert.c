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



// Convert string to integer
int acnv_string_to_integer(char *String, long long int *Number){
	
	size_t Size = strlen(String);
	int i = 0;
	int Sign = 1;
	int Cipher = 0;
	
	if (Size < 20){
		if (String[0] == '-'){
			Sign = -1; i++;
		}
		for(;i < Size; i++){
			Cipher = acnv_char_to_cipher((int)String[i]);
			if (Cipher < 0){
				return -3;
			} else {
				*Number = *Number * 10 + Cipher;
			}
		}
		*Number = *Number * Sign;
		SUCCESS;
	} else {
		return -2;
	}
}


// Convert integer to string within zero
int acnv_integer_to_zstring(long long int Number, char **String, int Length){
	
	int Max_length = 18;
	int Max_size = Max_length + 2;
	
	if (Length > Max_length){
		FAILURE;
	}
	
	char Buffer[Max_size];
	char Output[Max_size];
	long long int Sign = 0;
	int Counter_b = 0;
	int Counter_o = 0;
	
	if ((Sign = Number) < 0){
		Sign = -Sign;
	}
	do {
		Buffer[Counter_b++] = acnv_cipher_to_char((int)(Sign % 10));
	} while ((Sign /= 10) > 0);
	if (Counter_b < Length){
		while (Counter_b < Length){
			Buffer[Counter_b++] = '0';
		};
	}
	if (Number < 0){
		Buffer[Counter_b++] = '-';
		Sign = 1;
	} else {
		Sign = 0;
	}
	if ((Counter_b - Sign) > Length){
		for (; Counter_o < (Length + Sign); Counter_o++){
			Output[Counter_o] = Buffer[--Counter_b];
		}
		Output[Counter_o] = '\0';
		*String = malloc(Counter_o * sizeof(char));
		if (*String != NULL){
			strcpy(*String,Output);
			SUCCESS;
		} else {
			FAILURE;
		}
	} else {
		Buffer[Counter_b] = '\0';
		astr_reverse(Buffer);
		*String = malloc(Counter_b * sizeof(char));
		if (*String != NULL){
			strcpy(*String,Buffer);
			SUCCESS;
		} else {
			FAILURE;
		}
	}
}


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
int acnv_char_to_cipher(int Char){
	
	int Cipher = Char - '0';
	
	if (Cipher < 0){
		return -1;
	} else if(Cipher > 9){
		return -1;
	} else {
		return (Char - '0');
	}
}