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
#include "../../constants/aConstantsGeneral.h"
#include "headers/aConvert.h"
#include "headers/aString.h"



// Convert string to integer
int acnvStringToInteger(char *String, long long int *Number){
	
	size_t Size = strlen(String);
	int i = 0;
	int Sign = 1;
	int Cipher = 0;
	
	if (Size < 20){
		if (String[0] == '-'){
			Sign = -1; i++;
		}
		for(;i < Size; i++){
			Cipher = acnvCharToCipher((int) String[i]);
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
int acnvIntegerToZString(long long int Number, char **String, int Length){
	
	int Max_length = 18;
	int Max_size = Max_length + 2;
	int Size = 0;
	long long int Sign = 0;
	char Buffer[Max_size];
	int Counter_b = 0;
	char Output[Max_size];
	int Counter_o = 0;
	
	if (Length > Max_length){
		FAILURE;
	}
	if ((Sign = Number) < 0){
		Sign = -Sign;
	}
	do {
		Buffer[Counter_b++] = acnvCipherToChar((int) (Sign % 10));
	} while ((Sign /= 10) > 0);
	if (Counter_b < Length){
		while (Counter_b < Length){
			Buffer[Counter_b] = '0';
			Counter_b++;
		};
	}
	if (Number < 0){
		Buffer[Counter_b++] = '-';
		Sign = 1;
	} else {
		Sign = 0;
	}
	Size = Length + (int)Sign - 1;
	for (; Size >= 0; Counter_o++, Size--){
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
}


// Convert integer to string
int acnvIntegerToString(long long int Number, char **String){
	
	char Output[20];
	long long int Sign = 0;
	int Counter = 0;
	
	if ((Sign = Number) < 0){
		Sign = -Sign;
	}
	do {
		Output[Counter++] = acnvCipherToChar((int) (Sign % 10));
	} while ((Sign /= 10) > 0);
	if (Number < 0){
		Output[Counter++] = '-';
	}
	Output[Counter] = '\0';
	astrReverse(Output);
	*String = malloc(Counter * sizeof(char));
	if (*String != NULL){
		strcpy(*String,Output);
		SUCCESS;
	} else {
		FAILURE;
	}
}


// Convert cipher to unicode char
char acnvCipherToChar(int Cipher){
	return (char)('0' + Cipher);
}


// Convert unicode char to cipher
int acnvCharToCipher(int Char){
	
	int Cipher = Char - '0';
	
	if (Cipher < 0){
		return -1;
	} else if(Cipher > 9){
		return -1;
	} else {
		return (Char - '0');
	}
}