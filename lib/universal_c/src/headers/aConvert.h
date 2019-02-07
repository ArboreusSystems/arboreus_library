// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus C data conversation handler headers
//
// @end
// Created : 12/27/2018 at 20:17
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_CONVERT_H
#define ARBOREUS_A_CONVERT_H

int acnvStringToInteger(char *String, long long int *Number);
int acnvIntegerToZString(long long int Number, char **String, int Length);
int acnvIntegerToString(long long int Number, char **String);
char acnvCipherToChar(int Cipher);
int acnvCharToCipher(int Char);

#endif //ARBOREUS_A_CONVERT_H
