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

int acnv_string_to_integer(char *String, long long int *Number);
int acnv_integer_to_zstring(long long int Number, char **String, int Length);
int acnv_integer_to_string(long long int Number, char **String);
char acnv_cipher_to_char(int Cipher);
int acnv_char_to_cipher(int Char);

#endif //ARBOREUS_A_CONVERT_H
