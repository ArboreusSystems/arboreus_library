// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus C string handler headers
//
// @end
// Created : 12/27/2018 at 20:04
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_STRING_H
#define ARBOREUS_A_STRING_H

#ifdef __cplusplus
extern "C" {
#endif

int astrReverse(char *String);

int astrIsLatin(char *String,int Length);
int astrIsLatinNumeric(char *String,int Length);

#ifdef __cplusplus
}
#endif

#endif //ARBOREUS_A_STRING_H
