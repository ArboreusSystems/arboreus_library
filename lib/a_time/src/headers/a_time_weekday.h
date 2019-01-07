// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus weekday name handler headers
//
// @end
// Created : 12/24/2018 at 19:10
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_WEEKDAY_H
#define ARBOREUS_A_TIME_WEEKDAY_H

const char *atwd_integer_to_alpha3(int Month);
const char *atwd_integer_to_alpha2(int Month);
const char *atwd_integer_to_full(int Month);
const char *atwd_integer_to_numeric(int Month);

int atwd_alpha3_to_integer(char *Pointer);
const char *atwd_alpha3_to_alpha2(char *Pointer);
const char *atwd_alpha3_to_full(char *Pointer);
const char *atwd_alpha3_to_numeric(char *Pointer);

int atwd_full_to_integer(char *Pointer);
const char *atwd_full_to_alpha3(char *Pointer);
const char *atwd_full_to_alpha2(char *Pointer);
const char *atwd_full_to_numeric(char *Pointer);

int atwd_numeric_to_integer(char *Pointer);
const char *atwd_numeric_to_alpha3(char *Pointer);
const char *atwd_numeric_to_alpha2(char *Pointer);
const char *atwd_numeric_to_full(char *Pointer);

#endif //ARBOREUS_A_TIME_WEEKDAY_H
