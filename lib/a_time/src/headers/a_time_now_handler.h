// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc
//
// @end
// Created : 12/24/2018 at 13:56
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_NOW_HANDLER_H
#define ARBOREUS_A_TIME_NOW_HANDLER_H

int atnh_microseconds(long long int *Pointer);
int atnh_milliseconds(long long int *Pointer);
int atnh_seconds(long long int *Pointer);
int atnh_int(long long int *Pointer);
int atnh_int_date(long long int *Pointer);
int atnh_int_full(long long int *Pointer);
int atnh_int_extend(long long int *Pointer);
int atnh_rfc_822(char **RFC_822);
int atnh_rfc_850(char **RFC_850);
int atnh_ansi();

#endif //ARBOREUS_A_TIME_NOW_HANDLER_H
