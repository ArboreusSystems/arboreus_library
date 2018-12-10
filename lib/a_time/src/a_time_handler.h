// -------------------------------------------------------------------
// Arboreus A_time handler headers
// @author Alexandr KIRILOV (http://alexandr.kirilov.me)
// @copyright (C) 2015-2019, Arboreus, (http://arboreus.systems)
//
// Created at 18:23 03.12.2018
// -------------------------------------------------------------------


#ifndef ARBOREUS_A_TIME_HANDLER_H
#define ARBOREUS_A_TIME_HANDLER_H

int a_time_seconds(long long int *Pointer);
int a_time_milliseconds(long long int *Pointer);
int a_time_microseconds(long long int *Pointer);
int a_time_date_int(long long int *Pointer);
int a_time_full_int(long long int *Pointer);
int a_time_int(long long int *Pointer);
int a_time_date(int Pointer[3]);

#endif
