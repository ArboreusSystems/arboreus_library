// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus ANSI time format handler headers
//
// @end
// Created : 01/10/2019 at 15:34
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_ANSI_H
#define ARBOREUS_A_TIME_ANSI_H

#include <sys/time.h>
#include <sys/timeb.h>

int atansi_from_struct(struct tm Time_struct,char **ANSI);
int atansi_from_timestamp(long long int Timestamp,char **ANSI);

#endif //ARBOREUS_A_TIME_ANSI_H
