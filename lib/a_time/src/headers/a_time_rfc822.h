// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus RFC822 time format handler headers
//
// @end
// Created : 01/07/2019 at 16:44
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_RFC822_H
#define ARBOREUS_A_TIME_RFC822_H

#include <sys/time.h>
#include <sys/timeb.h>

int atrfc_822_from_struct(struct tm Time_struct,char **RFC_822);
int atrfc_822_from_timestamp(long long int Timestamp,char **RFC22);

#endif //ARBOREUS_A_TIME_RFC822_H
