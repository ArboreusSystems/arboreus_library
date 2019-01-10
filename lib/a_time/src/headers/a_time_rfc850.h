// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc
//
// @end
// Created : 01/09/2019 at 19:02
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_RFC850_H
#define ARBOREUS_A_TIME_RFC850_H

#include <sys/time.h>
#include <sys/timeb.h>

int atrfc_850_from_struct(struct tm Time_struct,char **RFC_850);
int atrfc_850_from_timestamp(long long int Timestamp,char **RFC_850);

#endif //ARBOREUS_A_TIME_RFC850_H
