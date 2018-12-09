// -------------------------------------------------------------------
// Arboreus A_time FreeBSD addon
// @author Alexandr KIRILOV (http://alexandr.kirilov.me)
// @copyright (C) 2015-2019, Arboreus, (http://arboreus.systems)
//
// Created at 18:23 03.12.2018
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>
#include <err.h>

// Application includes
#include "../../constants/constants_general.h"
#include "a_time_handler.h"


int main(int Number,char *Arguments[]){
	
	long long int Time = 0;

	if (Number > 1) {
		if (!strcmp("seconds",Arguments[1])) {
			a_time_seconds(&Time);
		} else if (!strcmp("milliseconds",Arguments[1])) {
			a_time_milliseconds(&Time);
		} else if (!strcmp("microseconds",Arguments[1])){
			a_time_microseconds(&Time);
		} else if (!strcmp("date_int",Arguments[1])){
			a_time_date_int(&Time);
		} else if (!strcmp("full_int",Arguments[1])){
			a_time_full_int(&Time);
		} else {
			Time = -1;
		}
	} else {
		a_time_microseconds(&Time);
	}
	
	if (Time > 0) {
		printf("%lld\n",Time);
		SUCCESS;
	} else {
		FAIL_ERROR(1,"Wrong parameter");
	}
}
