// -------------------------------------------------------------------
// Arboreus A_time handler headers
// @author Alexandr KIRILOV (http://alexandr.kirilov.me)
// @copyright (C) 2015-2019, Arboreus, (http://arboreus.systems)
//
// Created at 18:23 03.12.2018
// -------------------------------------------------------------------


// System includes
#include <time.h>
#include <sys/time.h>
#include <sys/timeb.h>

// Application includes
#include "../../constants/constants_general.h"
#include "a_time_handler.h"


// Return time in seconds
int a_time_seconds(long long int *Pointer){
	
	time_t Seconds;
	Seconds = time(&Seconds);
	*Pointer = Seconds;
	
	SUCCESS;
}


// Return time in milliseconds
int a_time_milliseconds(long long int *Pointer){
	
	struct timeb Time;
	if (!ftime(&Time)) {
		*Pointer = ((long long int)Time.time)*1000ll + (long long int)Time.millitm;
	} else {
		*Pointer = -1;
	}
	
	SUCCESS;
}


// Return time in microseconds
int a_time_microseconds(long long int *Pointer){
	
	struct timeval Time;
	if (!gettimeofday(&Time, NULL)) {
		*Pointer = ((long long int)Time.tv_sec)*1000000ll + (long long int)Time.tv_usec;
	} else {
		*Pointer = -1;
	}
	
	SUCCESS;
}