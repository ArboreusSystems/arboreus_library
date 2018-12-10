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
		*Pointer =
			((long long int)Time.time)*1000ll +
			(long long int)Time.millitm;
	} else {
		*Pointer = -1;
	}
	
	SUCCESS;
}


// Return time in microseconds
int a_time_microseconds(long long int *Pointer){
	
	struct timeval Time;
	if (!gettimeofday(&Time, NULL)) {
		*Pointer =
			((long long int)Time.tv_sec)*1000000ll +
			(long long int)Time.tv_usec;
	} else {
		*Pointer = -1;
	}
	
	SUCCESS;
}


// Return date integer
int a_time_date_int(long long int *Pointer){
	
	time_t Time;
	struct tm *Today;
	time(&Time);
	Today = localtime(&Time);
	*Pointer =
		((long long int)Today->tm_year+1900)*10000 +
		((long long int)Today->tm_mon+1)*100 +
		Today->tm_mday;
	
	SUCCESS;
}


// Return full time integer
int a_time_full_int(long long int *Pointer){
	
	time_t Time;
	struct tm *Today;
	time(&Time);
	Today = localtime(&Time);
	*Pointer =
		((long long int)Today->tm_year+1900)*10000000000 +
		((long long int)Today->tm_mon+1)*100000000 +
		(long long int)Today->tm_mday*1000000 +
		(long long int)Today->tm_hour*10000 +
		(long long int)Today->tm_min*100 +
		(long long int)Today->tm_sec;
		
	SUCCESS;
}


// Return time integer
int a_time_int(long long int *Pointer){
	
	time_t Time;
	struct tm *Today;
	time(&Time);
	Today = localtime(&Time);
	*Pointer =
		(long long int)Today->tm_hour*10000 +
		(long long int)Today->tm_min*100 +
		(long long int)Today->tm_sec;
	
	SUCCESS;
}