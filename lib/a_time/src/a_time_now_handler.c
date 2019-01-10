// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus current time handler
//
// @end
// Created : 12/24/2018 at 13:54
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/timeb.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_now_handler.h"
#include "headers/a_time_weekday.h"
#include "headers/a_time_month.h"
#include "headers/a_time_rfc822.h"
#include "headers/a_time_rfc850.h"
#include "headers/a_time_ansi.h"
#include "../../universal_c/src/headers/a_convert.h"
#include "../../universal_c/src/headers/a_string.h"


// Return time in microseconds
int atnh_microseconds(long long int *Pointer){
	
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


// Return time in milliseconds
int atnh_milliseconds(long long int *Pointer){
	
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


// Return time in seconds
int atnh_seconds(long long int *Pointer){
	
	*Pointer = time(NULL);
	
	SUCCESS;
}


// Return time like integer
int atnh_int(long long int *Pointer){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	*Pointer =
		(long long int)Today->tm_hour*10000 +
		(long long int)Today->tm_min*100 +
		(long long int)Today->tm_sec;
	
	SUCCESS;
}


// Return date like integer
int atnh_int_date(long long int *Pointer){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	*Pointer =
		((long long int)Today->tm_year+1900)*10000 +
		((long long int)Today->tm_mon+1)*100 +
		Today->tm_mday;
	
	SUCCESS;
}


// Return full time like integer
int atnh_int_full(long long int *Pointer){
	
	time_t Time = time(NULL);
	struct tm *Today;
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


// Return full time like integer within extended precession
int atnh_int_extend(long long int *Pointer){
	
	struct timeval Time;
	if (!gettimeofday(&Time, NULL)) {
		div_t Extend = div(Time.tv_usec,10);
		const long int Seconds = Time.tv_sec;
		struct tm *Today;
		Today = localtime(&Seconds);
		*Pointer =
			((long long int)Today->tm_year+1900)*1000000000000000 +
			((long long int)Today->tm_mon+1)*10000000000000 +
			(long long int)Today->tm_mday*100000000000 +
			(long long int)Today->tm_hour*1000000000 +
			(long long int)Today->tm_min*10000000 +
			(long long int)Today->tm_sec*100000 +
			(long long int)Extend.quot;
	} else {
		*Pointer = -1;
	}
	
	SUCCESS;
}


// Return time in RFC822 format "Sun, 06 Nov 1994 08:49:37 GMT"
int atnh_rfc_822(char **RFC_822){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &RFC_822;
	if (atrfc_822_from_struct(*Today,*Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}


// Return time in RFC850 format "Sunday, 06-Nov-94 08:49:37 GMT"
int atnh_rfc_850(char **RFC_850){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &RFC_850;
	if (atrfc_850_from_struct(*Today,*Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}


// Return time in ANSI format "Sun Nov  6 08:49:37 1994"
int atnh_ansi(char **ANSI){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &ANSI;
	if (atansi_from_struct(*Today,*Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}