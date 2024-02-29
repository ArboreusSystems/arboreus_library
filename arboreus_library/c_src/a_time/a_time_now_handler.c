/* -------------------------------------------------------------------
 *  @doc Arboreus current time handler
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 12/24/2018 at 13:54
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <inttypes.h>

// Application includes
#include "../../../arboreus_library/c_src/constants/a_constants_general.h"
#include "headers/a_time_now_handler.h"
#include "headers/a_time_weekday.h"
#include "headers/a_time_month.h"
#include "headers/a_time_rfc822.h"
#include "headers/a_time_rfc850.h"
#include "headers/a_time_ansi.h"
#include "../a_universal/headers/a_convert.h"
#include "../a_universal/headers/a_string.h"


// Return time in nanoseconds
int atnhNanoseconds(long long int *inPointer){

	struct timespec oTimeSpecification;
	if (clock_gettime(CLOCK_REALTIME,&oTimeSpecification)) {
		*inPointer = -1;
		FAILURE;
	}

	*inPointer = oTimeSpecification.tv_sec * 1000000000 + oTimeSpecification.tv_nsec;
	SUCCESS;
}


// Return time in microseconds
int atnhMicroseconds(long long int *inPointer){

	struct timespec oTimeSpecification;
	if (clock_gettime(CLOCK_REALTIME,&oTimeSpecification)) {
		*inPointer = -1;
		FAILURE;
	}
	
	*inPointer = oTimeSpecification.tv_sec * 1000000 + oTimeSpecification.tv_nsec / 1000;

	SUCCESS;
}


// Return time in milliseconds
int atnhMilliseconds(long long int *inPointer){

	struct timespec oTimeSpecification;
	if (clock_gettime(CLOCK_REALTIME,&oTimeSpecification)) {
		*inPointer = -1;
		FAILURE;
	}
	
	*inPointer = oTimeSpecification.tv_sec * 1000 + oTimeSpecification.tv_nsec / 1000000;
	
	SUCCESS;
}


// Return time in seconds
int atnhSeconds(long long int *inPointer){

	struct timespec oTimeSpecification;
	if (clock_gettime(CLOCK_REALTIME,&oTimeSpecification)) {
		*inPointer = -1;
		FAILURE;
	}

	*inPointer = oTimeSpecification.tv_sec;
	
	SUCCESS;
}


// Return time like integer
int atnhInt(long long int *Pointer){
	
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
int atnhIntDate(long long int *Pointer){
	
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
int atnhIntFull(long long int *Pointer){
	
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
int atnhIntExtend(long long int *Pointer){
	
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
int atnhRFC822(char **RFC_822){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &RFC_822;
	if (atRFC822FromStruct(*Today, *Output) != EXIT_SUCCESS){
		FAILURE;
	}

	SUCCESS;
}


// Return time in RFC850 format "Sunday, 06-Nov-94 08:49:37 GMT"
int atnhRFC850(char **RFC_850){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &RFC_850;
	if (atRFC850FromStruct(*Today, *Output) != EXIT_SUCCESS){
		FAILURE;
	}

	SUCCESS;
}


// Return time in ANSI format "Sun Nov  6 08:49:37 1994"
int atnhANSI(char **ANSI){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &ANSI;
	if (atANSIFromStruct(*Today, *Output) != EXIT_SUCCESS){
		FAILURE;
	}

	SUCCESS;
}
