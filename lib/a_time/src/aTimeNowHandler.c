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
#include <sys/time.h>
#include <sys/timeb.h>

// Application includes
#include "../../constants/aConstantsGeneral.h"
#include "headers/aTimeNowHandler.h"
#include "headers/aTimeWeekday.h"
#include "headers/aTimeMonth.h"
#include "headers/aTimeRFC822.h"
#include "headers/aTimeRFC850.h"
#include "headers/aTimeANSI.h"
#include "../../universal_c/src/headers/aConvert.h"
#include "../../universal_c/src/headers/aString.h"


// Return time in microseconds
int atnhMicroseconds(long long int *Pointer){
	
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
int atnhMilliseconds(long long int *Pointer){
	
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
int atnhSeconds(long long int *Pointer){
	
	*Pointer = time(NULL);
	
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
	if (atRFC822FromStruct(*Today, *Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}


// Return time in RFC850 format "Sunday, 06-Nov-94 08:49:37 GMT"
int atnhRFC850(char **RFC_850){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &RFC_850;
	if (atRFC850FromStruct(*Today, *Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}


// Return time in ANSI format "Sun Nov  6 08:49:37 1994"
int atnhANSI(char **ANSI){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char ***Output = &ANSI;
	if (atANSIFromStruct(*Today, *Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}