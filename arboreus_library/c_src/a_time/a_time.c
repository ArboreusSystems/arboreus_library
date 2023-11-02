/* -------------------------------------------------------------------
 *  @doc Arboreus UNIX current time handler
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 12/23/2018 at 20:47
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

// Application includes
#include "../../../arboreus_library/c_src/constants/a_constants_general.h"
#include "headers/a_time.h"
#include "headers/a_time_now_handler.h"



// --------------------------------------
// UNIX Module

int main(int Number, char *Arguments[]) {
	
	if (Number>1) {
		if (!strcmp("microseconds",Arguments[1])) {
			aTimeNowMicroseconds();
		} else if (!strcmp("milliseconds",Arguments[1])){
			aTimeNowMilliseconds();
		} else if (!strcmp("seconds",Arguments[1])){
			aTimeNowSeconds();
		} else if (!strcmp("integer",Arguments[1])){
			aTimeNowInt();
		} else if (!strcmp("integer_date",Arguments[1])){
			aTimeNowIntDate();
		} else if (!strcmp("integer_full",Arguments[1])){
			aTimeNowIntFull();
		} else if (!strcmp("integer_extend",Arguments[1])){
			aTimeNowIntExtend();
		} else if (!strcmp("rfc_822",Arguments[1])){
			aTimeNowRFC822();
		} else if (!strcmp("rfc_850",Arguments[1])){
			aTimeNowRFC850();
		} else if (!strcmp("ansi",Arguments[1])){
			aTimeNowANSI();
		} else {
			FAILURE_ERROR(1,"Wrong parameter");
		}
	} else {
		aTimeNowMicroseconds();
	}
}



// --------------------------------------
// Module API


// Return time in microseconds
int aTimeNowMicroseconds(){
	
	long long int Microseconds = 0;
	if (atnhMicroseconds(&Microseconds) == EXIT_SUCCESS) {
		printf("%lld\n",Microseconds);
		SUCCESS;
	} else {
		FAILURE_ERROR(2,"Wrong microseconds");
	}
}


// Return time in milliseconds
int aTimeNowMilliseconds(){
	
	long long int Milliseconds = 0;
	if (atnhMilliseconds(&Milliseconds) == EXIT_SUCCESS) {
		printf("%lld\n",Milliseconds);
		SUCCESS;
	} else {
		FAILURE_ERROR(2,"Wrong milliseconds");
	}
}


// Return time in seconds
int aTimeNowSeconds(){
	
	long long int Seconds = 0;
	atnhSeconds(&Seconds);
	printf("%lld\n",Seconds);
	SUCCESS;
}


// Return time like integer
int aTimeNowInt(){
	
	long long Int = 0;
	atnhInt(&Int);
	printf("%lld\n",Int);
	SUCCESS;
}


// Return date like integer
int aTimeNowIntDate(){
	
	long long Int_date = 0;
	atnhIntDate(&Int_date);
	printf("%lld\n",Int_date);
	SUCCESS;
}


// Return full time like integer
int aTimeNowIntFull(){
	
	long long Int_full = 0;
	atnhIntFull(&Int_full);
	printf("%lld\n",Int_full);
	SUCCESS;
}


// Return full time like integer
int aTimeNowIntExtend(){
	
	long long Int_extend = 0;
	if (atnhIntExtend(&Int_extend) == EXIT_SUCCESS) {
		printf("%lld\n",Int_extend);
		SUCCESS;
	} else {
		FAILURE_ERROR(2,"Wrong int_extend");
	}
}


// Return time in RFC822 format
int aTimeNowRFC822(){
	
	char *RFC_822;
	if (atnhRFC822(&RFC_822) != EXIT_SUCCESS){FAILURE;};
	printf("%s\n",RFC_822);
	SUCCESS;
}


// Return time in RFC850 format
int aTimeNowRFC850(){
	
	char *RFC_850;
	if (atnhRFC850(&RFC_850) != EXIT_SUCCESS){FAILURE;};
	printf("%s\n",RFC_850);
	SUCCESS;
}


// Return time in ANSI format
int aTimeNowANSI(){
	
	char *ANSI;
	if (atnhANSI(&ANSI) != EXIT_SUCCESS){FAILURE;};
	printf("%s\n",ANSI);
	SUCCESS;
}
