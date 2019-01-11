// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus UNIX current time handler
//
// @end
// Created : 12/23/2018 at 20:47
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_now.h"
#include "headers/a_time_now_handler.h"



// --------------------------------------
// UNIX Module

int main(int Number, char *Arguments[]) {
	
	if (Number>1) {
		if (!strcmp("microseconds",Arguments[1])) {
			a_time_now_microseconds();
		} else if (!strcmp("milliseconds",Arguments[1])){
			a_time_now_milliseconds();
		} else if (!strcmp("seconds",Arguments[1])){
			a_time_now_seconds();
		} else if (!strcmp("integer",Arguments[1])){
			a_time_now_int();
		} else if (!strcmp("integer_date",Arguments[1])){
			a_time_now_int_date();
		} else if (!strcmp("integer_full",Arguments[1])){
			a_time_now_int_full();
		} else if (!strcmp("integer_extend",Arguments[1])){
			a_time_now_int_extend();
		} else if (!strcmp("rfc_822",Arguments[1])){
			a_time_now_rfc_822();
		} else if (!strcmp("rfc_850",Arguments[1])){
			a_time_now_rfc_850();
		} else if (!strcmp("ansi",Arguments[1])){
			a_time_now_ansi();
		} else {
			FAILURE_ERROR(1,"Wrong parameter");
		}
	} else {
		a_time_now_microseconds();
	}
}



// --------------------------------------
// Module API


// Return time in microseconds
int a_time_now_microseconds(){
	
	long long int Microseconds = 0;
	if (atnh_microseconds(&Microseconds) == EXIT_SUCCESS) {
		printf("%lld\n",Microseconds);
		SUCCESS;
	} else {
		FAILURE_ERROR(2,"Wrong microseconds");
	}
}


// Return time in milliseconds
int a_time_now_milliseconds(){
	
	long long int Milliseconds = 0;
	if (atnh_milliseconds(&Milliseconds) == EXIT_SUCCESS) {
		printf("%lld\n",Milliseconds);
		SUCCESS;
	} else {
		FAILURE_ERROR(2,"Wrong milliseconds");
	}
}


// Return time in seconds
int a_time_now_seconds(){
	
	long long int Seconds = 0;
	atnh_seconds(&Seconds);
	printf("%lld\n",Seconds);
	SUCCESS;
}


// Return time like integer
int a_time_now_int(){
	
	long long Int = 0;
	atnh_int(&Int);
	printf("%lld\n",Int);
	SUCCESS;
}


// Return date like integer
int a_time_now_int_date(){
	
	long long Int_date = 0;
	atnh_int_date(&Int_date);
	printf("%lld\n",Int_date);
	SUCCESS;
}


// Return full time like integer
int a_time_now_int_full(){
	
	long long Int_full = 0;
	atnh_int_full(&Int_full);
	printf("%lld\n",Int_full);
	SUCCESS;
}


// Return full time like integer
int a_time_now_int_extend(){
	
	long long Int_extend = 0;
	if (atnh_int_extend(&Int_extend) == EXIT_SUCCESS) {
		printf("%lld\n",Int_extend);
		SUCCESS;
	} else {
		FAILURE_ERROR(2,"Wrong int_extend");
	}
}


// Return time in RFC822 format
int a_time_now_rfc_822(){
	
	char *RFC_822;
	if (atnh_rfc_822(&RFC_822) != EXIT_SUCCESS){FAILURE;};
	printf("%s\n",RFC_822);
	SUCCESS;
}


// Return time in RFC850 format
int a_time_now_rfc_850(){
	
	char *RFC_850;
	if (atnh_rfc_850(&RFC_850) != EXIT_SUCCESS){FAILURE;};
	printf("%s\n",RFC_850);
	SUCCESS;
}


// Return time in ANSI format
int a_time_now_ansi(){
	
	char *ANSI;
	if (atnh_ansi(&ANSI) != EXIT_SUCCESS){FAILURE;};
	printf("%s\n",ANSI);
	SUCCESS;
}