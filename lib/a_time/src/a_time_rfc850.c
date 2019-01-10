// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus RFC850 time format handler
//
// @end
// Created : 01/09/2019 at 19:02
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_rfc850.h"
#include "headers/a_time_weekday.h"
#include "headers/a_time_month.h"
#include "../../universal_c/src/headers/a_convert.h"


// Generate the RFC850 string from tm_struct
int atrfc_850_from_struct(struct tm Time_struct,char **RFC_850){
	
	int Counter = 0;
	int Weekday_length = 0;
	char Output[45];
	
	char *Weekday = (char*)atwd_integer_to_full(Time_struct.tm_wday);
	Weekday_length = (int)strlen(Weekday);
	for (;Weekday_length > 0;--Weekday_length,++Counter){
		Output[Counter] = Weekday[Counter];
	}
	Output[Counter++] = ','; Output[Counter++] = ' ';
	char *Day; acnv_integer_to_zstring(Time_struct.tm_mday,&Day,2);
	Output[Counter++] = Day[0]; Output[Counter++] = Day[1]; Output[Counter++] = '-';
	char *Month = (char*)atmn_integer_to_alpha3(Time_struct.tm_mon);
	Output[Counter++] = Month[0]; Output[Counter++] = Month[1]; Output[Counter++] = Month[2]; Output[Counter++] = '-';
	char *Year; acnv_integer_to_zstring((Time_struct.tm_year+1900)%100,&Year,2);
	Output[Counter++] = Year[0]; Output[Counter++] = Year[1]; Output[Counter++] = ' ';
	char *Hours; acnv_integer_to_zstring(Time_struct.tm_hour,&Hours,2);
	Output[Counter++] = Hours[0]; Output[Counter++] = Hours[1]; Output[Counter++] = ':';
	char *Minutes; acnv_integer_to_zstring(Time_struct.tm_min,&Minutes,2);
	Output[Counter++] = Minutes[0]; Output[Counter++] = Minutes[1]; Output[Counter++] = ':';
	char *Seconds; acnv_integer_to_zstring(Time_struct.tm_sec,&Seconds,2);
	Output[Counter++] = Seconds[0]; Output[Counter++] = Seconds[1]; Output[Counter++] = ' ';
	Output[Counter++] = 'G'; Output[Counter++] = 'M'; Output[Counter++] = 'T';
	Output[Counter] = '\0';
	
	*RFC_850 = malloc(strlen(Output)*sizeof(char));
	
	if (*RFC_850 != NULL){
		strcpy(*RFC_850,Output);
		SUCCESS;
	} else {
		FAILURE;
	}
}


// Generate RFC850 string from UNIX-timestamp
int atrfc_850_from_timestamp(long long int Timestamp,char **RFC_850){
	
	time_t Time = Timestamp;
	struct tm *Time_struct = localtime(&Time);
	char ***Output = &RFC_850;
	if (atrfc_850_from_struct(*Time_struct,*Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}