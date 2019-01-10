// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus ANSI time format handler
//
// @end
// Created : 01/10/2019 at 15:34
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_ansi.h"
#include "headers/a_time_weekday.h"
#include "headers/a_time_month.h"
#include "../../universal_c/src/headers/a_convert.h"


// Generate ANSI string from tm_struct
int atansi_from_struct(struct tm Time_struct,char **ANSI){
	
	char Output[25];
	
	char *Weekday = (char*)atwd_integer_to_alpha3(Time_struct.tm_wday);
	Output[0] = Weekday[0]; Output[1] = Weekday[1]; Output[2] = Weekday[2]; Output[3] = ' ';
	char *Month = (char*)atmn_integer_to_alpha3(Time_struct.tm_mon);
	Output[4] = Month[0]; Output[5] = Month[1]; Output[6] = Month[2]; Output[7] = ' ';
	char *Day; acnv_integer_to_zstring(Time_struct.tm_mday,&Day,2);
	Output[8] = Day[0]; Output[9] = Day[1]; Output[10] = ' ';
	char *Hours; acnv_integer_to_zstring(Time_struct.tm_hour,&Hours,2);
	Output[11] = Hours[0]; Output[12] = Hours[1]; Output[13] = ':';
	char *Minutes; acnv_integer_to_zstring(Time_struct.tm_min,&Minutes,2);
	Output[14] = Minutes[0]; Output[15] = Minutes[1]; Output[16] = ':';
	char *Seconds; acnv_integer_to_zstring(Time_struct.tm_sec,&Seconds,2);
	Output[17] = Seconds[0]; Output[18] = Seconds[1]; Output[19] = ' ';
	char *Year; acnv_integer_to_zstring(Time_struct.tm_year + 1900,&Year,4);
	Output[20] = Year[0]; Output[21] = Year[1]; Output[22] = Year[2]; Output[23] = Year[3]; 
	Output[24] = '\0';
	
	*ANSI = malloc(strlen(Output)*sizeof(char));
	
	if (*ANSI != NULL){
		strcpy(*ANSI,Output);
		SUCCESS;
	} else {
		FAILURE;
	}
}


// Generate ANSI string from UNIX-timestamp
int atansi_from_timestamp(long long int Timestamp,char **ANSI){
	
	time_t Time = Timestamp;
	struct tm *Time_struct = localtime(&Time);
	char ***Output = &ANSI;
	if (atansi_from_struct(*Time_struct,*Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}