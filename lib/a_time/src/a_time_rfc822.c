// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus RFC822 time format handler
//
// @end
// Created : 01/07/2019 at 16:44
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_rfc822.h"
#include "headers/a_time_weekday.h"
#include "headers/a_time_month.h"
#include "../../universal_c/src/headers/a_convert.h"


// Generate the RFC822 string from tm_struct
int atrfc_822_from_struct(struct tm Time_struct,char **RFC_822){
	
	char Output[30];
	
	char *Weekday = (char*)atwd_integer_to_alpha3(Time_struct.tm_wday);
	Output[0] = Weekday[0]; Output[1] = Weekday[1]; Output[2] = Weekday[2];
	Output[3] = ','; Output[4] = ' ';
	char *Day; acnv_integer_to_zstring(Time_struct.tm_mday,&Day,2);
	Output[5] = Day[0]; Output[6] = Day[1]; Output[7] = ' ';
	char *Month = (char*)atmn_integer_to_alpha3(Time_struct.tm_mon);
	Output[8] = Month[0]; Output[9] = Month[1]; Output[10] = Month[2]; Output[11] = ' ';
	char *Year; acnv_integer_to_zstring(Time_struct.tm_year + 1900,&Year,4);
	Output[12] = Year[0]; Output[13] = Year[1]; Output[14] = Year[2]; Output[15] = Year[3]; Output[16] = ' ';
	char *Hours; acnv_integer_to_zstring(Time_struct.tm_hour,&Hours,2);
	Output[17] = Hours[0]; Output[18] = Hours[1]; Output[19] = ':';
	char *Minutes; acnv_integer_to_zstring(Time_struct.tm_min,&Minutes,2);
	Output[20] = Minutes[0]; Output[21] = Minutes[1]; Output[22] = ':';
	char *Seconds; acnv_integer_to_zstring(Time_struct.tm_sec,&Seconds,2);
	Output[23] = Seconds[0]; Output[24] = Seconds[1]; Output[25] = ' ';
	Output[26] = 'G'; Output[27] = 'M'; Output[28] = 'T';
	Output[29] = '\0';
	
	*RFC_822 = malloc(strlen(Output)*sizeof(char));
	
	if (*RFC_822 != NULL){
		strcpy(*RFC_822,Output);
		SUCCESS;
	} else {
		FAILURE;
	}
}


// Generate RFC822 string from UNIX-timestamp
int atrfc_822_from_timestamp(long long int Timestamp,char **RFC_822){

	time_t Time = Timestamp;
	struct tm *Time_struct = localtime(&Time);
	char ***Output = &RFC_822;
	if (atrfc_822_from_struct(*Time_struct,*Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}