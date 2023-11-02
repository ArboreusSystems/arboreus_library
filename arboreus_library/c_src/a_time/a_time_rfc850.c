/* -------------------------------------------------------------------
 *  @doc Arboreus RFC850 time format handler
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/09/2019 at 19:02
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../arboreus_library/c_src/constants/a_constants_general.h"
#include "headers/a_time_rfc850.h"
#include "headers/a_time_weekday.h"
#include "headers/a_time_month.h"
#include "../a_universal/headers/a_convert.h"


// Generate the RFC850 string from tm_struct
int atRFC850FromStruct(struct tm TimeStruct, char **RFC_850){
	
	int Counter = 0;
	int Weekday_length = 0;
	char Output[45];
	
	char *Weekday = (char*) atwdIntegerToFull(TimeStruct.tm_wday);
	Weekday_length = (int)strlen(Weekday);
	for (;Weekday_length > 0;--Weekday_length,++Counter){
		Output[Counter] = Weekday[Counter];
	}
	Output[Counter++] = ','; Output[Counter++] = ' ';
	char *Day;
	acnvIntegerToZString(TimeStruct.tm_mday, &Day, 2);
	Output[Counter++] = Day[0]; Output[Counter++] = Day[1]; Output[Counter++] = '-';
	char *Month = (char*) atmnIntegerToAlpha3(TimeStruct.tm_mon);
	Output[Counter++] = Month[0]; Output[Counter++] = Month[1]; Output[Counter++] = Month[2]; Output[Counter++] = '-';
	char *Year;
	acnvIntegerToZString((TimeStruct.tm_year + 1900) % 100, &Year, 2);
	Output[Counter++] = Year[0]; Output[Counter++] = Year[1]; Output[Counter++] = ' ';
	char *Hours;
	acnvIntegerToZString(TimeStruct.tm_hour, &Hours, 2);
	Output[Counter++] = Hours[0]; Output[Counter++] = Hours[1]; Output[Counter++] = ':';
	char *Minutes;
	acnvIntegerToZString(TimeStruct.tm_min, &Minutes, 2);
	Output[Counter++] = Minutes[0]; Output[Counter++] = Minutes[1]; Output[Counter++] = ':';
	char *Seconds;
	acnvIntegerToZString(TimeStruct.tm_sec, &Seconds, 2);
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
int atRFC850FromTimestamp(long long int Timestamp, char **RFC_850){
	
	time_t Time = Timestamp;
	struct tm *Time_struct = localtime(&Time);
	char ***Output = &RFC_850;
	if (atRFC850FromStruct(*Time_struct, *Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}
