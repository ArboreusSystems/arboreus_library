/* -------------------------------------------------------------------
 *  @doc Arboreus RFC822 time format handler
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/07/2019 at 16:44
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../../arboreus_library/c_src/constants/a_constants_general.h"
#include "headers/a_time_rfc822.h"
#include "headers/a_time_weekday.h"
#include "headers/a_time_month.h"
#include "../a_universal/headers/a_convert.h"


// Generate the RFC822 string from tm_struct
int atRFC822FromStruct(struct tm TimeStruct, char **RFC_822){
	
	char Output[30];
	
	char *Weekday = (char*) atwdIntegerToAlpha3(TimeStruct.tm_wday);
	Output[0] = Weekday[0]; Output[1] = Weekday[1]; Output[2] = Weekday[2];
	Output[3] = ','; Output[4] = ' ';
	char *Day;
	acnvIntegerToZString(TimeStruct.tm_mday, &Day, 2);
	Output[5] = Day[0]; Output[6] = Day[1]; Output[7] = ' ';
	char *Month = (char*) atmnIntegerToAlpha3(TimeStruct.tm_mon);
	Output[8] = Month[0]; Output[9] = Month[1]; Output[10] = Month[2]; Output[11] = ' ';
	char *Year;
	acnvIntegerToZString(TimeStruct.tm_year + 1900, &Year, 4);
	Output[12] = Year[0]; Output[13] = Year[1]; Output[14] = Year[2]; Output[15] = Year[3]; Output[16] = ' ';
	char *Hours;
	acnvIntegerToZString(TimeStruct.tm_hour, &Hours, 2);
	Output[17] = Hours[0]; Output[18] = Hours[1]; Output[19] = ':';
	char *Minutes;
	acnvIntegerToZString(TimeStruct.tm_min, &Minutes, 2);
	Output[20] = Minutes[0]; Output[21] = Minutes[1]; Output[22] = ':';
	char *Seconds;
	acnvIntegerToZString(TimeStruct.tm_sec, &Seconds, 2);
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
int atRFC822FromTimestamp(long long int Timestamp, char **RFC822){

	time_t Time = Timestamp;
	struct tm *Time_struct = localtime(&Time);
	char ***Output = &RFC822;
	if (atRFC822FromStruct(*Time_struct, *Output) != EXIT_SUCCESS){FAILURE;}
	SUCCESS;
}
