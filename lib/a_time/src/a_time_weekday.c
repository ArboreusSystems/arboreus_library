// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus weekday name handler
//
// @end
// Created : 12/24/2018 at 19:10
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_weekday.h"
#include "../../universal_c/src/headers/a_convert.h"


// Return weekday in Alpha3 format
const char *atwd_integer_to_alpha3(int Month){
	
	if (Month == A_WEEKDAY_INT_MONDAY){return A_WEEKDAY_ALPHA3_MONDAY;}
	if (Month == A_WEEKDAY_INT_TUESDAY){return A_WEEKDAY_ALPHA3_TUESDAY;}
	if (Month == A_WEEKDAY_INT_WEDNESDAY){return A_WEEKDAY_ALPHA3_WEDNESDAY;}
	if (Month == A_WEEKDAY_INT_THURSDAY){return A_WEEKDAY_ALPHA3_THURSDAY;}
	if (Month == A_WEEKDAY_INT_FRIDAY){return A_WEEKDAY_ALPHA3_FRIDAY;}
	if (Month == A_WEEKDAY_INT_SATURDAY){return A_WEEKDAY_ALPHA3_SATURDAY;}
	if (Month == A_WEEKDAY_INT_SUNDAY){return A_WEEKDAY_ALPHA3_SUNDAY;}
	return A_WEEKDAY_ALPHA3_NOTAWEEKDAY;
}


// Return weekday in Alpha2 format
const char *atwd_integer_to_alpha2(int Month){
	
	if (Month == A_WEEKDAY_INT_MONDAY){return A_WEEKDAY_ALPHA2_MONDAY;}
	if (Month == A_WEEKDAY_INT_TUESDAY){return A_WEEKDAY_ALPHA2_TUESDAY;}
	if (Month == A_WEEKDAY_INT_WEDNESDAY){return A_WEEKDAY_ALPHA2_WEDNESDAY;}
	if (Month == A_WEEKDAY_INT_THURSDAY){return A_WEEKDAY_ALPHA2_THURSDAY;}
	if (Month == A_WEEKDAY_INT_FRIDAY){return A_WEEKDAY_ALPHA2_FRIDAY;}
	if (Month == A_WEEKDAY_INT_SATURDAY){return A_WEEKDAY_ALPHA2_SATURDAY;}
	if (Month == A_WEEKDAY_INT_SUNDAY){return A_WEEKDAY_ALPHA2_SUNDAY;}
	return A_WEEKDAY_ALPHA2_NOTAWEEKDAY;
}


// Return weekday in Full format
const char *atwd_integer_to_full(int Month){
	
	if (Month == A_WEEKDAY_INT_MONDAY){return A_WEEKDAY_FULL_MONDAY;}
	if (Month == A_WEEKDAY_INT_TUESDAY){return A_WEEKDAY_FULL_TUESDAY;}
	if (Month == A_WEEKDAY_INT_WEDNESDAY){return A_WEEKDAY_FULL_WEDNESDAY;}
	if (Month == A_WEEKDAY_INT_THURSDAY){return A_WEEKDAY_FULL_THURSDAY;}
	if (Month == A_WEEKDAY_INT_FRIDAY){return A_WEEKDAY_FULL_FRIDAY;}
	if (Month == A_WEEKDAY_INT_SATURDAY){return A_WEEKDAY_FULL_SATURDAY;}
	if (Month == A_WEEKDAY_INT_SUNDAY){return A_WEEKDAY_FULL_SUNDAY;}
	return A_WEEKDAY_FULL_NOTAWEEKDAY;
}

// Return weekday in Numeric format
const char *atwd_integer_to_numeric(int Weekday){
	
	if (Weekday > A_WEEKDAY_INT_SUNDAY){return A_WEEKDAY_NUMERIC_NOTAWEEKDAY;}
	if (Weekday < A_WEEKDAY_INT_MONDAY){return A_WEEKDAY_NUMERIC_NOTAWEEKDAY;}
	
	char *Numeric;
	
	if (acnv_integer_to_zstring(Weekday,&Numeric,2) == EXIT_SUCCESS){
		return Numeric;
	} else {
		return A_WEEKDAY_NUMERIC_NOTAWEEKDAY;
	}
}


// Return weekday number from Alpha3 string
int atwd_alpha3_to_integer(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_ALPHA3_MONDAY,Pointer)){return A_WEEKDAY_INT_MONDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_TUESDAY,Pointer)){return A_WEEKDAY_INT_TUESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY,Pointer)){return A_WEEKDAY_INT_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_THURSDAY,Pointer)){return A_WEEKDAY_INT_THURSDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_FRIDAY,Pointer)){return A_WEEKDAY_INT_FRIDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SATURDAY,Pointer)){return A_WEEKDAY_INT_SATURDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SUNDAY,Pointer)){return A_WEEKDAY_INT_SUNDAY;}
	return A_WEEKDAY_INT_NOTAWEEKDAY;
}


// Return weekday Alpha2 from Alpha3
const char *atwd_alpha3_to_alpha2(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_ALPHA3_MONDAY,Pointer)){return A_WEEKDAY_ALPHA2_MONDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_TUESDAY,Pointer)){return A_WEEKDAY_ALPHA2_TUESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY,Pointer)){return A_WEEKDAY_ALPHA2_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_THURSDAY,Pointer)){return A_WEEKDAY_ALPHA2_THURSDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_FRIDAY,Pointer)){return A_WEEKDAY_ALPHA2_FRIDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SATURDAY,Pointer)){return A_WEEKDAY_ALPHA2_SATURDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SUNDAY,Pointer)){return A_WEEKDAY_ALPHA2_SUNDAY;}
	return A_WEEKDAY_ALPHA2_NOTAWEEKDAY;
}


// Return weekday Full from Alpha3
const char *atwd_alpha3_to_full(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_ALPHA3_MONDAY,Pointer)){return A_WEEKDAY_FULL_MONDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_TUESDAY,Pointer)){return A_WEEKDAY_FULL_TUESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY,Pointer)){return A_WEEKDAY_FULL_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_THURSDAY,Pointer)){return A_WEEKDAY_FULL_THURSDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_FRIDAY,Pointer)){return A_WEEKDAY_FULL_FRIDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SATURDAY,Pointer)){return A_WEEKDAY_FULL_SATURDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SUNDAY,Pointer)){return A_WEEKDAY_FULL_SUNDAY;}
	return A_WEEKDAY_FULL_NOTAWEEKDAY;
}


// Return weekday Numeric from Alpha3
const char *atwd_alpha3_to_numeric(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_ALPHA3_MONDAY,Pointer)){return A_WEEKDAY_NUMERIC_MONDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_TUESDAY,Pointer)){return A_WEEKDAY_NUMERIC_TUESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_WEDNESDAY,Pointer)){return A_WEEKDAY_NUMERIC_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_THURSDAY,Pointer)){return A_WEEKDAY_NUMERIC_THURSDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_FRIDAY,Pointer)){return A_WEEKDAY_NUMERIC_FRIDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SATURDAY,Pointer)){return A_WEEKDAY_NUMERIC_SATURDAY;}
	if (!strcmp(A_WEEKDAY_ALPHA3_SUNDAY,Pointer)){return A_WEEKDAY_NUMERIC_SUNDAY;}
	return A_WEEKDAY_NUMERIC_NOTAWEEKDAY;
}


// Return weekday Alpha3 from Full
const char *atwd_full_to_alpha3(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_FULL_MONDAY,Pointer)){return A_WEEKDAY_ALPHA3_MONDAY;}
	if (!strcmp(A_WEEKDAY_FULL_TUESDAY,Pointer)){return A_WEEKDAY_ALPHA3_TUESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_WEDNESDAY,Pointer)){return A_WEEKDAY_ALPHA3_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_THURSDAY,Pointer)){return A_WEEKDAY_ALPHA3_THURSDAY;}
	if (!strcmp(A_WEEKDAY_FULL_FRIDAY,Pointer)){return A_WEEKDAY_ALPHA3_FRIDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SATURDAY,Pointer)){return A_WEEKDAY_ALPHA3_SATURDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SUNDAY,Pointer)){return A_WEEKDAY_ALPHA3_SUNDAY;}
	return A_WEEKDAY_ALPHA3_NOTAWEEKDAY;
}


// Return weekday Alpha3 from Full
const char *atwd_full_to_alpha2(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_FULL_MONDAY,Pointer)){return A_WEEKDAY_ALPHA2_MONDAY;}
	if (!strcmp(A_WEEKDAY_FULL_TUESDAY,Pointer)){return A_WEEKDAY_ALPHA2_TUESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_WEDNESDAY,Pointer)){return A_WEEKDAY_ALPHA2_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_THURSDAY,Pointer)){return A_WEEKDAY_ALPHA2_THURSDAY;}
	if (!strcmp(A_WEEKDAY_FULL_FRIDAY,Pointer)){return A_WEEKDAY_ALPHA2_FRIDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SATURDAY,Pointer)){return A_WEEKDAY_ALPHA2_SATURDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SUNDAY,Pointer)){return A_WEEKDAY_ALPHA2_SUNDAY;}
	return A_WEEKDAY_ALPHA2_NOTAWEEKDAY;
}


// Return weekday number from full name string
int atwd_full_to_integer(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_FULL_MONDAY,Pointer)){return A_WEEKDAY_INT_MONDAY;}
	if (!strcmp(A_WEEKDAY_FULL_TUESDAY,Pointer)){return A_WEEKDAY_INT_TUESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_WEDNESDAY,Pointer)){return A_WEEKDAY_INT_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_THURSDAY,Pointer)){return A_WEEKDAY_INT_THURSDAY;}
	if (!strcmp(A_WEEKDAY_FULL_FRIDAY,Pointer)){return A_WEEKDAY_INT_FRIDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SATURDAY,Pointer)){return A_WEEKDAY_INT_SATURDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SUNDAY,Pointer)){return A_WEEKDAY_INT_SUNDAY;}
	return A_WEEKDAY_INT_NOTAWEEKDAY;
}


// Return weekday numeric from full
const char *atwd_full_to_numeric(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_FULL_MONDAY,Pointer)){return A_WEEKDAY_NUMERIC_MONDAY;}
	if (!strcmp(A_WEEKDAY_FULL_TUESDAY,Pointer)){return A_WEEKDAY_NUMERIC_TUESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_WEDNESDAY,Pointer)){return A_WEEKDAY_NUMERIC_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_FULL_THURSDAY,Pointer)){return A_WEEKDAY_NUMERIC_THURSDAY;}
	if (!strcmp(A_WEEKDAY_FULL_FRIDAY,Pointer)){return A_WEEKDAY_NUMERIC_FRIDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SATURDAY,Pointer)){return A_WEEKDAY_NUMERIC_SATURDAY;}
	if (!strcmp(A_WEEKDAY_FULL_SUNDAY,Pointer)){return A_WEEKDAY_NUMERIC_SUNDAY;}
	return A_WEEKDAY_NUMERIC_NOTAWEEKDAY;
}


// Return weekday integer from numeric
int atwd_numeric_to_integer(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_NUMERIC_MONDAY,Pointer)){return A_WEEKDAY_INT_MONDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_TUESDAY,Pointer)){return A_WEEKDAY_INT_TUESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY,Pointer)){return A_WEEKDAY_INT_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_THURSDAY,Pointer)){return A_WEEKDAY_INT_THURSDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_FRIDAY,Pointer)){return A_WEEKDAY_INT_FRIDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SATURDAY,Pointer)){return A_WEEKDAY_INT_SATURDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SUNDAY,Pointer)){return A_WEEKDAY_INT_SUNDAY;}
	return A_WEEKDAY_INT_NOTAWEEKDAY;
}


// Return weekday alpha3 from numeric
const char *atwd_numeric_to_alpha3(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_NUMERIC_MONDAY,Pointer)){return A_WEEKDAY_ALPHA3_MONDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_TUESDAY,Pointer)){return A_WEEKDAY_ALPHA3_TUESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY,Pointer)){return A_WEEKDAY_ALPHA3_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_THURSDAY,Pointer)){return A_WEEKDAY_ALPHA3_THURSDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_FRIDAY,Pointer)){return A_WEEKDAY_ALPHA3_FRIDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SATURDAY,Pointer)){return A_WEEKDAY_ALPHA3_SATURDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SUNDAY,Pointer)){return A_WEEKDAY_ALPHA3_SUNDAY;}
	return A_WEEKDAY_ALPHA3_NOTAWEEKDAY;
}


// Return weekday alpha2 from numeric
const char *atwd_numeric_to_alpha2(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_NUMERIC_MONDAY,Pointer)){return A_WEEKDAY_ALPHA2_MONDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_TUESDAY,Pointer)){return A_WEEKDAY_ALPHA2_TUESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY,Pointer)){return A_WEEKDAY_ALPHA2_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_THURSDAY,Pointer)){return A_WEEKDAY_ALPHA2_THURSDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_FRIDAY,Pointer)){return A_WEEKDAY_ALPHA2_FRIDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SATURDAY,Pointer)){return A_WEEKDAY_ALPHA2_SATURDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SUNDAY,Pointer)){return A_WEEKDAY_ALPHA2_SUNDAY;}
	return A_WEEKDAY_ALPHA2_NOTAWEEKDAY;
}


// Return weekday full from numeric
const char *atwd_numeric_to_full(char *Pointer){
	
	if (!strcmp(A_WEEKDAY_NUMERIC_MONDAY,Pointer)){return A_WEEKDAY_FULL_MONDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_TUESDAY,Pointer)){return A_WEEKDAY_FULL_TUESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_WEDNESDAY,Pointer)){return A_WEEKDAY_FULL_WEDNESDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_THURSDAY,Pointer)){return A_WEEKDAY_FULL_THURSDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_FRIDAY,Pointer)){return A_WEEKDAY_FULL_FRIDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SATURDAY,Pointer)){return A_WEEKDAY_FULL_SATURDAY;}
	if (!strcmp(A_WEEKDAY_NUMERIC_SUNDAY,Pointer)){return A_WEEKDAY_FULL_SUNDAY;}
	return A_WEEKDAY_FULL_NOTAWEEKDAY;
}