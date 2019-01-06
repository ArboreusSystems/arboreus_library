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
	
	if (Month == 1) {return "Mon";} else
	if (Month == 2) {return "Tue";} else
	if (Month == 3) {return "Wed";} else
	if (Month == 4) {return "Thu";} else
	if (Month == 5) {return "Fri";} else
	if (Month == 6) {return "Sat";} else
	if (Month == 7) {return "Sun";} else
	{return "Not";}
}


// Return weekday in Alpha2 format
const char *atwd_integer_to_alpha2(int Month){
	
	if (Month == 1) {return "Mo";} else
	if (Month == 2) {return "Tu";} else
	if (Month == 3) {return "We";} else
	if (Month == 4) {return "Th";} else
	if (Month == 5) {return "Fr";} else
	if (Month == 6) {return "Sa";} else
	if (Month == 7) {return "Su";} else
	{return "No";}
}


// Return weekday in Full format
const char *atwd_integer_to_full(int Month){
	
	if (Month == 1){return "Monday";} else
	if (Month == 2){return "Tuesday";} else
	if (Month == 3){return "Wednesday";} else
	if (Month == 4){return "Thursday";} else
	if (Month == 5){return "Friday";} else
	if (Month == 6){return "Saturday";} else
	if (Month == 7){return "Sunday";} else
	{return "Notaweekday";}
}

// Return weekday in Numeric format
const char *atwd_integer_to_numeric(int Month){
	
	if (Month > 7){return "00";}
	if (Month <= 0){return "00";}
	
	char *Numeric;
	
	if (!acnv_integer_to_zstring(Month,&Numeric,2)){
		return Numeric;
	} else {
		return "00";
	}
}


// Return weekday number from Alpha3 string
int atwd_alpha3_to_integer(char *Pointer){
	
	if (!strcmp("Mon",Pointer)){return 1;} else
	if (!strcmp("Tue",Pointer)){return 2;} else
	if (!strcmp("Wed",Pointer)){return 3;} else
	if (!strcmp("Thu",Pointer)){return 4;} else
	if (!strcmp("Fri",Pointer)){return 5;} else
	if (!strcmp("Sat",Pointer)){return 6;} else
	if (!strcmp("Sun",Pointer)){return 7;} else
	{return -1;}
}


// Return weekday Alpha2 from Alpha3
const char *atwd_alpha3_to_alpha2(char *Pointer){
	
	if (!strcmp("Mon",Pointer)){return "Mo";} else
	if (!strcmp("Tue",Pointer)){return "Tu";} else
	if (!strcmp("Wed",Pointer)){return "We";} else
	if (!strcmp("Thu",Pointer)){return "Th";} else
	if (!strcmp("Fri",Pointer)){return "Fr";} else
	if (!strcmp("Sat",Pointer)){return "Sa";} else
	if (!strcmp("Sun",Pointer)){return "Su";} else
	{return "No";}
}


// Return weekday Full from Alpha3
const char *atwd_alpha3_to_full(char *Pointer){
	
	if (!strcmp("Mon",Pointer)){return "Monday";} else
	if (!strcmp("Tue",Pointer)){return "Tuesday";} else
	if (!strcmp("Wed",Pointer)){return "Wednesday";} else
	if (!strcmp("Thu",Pointer)){return "Thursday";} else
	if (!strcmp("Fri",Pointer)){return "Friday";} else
	if (!strcmp("Sat",Pointer)){return "Saturday";} else
	if (!strcmp("Sun",Pointer)){return "Sunday";} else
	{return "Notaweekday";}
}


// Return weekday Numeric from Alpha3
const char *atwd_alpha3_to_numeric(char *Pointer){
	
	if (!strcmp("Mon",Pointer)){return "01";} else
	if (!strcmp("Tue",Pointer)){return "02";} else
	if (!strcmp("Wed",Pointer)){return "03";} else
	if (!strcmp("Thu",Pointer)){return "04";} else
	if (!strcmp("Fri",Pointer)){return "05";} else
	if (!strcmp("Sat",Pointer)){return "06";} else
	if (!strcmp("Sun",Pointer)){return "07";} else
	{return "00";}
}


// Return weekday Alpha3 from Full
const char *atwd_full_to_alpha3(char *Pointer){
	
	if (!strcmp("Monday",Pointer)){return "Mon";} else
	if (!strcmp("Tuesday",Pointer)){return "Tue";} else
	if (!strcmp("Wednesday",Pointer)){return "Wed";} else
	if (!strcmp("Thursday",Pointer)){return "Thu";} else
	if (!strcmp("Friday",Pointer)){return "Fri";} else
	if (!strcmp("Saturday",Pointer)){return "Sat";} else
	if (!strcmp("Sunday",Pointer)){return "Sun";} else
	{return "Not";}
}


// Return weekday Alpha3 from Full
const char *atwd_full_to_alpha2(char *Pointer){
	
	if (!strcmp("Monday",Pointer)){return "Mo";} else
	if (!strcmp("Tuesday",Pointer)){return "Tu";} else
	if (!strcmp("Wednesday",Pointer)){return "We";} else
	if (!strcmp("Thursday",Pointer)){return "Th";} else
	if (!strcmp("Friday",Pointer)){return "Fr";} else
	if (!strcmp("Saturday",Pointer)){return "Sa";} else
	if (!strcmp("Sunday",Pointer)){return "Su";} else
	{return "No";}
}


// Return weekday number from full name string
int atwd_full_to_integer(char *Pointer){
	
	if (!strcmp("Monday",Pointer)){return 1;} else
	if (!strcmp("Tuesday",Pointer)){return 2;} else
	if (!strcmp("Wednesday",Pointer)){return 3;} else
	if (!strcmp("Thursday",Pointer)){return 4;} else
	if (!strcmp("Friday",Pointer)){return 5;} else
	if (!strcmp("Saturday",Pointer)){return 6;} else
	if (!strcmp("Sunday",Pointer)){return 7;} else
	{return -1;}
}


// Return weekday numeric from full
const char *atwd_full_to_numeric(char *Pointer){
	
	if (!strcmp("Monday",Pointer)){return "01";} else
	if (!strcmp("Tuesday",Pointer)){return "02";} else
	if (!strcmp("Wednesday",Pointer)){return "03";} else
	if (!strcmp("Thursday",Pointer)){return "04";} else
	if (!strcmp("Friday",Pointer)){return "05";} else
	if (!strcmp("Saturday",Pointer)){return "06";} else
	if (!strcmp("Sunday",Pointer)){return "07";} else
	{return "00";}
}