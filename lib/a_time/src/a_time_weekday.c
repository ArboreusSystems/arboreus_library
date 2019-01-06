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


// Return weekday in full format
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

// Return weekday in numeric format
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
