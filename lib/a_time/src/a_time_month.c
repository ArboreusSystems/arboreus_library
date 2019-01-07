// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus month name handler
//
// @end
// Created : 12/24/2018 at 18:17
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_month.h"
#include "../../universal_c/src/headers/a_convert.h"


// Return month alpha3 from integer
const char *atmn_integer_to_alpha3(int Month){
	
	if (Month == 1) {return "Jan";} else
	if (Month == 2) {return "Feb";} else
	if (Month == 3) {return "Mar";} else
	if (Month == 4) {return "Apr";} else
	if (Month == 5) {return "May";} else
	if (Month == 6) {return "Jun";} else
	if (Month == 7) {return "Jul";} else
	if (Month == 8) {return "Aug";} else
	if (Month == 9) {return "Sep";} else
	if (Month == 10) {return "Oct";} else
	if (Month == 11) {return "Nov";} else
	if (Month == 12) {return "Dec";} else
	{return "Not";}
};


// Return month alpha2 from integer
const char *atmn_integer_to_alpha2(int Month){
	
	if (Month == 1) {return "Ja";} else
	if (Month == 2) {return "Fb";} else
	if (Month == 3) {return "Mr";} else
	if (Month == 4) {return "Ap";} else
	if (Month == 5) {return "Ma";} else
	if (Month == 6) {return "Jn";} else
	if (Month == 7) {return "Jl";} else
	if (Month == 8) {return "Ag";} else
	if (Month == 9) {return "Sp";} else
	if (Month == 10) {return "Oc";} else
	if (Month == 11) {return "No";} else
	if (Month == 12) {return "De";} else
	{return "No";}
};


// Return month full from integer
const char *atmn_integer_to_full(int Month){
	
	if (Month == 1) {return "January";} else
	if (Month == 2) {return "February";} else
	if (Month == 3) {return "March";} else
	if (Month == 4) {return "April";} else
	if (Month == 5) {return "May";} else
	if (Month == 6) {return "June";} else
	if (Month == 7) {return "July";} else
	if (Month == 8) {return "August";} else
	if (Month == 9) {return "September";} else
	if (Month == 10) {return "October";} else
	if (Month == 11) {return "November";} else
	if (Month == 12) {return "December";} else
	{return "Notamonth";}
};


// Return month numeric from integer
const char *atmn_integer_to_numeric(int Month){
	
	if (Month > 12){return "00";}
	if (Month < 1){return "00";}
	
	char *Numeric;
	
	if (acnv_integer_to_zstring(Month,&Numeric,2) == EXIT_SUCCESS){
		return Numeric;
	} else {
		return "00";
	}
};