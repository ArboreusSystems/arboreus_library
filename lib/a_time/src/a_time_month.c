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


// Return integer month from alpha2
int atmn_alpha2_to_integer(char *Alpha2){
	
	if (!strcmp("Ja",Alpha2)){return 1;}
	if (!strcmp("Fb",Alpha2)){return 2;}
	if (!strcmp("Mr",Alpha2)){return 3;}
	if (!strcmp("Ap",Alpha2)){return 4;}
	if (!strcmp("Ma",Alpha2)){return 5;}
	if (!strcmp("Jn",Alpha2)){return 6;}
	if (!strcmp("Jl",Alpha2)){return 7;}
	if (!strcmp("Ag",Alpha2)){return 8;}
	if (!strcmp("Sp",Alpha2)){return 9;}
	if (!strcmp("Oc",Alpha2)){return 10;}
	if (!strcmp("No",Alpha2)){return 11;}
	if (!strcmp("De",Alpha2)){return 12;}
	return -1;
}


// Return alpha3 month from alpha2
const char *atmn_alpha2_to_alpha3(char *Alpha2){
	
	if (!strcmp("Ja",Alpha2)){return "Jan";}
	if (!strcmp("Fb",Alpha2)){return "Feb";}
	if (!strcmp("Mr",Alpha2)){return "Mar";}
	if (!strcmp("Ap",Alpha2)){return "Apr";}
	if (!strcmp("Ma",Alpha2)){return "May";}
	if (!strcmp("Jn",Alpha2)){return "Jun";}
	if (!strcmp("Jl",Alpha2)){return "Jul";}
	if (!strcmp("Ag",Alpha2)){return "Aug";}
	if (!strcmp("Sp",Alpha2)){return "Sep";}
	if (!strcmp("Oc",Alpha2)){return "Oct";}
	if (!strcmp("No",Alpha2)){return "Nov";}
	if (!strcmp("De",Alpha2)){return "Dec";}
	return "Not";
}


// Return full month from alpha2
const char *atmn_alpha2_to_full(char *Alpha2){
	
	if (!strcmp("Ja",Alpha2)){return "January";}
	if (!strcmp("Fb",Alpha2)){return "February";}
	if (!strcmp("Mr",Alpha2)){return "March";}
	if (!strcmp("Ap",Alpha2)){return "April";}
	if (!strcmp("Ma",Alpha2)){return "May";}
	if (!strcmp("Jn",Alpha2)){return "June";}
	if (!strcmp("Jl",Alpha2)){return "July";}
	if (!strcmp("Ag",Alpha2)){return "August";}
	if (!strcmp("Sp",Alpha2)){return "September";}
	if (!strcmp("Oc",Alpha2)){return "October";}
	if (!strcmp("No",Alpha2)){return "November";}
	if (!strcmp("De",Alpha2)){return "December";}
	return "Notamonth";
}


// Return numeric month from alpha2
const char *atmn_alpha2_to_numeric(char *Alpha2){
	
	if (!strcmp("Ja",Alpha2)){return "01";}
	if (!strcmp("Fb",Alpha2)){return "02";}
	if (!strcmp("Mr",Alpha2)){return "03";}
	if (!strcmp("Ap",Alpha2)){return "04";}
	if (!strcmp("Ma",Alpha2)){return "05";}
	if (!strcmp("Jn",Alpha2)){return "06";}
	if (!strcmp("Jl",Alpha2)){return "07";}
	if (!strcmp("Ag",Alpha2)){return "08";}
	if (!strcmp("Sp",Alpha2)){return "09";}
	if (!strcmp("Oc",Alpha2)){return "10";}
	if (!strcmp("No",Alpha2)){return "11";}
	if (!strcmp("De",Alpha2)){return "12";}
	return "00";
}


// Return month integer from alpha3
int atmn_alpha3_to_integer(char *Alpha3){
	
	if (!strcmp("Jan",Alpha3)){return 1;}
	if (!strcmp("Feb",Alpha3)){return 2;}
	if (!strcmp("Mar",Alpha3)){return 3;}
	if (!strcmp("Apr",Alpha3)){return 4;}
	if (!strcmp("May",Alpha3)){return 5;}
	if (!strcmp("Jun",Alpha3)){return 6;}
	if (!strcmp("Jul",Alpha3)){return 7;}
	if (!strcmp("Aug",Alpha3)){return 8;}
	if (!strcmp("Sep",Alpha3)){return 9;}
	if (!strcmp("Oct",Alpha3)){return 10;}
	if (!strcmp("Nov",Alpha3)){return 11;}
	if (!strcmp("Dec",Alpha3)){return 12;}
	return -1;
}


// Return month alpha2 from alpha3
const char *atmn_alpha3_to_alpha2(char *Alpha3){
	
	if (!strcmp("Jan",Alpha3)){return "Ja";}
	if (!strcmp("Feb",Alpha3)){return "Fb";}
	if (!strcmp("Mar",Alpha3)){return "Mr";}
	if (!strcmp("Apr",Alpha3)){return "Ap";}
	if (!strcmp("May",Alpha3)){return "Ma";}
	if (!strcmp("Jun",Alpha3)){return "Jn";}
	if (!strcmp("Jul",Alpha3)){return "Jl";}
	if (!strcmp("Aug",Alpha3)){return "Ag";}
	if (!strcmp("Sep",Alpha3)){return "Sp";}
	if (!strcmp("Oct",Alpha3)){return "Oc";}
	if (!strcmp("Nov",Alpha3)){return "No";}
	if (!strcmp("Dec",Alpha3)){return "De";}
	return "No";
}


// Return month full from alpha3
const char *atmn_alpha3_to_full(char *Alpha3){
	
	if (!strcmp("Jan",Alpha3)){return "January";}
	if (!strcmp("Feb",Alpha3)){return "February";}
	if (!strcmp("Mar",Alpha3)){return "March";}
	if (!strcmp("Apr",Alpha3)){return "April";}
	if (!strcmp("May",Alpha3)){return "May";}
	if (!strcmp("Jun",Alpha3)){return "June";}
	if (!strcmp("Jul",Alpha3)){return "July";}
	if (!strcmp("Aug",Alpha3)){return "August";}
	if (!strcmp("Sep",Alpha3)){return "September";}
	if (!strcmp("Oct",Alpha3)){return "October";}
	if (!strcmp("Nov",Alpha3)){return "November";}
	if (!strcmp("Dec",Alpha3)){return "December";}
	return "Notamonth";
};


// Return month numeric from alpha3
const char *atmn_alpha3_to_numeric(char *Alpha3){
	
	if (!strcmp("Jan",Alpha3)){return "01";}
	if (!strcmp("Feb",Alpha3)){return "02";}
	if (!strcmp("Mar",Alpha3)){return "03";}
	if (!strcmp("Apr",Alpha3)){return "04";}
	if (!strcmp("May",Alpha3)){return "05";}
	if (!strcmp("Jun",Alpha3)){return "06";}
	if (!strcmp("Jul",Alpha3)){return "07";}
	if (!strcmp("Aug",Alpha3)){return "08";}
	if (!strcmp("Sep",Alpha3)){return "09";}
	if (!strcmp("Oct",Alpha3)){return "10";}
	if (!strcmp("Nov",Alpha3)){return "11";}
	if (!strcmp("Dec",Alpha3)){return "12";}
	return "00";
};


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