// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboeus library testing procedures: month name testing
//
// @end
// Created : 01/07/2019 at 12:38
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Application includes
#include "../../../constants/constants_general.h"
#include "headers/a_test_time_month.h"
#include "../../../a_time/src/headers/a_time_month.h"


// Test month name from numeric
int atmn_month_from_numeric(void){
	
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_JANUARY) != A_MONTH_INT_JANUARY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_FEBRUARY) != A_MONTH_INT_FEBRUARY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_MARCH) != A_MONTH_INT_MARCH){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_MARCH".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_APRIL) != A_MONTH_INT_APRIL){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_APRIL".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_MAY) != A_MONTH_INT_MAY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JUNE".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_JUNE) != A_MONTH_INT_JUNE){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_JULY) != A_MONTH_INT_JULY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_AUGUST) != A_MONTH_INT_AUGUST){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_SEPTEMBER) != A_MONTH_INT_SEPTEMBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_OCTOBER) != A_MONTH_INT_OCTOBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_NOVEMBER) != A_MONTH_INT_NOVEMBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_DECEMBER) != A_MONTH_INT_DECEMBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (atmn_numeric_to_integer(A_MONTH_NUMERIC_NOTAMONTH) != A_MONTH_INT_NOTAMONTH){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA2_JANUARY,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_JANUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_FEBRUARY,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_MARCH,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_MARCH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA2_APRIL,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_APRIL)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA2_MAY,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_MAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA2_JUNE,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_JUNE)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA2_JULY,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_JULY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA2_AUGUST,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_AUGUST)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA2_SEPTEMBER,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_OCTOBER,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_OCTOBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOVEMBER,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_DECEMBER,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_DECEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOTAMONTH,atmn_numeric_to_alpha2(A_MONTH_NUMERIC_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA3_JANUARY,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_JANUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_FEBRUARY,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_MARCH,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_MARCH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA3_APRIL,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_APRIL)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_MAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA3_JUNE,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_JUNE)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_JULY,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_JULY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_AUGUST,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_AUGUST)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA3_SEPTEMBER,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_OCTOBER,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_OCTOBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOVEMBER,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_DECEMBER,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_DECEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOTAMONTH,atmn_numeric_to_alpha3(A_MONTH_NUMERIC_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_FULL_JANUARY,atmn_numeric_to_full(A_MONTH_NUMERIC_JANUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY,atmn_numeric_to_full(A_MONTH_NUMERIC_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH,atmn_numeric_to_full(A_MONTH_NUMERIC_MARCH)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL,atmn_numeric_to_full(A_MONTH_NUMERIC_APRIL)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_FULL_MAY,atmn_numeric_to_full(A_MONTH_NUMERIC_MAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE,atmn_numeric_to_full(A_MONTH_NUMERIC_JUNE)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY,atmn_numeric_to_full(A_MONTH_NUMERIC_JULY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST,atmn_numeric_to_full(A_MONTH_NUMERIC_AUGUST)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER,atmn_numeric_to_full(A_MONTH_NUMERIC_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER,atmn_numeric_to_full(A_MONTH_NUMERIC_OCTOBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER,atmn_numeric_to_full(A_MONTH_NUMERIC_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER,atmn_numeric_to_full(A_MONTH_NUMERIC_DECEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH,atmn_numeric_to_full(A_MONTH_NUMERIC_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};


// Test month name from alpha2
int atmn_month_from_alpha2(void){
	
	
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_JANUARY) != A_MONTH_INT_JANUARY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_FEBRUARY) != A_MONTH_INT_FEBRUARY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_MARCH) != A_MONTH_INT_MARCH){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_MARCH".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_APRIL) != A_MONTH_INT_APRIL){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_APRIL".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_MAY) != A_MONTH_INT_MAY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_MAY".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_JUNE) != A_MONTH_INT_JUNE){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_JUNE".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_JULY) != A_MONTH_INT_JULY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_AUGUST) != A_MONTH_INT_AUGUST){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_SEPTEMBER) != A_MONTH_INT_SEPTEMBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_OCTOBER) != A_MONTH_INT_OCTOBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_NOVEMBER) != A_MONTH_INT_NOVEMBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_DECEMBER) != A_MONTH_INT_DECEMBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (atmn_alpha2_to_integer(A_MONTH_ALPHA2_NOTAMONTH) != A_MONTH_INT_NOTAMONTH){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_NUMERIC_JANUARY,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_FEBRUARY,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_MARCH,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_NUMERIC_APRIL,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_NUMERIC_MAY,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_MAY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_NUMERIC_JUNE,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_NUMERIC_JULY,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_JULY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_NUMERIC_AUGUST,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_NUMERIC_SEPTEMBER,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_OCTOBER,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOVEMBER,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_DECEMBER,atmn_alpha2_to_numeric(A_MONTH_ALPHA2_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOTAMONTH,atmn_alpha2_to_numeric(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_FULL_JANUARY,atmn_alpha2_to_full(A_MONTH_ALPHA2_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY,atmn_alpha2_to_full(A_MONTH_ALPHA2_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH,atmn_alpha2_to_full(A_MONTH_ALPHA2_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL,atmn_alpha2_to_full(A_MONTH_ALPHA2_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_FULL_MAY,atmn_alpha2_to_full(A_MONTH_ALPHA2_MAY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE,atmn_alpha2_to_full(A_MONTH_ALPHA2_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY,atmn_alpha2_to_full(A_MONTH_ALPHA2_JULY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST,atmn_alpha2_to_full(A_MONTH_ALPHA2_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER,atmn_alpha2_to_full(A_MONTH_ALPHA2_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER,atmn_alpha2_to_full(A_MONTH_ALPHA2_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER,atmn_alpha2_to_full(A_MONTH_ALPHA2_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER,atmn_alpha2_to_full(A_MONTH_ALPHA2_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH,atmn_alpha2_to_full(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA3_JANUARY,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_FEBRUARY,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_MARCH,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA3_APRIL,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_MAY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA3_JUNE,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA3_JULY,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_JULY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_AUGUST,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA3_SEPTEMBER,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_OCTOBER,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOVEMBER,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_DECEMBER,atmn_alpha2_to_alpha3(A_MONTH_ALPHA2_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOTAMONTH,atmn_alpha2_to_alpha3(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};


// Test month name from alpha3
int atmn_month_from_alpha3(void){
	
	if (strcmp(A_MONTH_NUMERIC_JANUARY,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_FEBRUARY,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_MARCH,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_NUMERIC_APRIL,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_NUMERIC_MAY,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_MAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_NUMERIC_JUNE,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_NUMERIC_JULY,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_JULY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_NUMERIC_AUGUST,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_NUMERIC_SEPTEMBER,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_OCTOBER,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOVEMBER,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_DECEMBER,atmn_alpha3_to_numeric(A_MONTH_ALPHA3_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOTAMONTH,atmn_alpha3_to_numeric(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_FULL_JANUARY,atmn_alpha3_to_full(A_MONTH_ALPHA3_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY,atmn_alpha3_to_full(A_MONTH_ALPHA3_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH,atmn_alpha3_to_full(A_MONTH_ALPHA3_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL,atmn_alpha3_to_full(A_MONTH_ALPHA3_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY,atmn_alpha3_to_full(A_MONTH_ALPHA3_MAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE,atmn_alpha3_to_full(A_MONTH_ALPHA3_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY,atmn_alpha3_to_full(A_MONTH_ALPHA3_JULY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST,atmn_alpha3_to_full(A_MONTH_ALPHA3_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER,atmn_alpha3_to_full(A_MONTH_ALPHA3_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER,atmn_alpha3_to_full(A_MONTH_ALPHA3_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER,atmn_alpha3_to_full(A_MONTH_ALPHA3_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER,atmn_alpha3_to_full(A_MONTH_ALPHA3_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH,atmn_alpha3_to_full(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA2_JANUARY,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_FEBRUARY,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_MARCH,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA2_APRIL,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA2_MAY,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_MAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA2_JUNE,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA2_JULY,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_JULY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA2_AUGUST,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA2_SEPTEMBER,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_OCTOBER,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOVEMBER,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_DECEMBER,atmn_alpha3_to_alpha2(A_MONTH_ALPHA3_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOTAMONTH,atmn_alpha3_to_alpha2(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_JANUARY) != A_MONTH_INT_JANUARY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_FEBRUARY) != A_MONTH_INT_FEBRUARY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_MARCH) != A_MONTH_INT_MARCH){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_MARCH".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_APRIL) != A_MONTH_INT_APRIL){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_APRIL".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_MAY) != A_MONTH_INT_MAY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_MAY".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_JUNE) != A_MONTH_INT_JUNE){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_JUNE".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_JULY) != A_MONTH_INT_JULY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_AUGUST) != A_MONTH_INT_AUGUST){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_SEPTEMBER) != A_MONTH_INT_SEPTEMBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_OCTOBER) != A_MONTH_INT_OCTOBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_NOVEMBER) != A_MONTH_INT_NOVEMBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_DECEMBER) != A_MONTH_INT_DECEMBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (atmn_alpha3_to_integer(A_MONTH_ALPHA3_NOTAMONTH) != A_MONTH_INT_NOTAMONTH){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};


// Test month name from integer
int atmn_month_from_integer(void){
	
	if (strcmp(A_MONTH_FULL_JANUARY,atmn_integer_to_full(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY,atmn_integer_to_full(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH,atmn_integer_to_full(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL,atmn_integer_to_full(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_FULL_MAY,atmn_integer_to_full(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE,atmn_integer_to_full(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY,atmn_integer_to_full(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST,atmn_integer_to_full(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER,atmn_integer_to_full(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER,atmn_integer_to_full(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER,atmn_integer_to_full(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER,atmn_integer_to_full(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH,atmn_integer_to_full(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA3_JANUARY,atmn_integer_to_alpha3(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_FEBRUARY,atmn_integer_to_alpha3(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_MARCH,atmn_integer_to_alpha3(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA3_APRIL,atmn_integer_to_alpha3(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY,atmn_integer_to_alpha3(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA3_JUNE,atmn_integer_to_alpha3(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA3_JULY,atmn_integer_to_alpha3(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_AUGUST,atmn_integer_to_alpha3(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA3_SEPTEMBER,atmn_integer_to_alpha3(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_OCTOBER,atmn_integer_to_alpha3(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOVEMBER,atmn_integer_to_alpha3(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_DECEMBER,atmn_integer_to_alpha3(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOTAMONTH,atmn_integer_to_alpha3(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA2_JANUARY,atmn_integer_to_alpha2(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_FEBRUARY,atmn_integer_to_alpha2(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_MARCH,atmn_integer_to_alpha2(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA2_APRIL,atmn_integer_to_alpha2(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA2_MAY,atmn_integer_to_alpha2(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA2_JUNE,atmn_integer_to_alpha2(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA2_JULY,atmn_integer_to_alpha2(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA2_AUGUST,atmn_integer_to_alpha2(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA2_SEPTEMBER,atmn_integer_to_alpha2(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_OCTOBER,atmn_integer_to_alpha2(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOVEMBER,atmn_integer_to_alpha2(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_DECEMBER,atmn_integer_to_alpha2(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOTAMONTH,atmn_integer_to_alpha2(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_NUMERIC_JANUARY,atmn_integer_to_numeric(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_FEBRUARY,atmn_integer_to_numeric(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_MARCH,atmn_integer_to_numeric(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_NUMERIC_APRIL,atmn_integer_to_numeric(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_NUMERIC_MAY,atmn_integer_to_numeric(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_NUMERIC_JUNE,atmn_integer_to_numeric(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_NUMERIC_JULY,atmn_integer_to_numeric(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_NUMERIC_AUGUST,atmn_integer_to_numeric(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_NUMERIC_SEPTEMBER,atmn_integer_to_numeric(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_OCTOBER,atmn_integer_to_numeric(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOVEMBER,atmn_integer_to_numeric(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_DECEMBER,atmn_integer_to_numeric(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOTAMONTH,atmn_integer_to_numeric(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};