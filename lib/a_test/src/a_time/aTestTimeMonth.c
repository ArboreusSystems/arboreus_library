/* -------------------------------------------------------------------
 *  @doc Arboeus library testing procedures: month name testing
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/07/2019 at 12:38
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Application includes
#include "../../../constants/aConstantsGeneral.h"
#include "headers/aTestTimeMonth.h"
#include "../../../a_time/src/headers/aTimeMonth.h"


// Test month name from numeric
int atmnMonthFromNumeric(void){
	
	if (atmnNumericToInteger(A_MONTH_NUMERIC_JANUARY) != A_MONTH_INT_JANUARY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_FEBRUARY) != A_MONTH_INT_FEBRUARY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_MARCH) != A_MONTH_INT_MARCH){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_MARCH".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_APRIL) != A_MONTH_INT_APRIL){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_APRIL".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_MAY) != A_MONTH_INT_MAY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JUNE".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_JUNE) != A_MONTH_INT_JUNE){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_JULY) != A_MONTH_INT_JULY){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_AUGUST) != A_MONTH_INT_AUGUST){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_SEPTEMBER) != A_MONTH_INT_SEPTEMBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_OCTOBER) != A_MONTH_INT_OCTOBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_NOVEMBER) != A_MONTH_INT_NOVEMBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_DECEMBER) != A_MONTH_INT_DECEMBER){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (atmnNumericToInteger(A_MONTH_NUMERIC_NOTAMONTH) != A_MONTH_INT_NOTAMONTH){FAILURE;}
	printf("Done! Numeric to Integer month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA2_JANUARY, atmnNumericToAlpha2(A_MONTH_NUMERIC_JANUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_FEBRUARY, atmnNumericToAlpha2(A_MONTH_NUMERIC_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_MARCH, atmnNumericToAlpha2(A_MONTH_NUMERIC_MARCH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA2_APRIL, atmnNumericToAlpha2(A_MONTH_NUMERIC_APRIL)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA2_MAY, atmnNumericToAlpha2(A_MONTH_NUMERIC_MAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA2_JUNE, atmnNumericToAlpha2(A_MONTH_NUMERIC_JUNE)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA2_JULY, atmnNumericToAlpha2(A_MONTH_NUMERIC_JULY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA2_AUGUST, atmnNumericToAlpha2(A_MONTH_NUMERIC_AUGUST)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA2_SEPTEMBER, atmnNumericToAlpha2(A_MONTH_NUMERIC_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_OCTOBER, atmnNumericToAlpha2(A_MONTH_NUMERIC_OCTOBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOVEMBER, atmnNumericToAlpha2(A_MONTH_NUMERIC_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_DECEMBER, atmnNumericToAlpha2(A_MONTH_NUMERIC_DECEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOTAMONTH, atmnNumericToAlpha2(A_MONTH_NUMERIC_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha2 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA3_JANUARY, atmnNumericToAlpha3(A_MONTH_NUMERIC_JANUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_FEBRUARY, atmnNumericToAlpha3(A_MONTH_NUMERIC_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_MARCH, atmnNumericToAlpha3(A_MONTH_NUMERIC_MARCH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA3_APRIL, atmnNumericToAlpha3(A_MONTH_NUMERIC_APRIL)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY, atmnNumericToAlpha3(A_MONTH_NUMERIC_MAY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA3_JUNE, atmnNumericToAlpha3(A_MONTH_NUMERIC_JUNE)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_JULY, atmnNumericToAlpha3(A_MONTH_NUMERIC_JULY)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_AUGUST, atmnNumericToAlpha3(A_MONTH_NUMERIC_AUGUST)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA3_SEPTEMBER, atmnNumericToAlpha3(A_MONTH_NUMERIC_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_OCTOBER, atmnNumericToAlpha3(A_MONTH_NUMERIC_OCTOBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOVEMBER, atmnNumericToAlpha3(A_MONTH_NUMERIC_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_DECEMBER, atmnNumericToAlpha3(A_MONTH_NUMERIC_DECEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOTAMONTH, atmnNumericToAlpha3(A_MONTH_NUMERIC_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Numeric to Alpha3 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_FULL_JANUARY, atmnNumericToFull(A_MONTH_NUMERIC_JANUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY, atmnNumericToFull(A_MONTH_NUMERIC_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH, atmnNumericToFull(A_MONTH_NUMERIC_MARCH)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL, atmnNumericToFull(A_MONTH_NUMERIC_APRIL)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_FULL_MAY, atmnNumericToFull(A_MONTH_NUMERIC_MAY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE, atmnNumericToFull(A_MONTH_NUMERIC_JUNE)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY, atmnNumericToFull(A_MONTH_NUMERIC_JULY)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST, atmnNumericToFull(A_MONTH_NUMERIC_AUGUST)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER, atmnNumericToFull(A_MONTH_NUMERIC_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER, atmnNumericToFull(A_MONTH_NUMERIC_OCTOBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER, atmnNumericToFull(A_MONTH_NUMERIC_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER, atmnNumericToFull(A_MONTH_NUMERIC_DECEMBER)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH, atmnNumericToFull(A_MONTH_NUMERIC_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Numeric to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};


// Test month name from alpha2
int atmnMonthFromAlpha2(void){
	
	
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_JANUARY) != A_MONTH_INT_JANUARY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_FEBRUARY) != A_MONTH_INT_FEBRUARY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_MARCH) != A_MONTH_INT_MARCH){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_MARCH".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_APRIL) != A_MONTH_INT_APRIL){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_APRIL".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_MAY) != A_MONTH_INT_MAY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_MAY".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_JUNE) != A_MONTH_INT_JUNE){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_JUNE".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_JULY) != A_MONTH_INT_JULY){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_AUGUST) != A_MONTH_INT_AUGUST){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_SEPTEMBER) != A_MONTH_INT_SEPTEMBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_OCTOBER) != A_MONTH_INT_OCTOBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_NOVEMBER) != A_MONTH_INT_NOVEMBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_DECEMBER) != A_MONTH_INT_DECEMBER){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (atmnAlpha2ToInteger(A_MONTH_ALPHA2_NOTAMONTH) != A_MONTH_INT_NOTAMONTH){FAILURE;}
	printf("Done! Alpha2 to Integer month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_NUMERIC_JANUARY, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_FEBRUARY, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_MARCH, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_NUMERIC_APRIL, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_NUMERIC_MAY, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_MAY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_NUMERIC_JUNE, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_NUMERIC_JULY, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_JULY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_NUMERIC_AUGUST, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_NUMERIC_SEPTEMBER, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_OCTOBER, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOVEMBER, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_DECEMBER, atmnAlpha2ToNumeric(A_MONTH_ALPHA2_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOTAMONTH, atmnAlpha2ToNumeric(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Numeric month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_FULL_JANUARY, atmnAlpha2ToFull(A_MONTH_ALPHA2_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY, atmnAlpha2ToFull(A_MONTH_ALPHA2_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH, atmnAlpha2ToFull(A_MONTH_ALPHA2_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL, atmnAlpha2ToFull(A_MONTH_ALPHA2_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_FULL_MAY, atmnAlpha2ToFull(A_MONTH_ALPHA2_MAY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE, atmnAlpha2ToFull(A_MONTH_ALPHA2_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY, atmnAlpha2ToFull(A_MONTH_ALPHA2_JULY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST, atmnAlpha2ToFull(A_MONTH_ALPHA2_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER, atmnAlpha2ToFull(A_MONTH_ALPHA2_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER, atmnAlpha2ToFull(A_MONTH_ALPHA2_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER, atmnAlpha2ToFull(A_MONTH_ALPHA2_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER, atmnAlpha2ToFull(A_MONTH_ALPHA2_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH, atmnAlpha2ToFull(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA3_JANUARY, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_FEBRUARY, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_MARCH, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA3_APRIL, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_MAY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA3_JUNE, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA3_JULY, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_JULY)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_AUGUST, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA3_SEPTEMBER, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_OCTOBER, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOVEMBER, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_DECEMBER, atmnAlpha2ToAlpha3(A_MONTH_ALPHA2_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOTAMONTH, atmnAlpha2ToAlpha3(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha2 to Alpha3 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};


// Test month name from alpha3
int atmnMonthFromAlpha3(void){
	
	if (strcmp(A_MONTH_NUMERIC_JANUARY, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_FEBRUARY, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_MARCH, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_NUMERIC_APRIL, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_NUMERIC_MAY, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_MAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_NUMERIC_JUNE, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_NUMERIC_JULY, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_JULY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_NUMERIC_AUGUST, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_NUMERIC_SEPTEMBER, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_OCTOBER, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOVEMBER, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_DECEMBER, atmnAlpha3ToNumeric(A_MONTH_ALPHA3_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOTAMONTH, atmnAlpha3ToNumeric(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Numeric month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_FULL_JANUARY, atmnAlpha3ToFull(A_MONTH_ALPHA3_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY, atmnAlpha3ToFull(A_MONTH_ALPHA3_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH, atmnAlpha3ToFull(A_MONTH_ALPHA3_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL, atmnAlpha3ToFull(A_MONTH_ALPHA3_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY, atmnAlpha3ToFull(A_MONTH_ALPHA3_MAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE, atmnAlpha3ToFull(A_MONTH_ALPHA3_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY, atmnAlpha3ToFull(A_MONTH_ALPHA3_JULY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST, atmnAlpha3ToFull(A_MONTH_ALPHA3_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER, atmnAlpha3ToFull(A_MONTH_ALPHA3_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER, atmnAlpha3ToFull(A_MONTH_ALPHA3_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER, atmnAlpha3ToFull(A_MONTH_ALPHA3_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER, atmnAlpha3ToFull(A_MONTH_ALPHA3_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH, atmnAlpha3ToFull(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA2_JANUARY, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_JANUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_FEBRUARY, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_MARCH, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_MARCH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA2_APRIL, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_APRIL)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA2_MAY, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_MAY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA2_JUNE, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_JUNE)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA2_JULY, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_JULY)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA2_AUGUST, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_AUGUST)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA2_SEPTEMBER, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_OCTOBER, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_OCTOBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOVEMBER, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_DECEMBER, atmnAlpha3ToAlpha2(A_MONTH_ALPHA3_DECEMBER)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOTAMONTH, atmnAlpha3ToAlpha2(A_MONTH_FULL_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Alpha3 to Alpha2 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_JANUARY) != A_MONTH_INT_JANUARY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_FEBRUARY) != A_MONTH_INT_FEBRUARY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_MARCH) != A_MONTH_INT_MARCH){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_MARCH".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_APRIL) != A_MONTH_INT_APRIL){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_APRIL".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_MAY) != A_MONTH_INT_MAY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_MAY".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_JUNE) != A_MONTH_INT_JUNE){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_JUNE".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_JULY) != A_MONTH_INT_JULY){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_JULY".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_AUGUST) != A_MONTH_INT_AUGUST){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_SEPTEMBER) != A_MONTH_INT_SEPTEMBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_OCTOBER) != A_MONTH_INT_OCTOBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_NOVEMBER) != A_MONTH_INT_NOVEMBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_DECEMBER) != A_MONTH_INT_DECEMBER){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (atmnAlpha3ToInteger(A_MONTH_ALPHA3_NOTAMONTH) != A_MONTH_INT_NOTAMONTH){FAILURE;}
	printf("Done! Alpha3 to Integer month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};


// Test month name from integer
int atmnMonthFromInteger(void){
	
	if (strcmp(A_MONTH_FULL_JANUARY, atmnIntegerToFull(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_FULL_FEBRUARY, atmnIntegerToFull(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_FULL_MARCH, atmnIntegerToFull(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_FULL_APRIL, atmnIntegerToFull(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_FULL_MAY, atmnIntegerToFull(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_FULL_JUNE, atmnIntegerToFull(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_FULL_JULY, atmnIntegerToFull(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_FULL_AUGUST, atmnIntegerToFull(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_FULL_SEPTEMBER, atmnIntegerToFull(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_FULL_OCTOBER, atmnIntegerToFull(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_FULL_NOVEMBER, atmnIntegerToFull(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_FULL_DECEMBER, atmnIntegerToFull(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_FULL_NOTAMONTH, atmnIntegerToFull(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Full month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA3_JANUARY, atmnIntegerToAlpha3(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_FEBRUARY, atmnIntegerToAlpha3(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA3_MARCH, atmnIntegerToAlpha3(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA3_APRIL, atmnIntegerToAlpha3(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA3_MAY, atmnIntegerToAlpha3(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA3_JUNE, atmnIntegerToAlpha3(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA3_JULY, atmnIntegerToAlpha3(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA3_AUGUST, atmnIntegerToAlpha3(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA3_SEPTEMBER, atmnIntegerToAlpha3(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_OCTOBER, atmnIntegerToAlpha3(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOVEMBER, atmnIntegerToAlpha3(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_DECEMBER, atmnIntegerToAlpha3(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA3_NOTAMONTH, atmnIntegerToAlpha3(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha3 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_ALPHA2_JANUARY, atmnIntegerToAlpha2(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_FEBRUARY, atmnIntegerToAlpha2(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_ALPHA2_MARCH, atmnIntegerToAlpha2(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_ALPHA2_APRIL, atmnIntegerToAlpha2(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_ALPHA2_MAY, atmnIntegerToAlpha2(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_ALPHA2_JUNE, atmnIntegerToAlpha2(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_ALPHA2_JULY, atmnIntegerToAlpha2(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_ALPHA2_AUGUST, atmnIntegerToAlpha2(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_ALPHA2_SEPTEMBER, atmnIntegerToAlpha2(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_OCTOBER, atmnIntegerToAlpha2(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOVEMBER, atmnIntegerToAlpha2(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_DECEMBER, atmnIntegerToAlpha2(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_ALPHA2_NOTAMONTH, atmnIntegerToAlpha2(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Alpha2 month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	if (strcmp(A_MONTH_NUMERIC_JANUARY, atmnTntegerToNumeric(A_MONTH_INT_JANUARY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_JANUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_FEBRUARY, atmnTntegerToNumeric(A_MONTH_INT_FEBRUARY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_FEBRUARY".\n");
	if (strcmp(A_MONTH_NUMERIC_MARCH, atmnTntegerToNumeric(A_MONTH_INT_MARCH)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_MARCH".\n");
	if (strcmp(A_MONTH_NUMERIC_APRIL, atmnTntegerToNumeric(A_MONTH_INT_APRIL)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_APRIL".\n");
	if (strcmp(A_MONTH_NUMERIC_MAY, atmnTntegerToNumeric(A_MONTH_INT_MAY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_MAY".\n");
	if (strcmp(A_MONTH_NUMERIC_JUNE, atmnTntegerToNumeric(A_MONTH_INT_JUNE)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_JUNE".\n");
	if (strcmp(A_MONTH_NUMERIC_JULY, atmnTntegerToNumeric(A_MONTH_INT_JULY)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_JULY".\n");
	if (strcmp(A_MONTH_NUMERIC_AUGUST, atmnTntegerToNumeric(A_MONTH_INT_AUGUST)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_AUGUST".\n");
	if (strcmp(A_MONTH_NUMERIC_SEPTEMBER, atmnTntegerToNumeric(A_MONTH_INT_SEPTEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_SEPTEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_OCTOBER, atmnTntegerToNumeric(A_MONTH_INT_OCTOBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_OCTOBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOVEMBER, atmnTntegerToNumeric(A_MONTH_INT_NOVEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_NOVEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_DECEMBER, atmnTntegerToNumeric(A_MONTH_INT_DECEMBER)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_DECEMBER".\n");
	if (strcmp(A_MONTH_NUMERIC_NOTAMONTH, atmnTntegerToNumeric(A_MONTH_INT_NOTAMONTH)) != 0){FAILURE;}
	printf("Done! Integer to Numeric month test passed for "A_MONTH_FULL_NOTAMONTH".\n");
	
	printf("\n");
	
	SUCCESS;
};