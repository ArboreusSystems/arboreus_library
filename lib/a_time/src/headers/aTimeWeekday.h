/* -------------------------------------------------------------------
 *  @doc Arboreus weekday name handler headers
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 12/24/2018 at 19:10
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_A_TIME_WEEKDAY_H
#define ARBOREUS_A_TIME_WEEKDAY_H

// Constant: weekday integer
#define A_WEEKDAY_INT_SUNDAY 0
#define A_WEEKDAY_INT_MONDAY 1
#define A_WEEKDAY_INT_TUESDAY 2
#define A_WEEKDAY_INT_WEDNESDAY 3
#define A_WEEKDAY_INT_THURSDAY 4
#define A_WEEKDAY_INT_FRIDAY 5
#define A_WEEKDAY_INT_SATURDAY 6
#define A_WEEKDAY_INT_NOTAWEEKDAY -1

// Constant: weekday full
#define A_WEEKDAY_FULL_SUNDAY "Sunday"
#define A_WEEKDAY_FULL_MONDAY "Monday"
#define A_WEEKDAY_FULL_TUESDAY "Tuesday"
#define A_WEEKDAY_FULL_WEDNESDAY "Wednesday"
#define A_WEEKDAY_FULL_THURSDAY "Thursday"
#define A_WEEKDAY_FULL_FRIDAY "Friday"
#define A_WEEKDAY_FULL_SATURDAY "Saturday"
#define A_WEEKDAY_FULL_NOTAWEEKDAY "Notaweekday"

// Constant: weekday Alpha3
#define A_WEEKDAY_ALPHA3_SUNDAY "Sun"
#define A_WEEKDAY_ALPHA3_MONDAY "Mon"
#define A_WEEKDAY_ALPHA3_TUESDAY "Tue"
#define A_WEEKDAY_ALPHA3_WEDNESDAY "Wed"
#define A_WEEKDAY_ALPHA3_THURSDAY "Thu"
#define A_WEEKDAY_ALPHA3_FRIDAY "Fri"
#define A_WEEKDAY_ALPHA3_SATURDAY "Sat"
#define A_WEEKDAY_ALPHA3_NOTAWEEKDAY "Not"

// Constant: weekday Alpha2
#define A_WEEKDAY_ALPHA2_SUNDAY "Su"
#define A_WEEKDAY_ALPHA2_MONDAY "Mo"
#define A_WEEKDAY_ALPHA2_TUESDAY "Tu"
#define A_WEEKDAY_ALPHA2_WEDNESDAY "We"
#define A_WEEKDAY_ALPHA2_THURSDAY "Th"
#define A_WEEKDAY_ALPHA2_FRIDAY "Fr"
#define A_WEEKDAY_ALPHA2_SATURDAY "Sa"
#define A_WEEKDAY_ALPHA2_NOTAWEEKDAY "No"

// Constant: weekday numeric
#define A_WEEKDAY_NUMERIC_SUNDAY "00"
#define A_WEEKDAY_NUMERIC_MONDAY "01"
#define A_WEEKDAY_NUMERIC_TUESDAY "02"
#define A_WEEKDAY_NUMERIC_WEDNESDAY "03"
#define A_WEEKDAY_NUMERIC_THURSDAY "04"
#define A_WEEKDAY_NUMERIC_FRIDAY "05"
#define A_WEEKDAY_NUMERIC_SATURDAY "06"
#define A_WEEKDAY_NUMERIC_NOTAWEEKDAY "08"

// Functionality for Integer
const char *atwdIntegerToAlpha3(int Month);
const char *atwdIntegerToAlpha2(int Month);
const char *atwdIntegerToFull(int Month);
const char *atwdIntegerToNumeric(int Month);

// Functionality for Alpha3
int atwdAlpha3ToInteger(char *Pointer);
const char *atwdAlpha3ToAlpha2(char *Pointer);
const char *atwdAlpha3ToFull(char *Pointer);
const char *atwdAlpha3ToNumeric(char *Pointer);

// Functionality for Full
int atwdFullToInteger(char *Pointer);
const char *atwdFullToAlpha3(char *Pointer);
const char *atwdFullToAlpha2(char *Pointer);
const char *atwdFullToNumeric(char *Pointer);

// Functionality for Numeric
int atwdNumericToInteger(char *Pointer);
const char *atwdNumericToAlpha3(char *Pointer);
const char *atwdNumericToAlpha2(char *Pointer);
const char *atwdNumericToFull(char *Pointer);

#endif //ARBOREUS_A_TIME_WEEKDAY_H
