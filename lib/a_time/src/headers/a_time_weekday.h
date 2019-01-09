// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Arboreus weekday name handler headers
//
// @end
// Created : 12/24/2018 at 19:10
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_WEEKDAY_H
#define ARBOREUS_A_TIME_WEEKDAY_H

#define A_WEEKDAY_INT_SUNDAY 0
#define A_WEEKDAY_INT_MONDAY 1
#define A_WEEKDAY_INT_TUESDAY 2
#define A_WEEKDAY_INT_WEDNESDAY 3
#define A_WEEKDAY_INT_THURSDAY 4
#define A_WEEKDAY_INT_FRIDAY 5
#define A_WEEKDAY_INT_SATURDAY 6
#define A_WEEKDAY_INT_NOTAWEEKDAY -1

#define A_WEEKDAY_FULL_SUNDAY "Sunday"
#define A_WEEKDAY_FULL_MONDAY "Monday"
#define A_WEEKDAY_FULL_TUESDAY "Tuesday"
#define A_WEEKDAY_FULL_WEDNESDAY "Wednesday"
#define A_WEEKDAY_FULL_THURSDAY "Thursday"
#define A_WEEKDAY_FULL_FRIDAY "Friday"
#define A_WEEKDAY_FULL_SATURDAY "Saturday"
#define A_WEEKDAY_FULL_NOTAWEEKDAY "Notaweekday"

#define A_WEEKDAY_ALPHA3_SUNDAY "Sun"
#define A_WEEKDAY_ALPHA3_MONDAY "Mon"
#define A_WEEKDAY_ALPHA3_TUESDAY "Tue"
#define A_WEEKDAY_ALPHA3_WEDNESDAY "Wed"
#define A_WEEKDAY_ALPHA3_THURSDAY "Thu"
#define A_WEEKDAY_ALPHA3_FRIDAY "Fri"
#define A_WEEKDAY_ALPHA3_SATURDAY "Sat"
#define A_WEEKDAY_ALPHA3_NOTAWEEKDAY "Not"

#define A_WEEKDAY_ALPHA2_SUNDAY "Su"
#define A_WEEKDAY_ALPHA2_MONDAY "Mo"
#define A_WEEKDAY_ALPHA2_TUESDAY "Tu"
#define A_WEEKDAY_ALPHA2_WEDNESDAY "We"
#define A_WEEKDAY_ALPHA2_THURSDAY "Th"
#define A_WEEKDAY_ALPHA2_FRIDAY "Fr"
#define A_WEEKDAY_ALPHA2_SATURDAY "Sa"
#define A_WEEKDAY_ALPHA2_NOTAWEEKDAY "No"

#define A_WEEKDAY_NUMERIC_SUNDAY "07"
#define A_WEEKDAY_NUMERIC_MONDAY "01"
#define A_WEEKDAY_NUMERIC_TUESDAY "02"
#define A_WEEKDAY_NUMERIC_WEDNESDAY "03"
#define A_WEEKDAY_NUMERIC_THURSDAY "04"
#define A_WEEKDAY_NUMERIC_FRIDAY "05"
#define A_WEEKDAY_NUMERIC_SATURDAY "06"
#define A_WEEKDAY_NUMERIC_NOTAWEEKDAY "08"

const char *atwd_integer_to_alpha3(int Month);
const char *atwd_integer_to_alpha2(int Month);
const char *atwd_integer_to_full(int Month);
const char *atwd_integer_to_numeric(int Month);

int atwd_alpha3_to_integer(char *Pointer);
const char *atwd_alpha3_to_alpha2(char *Pointer);
const char *atwd_alpha3_to_full(char *Pointer);
const char *atwd_alpha3_to_numeric(char *Pointer);

int atwd_full_to_integer(char *Pointer);
const char *atwd_full_to_alpha3(char *Pointer);
const char *atwd_full_to_alpha2(char *Pointer);
const char *atwd_full_to_numeric(char *Pointer);

int atwd_numeric_to_integer(char *Pointer);
const char *atwd_numeric_to_alpha3(char *Pointer);
const char *atwd_numeric_to_alpha2(char *Pointer);
const char *atwd_numeric_to_full(char *Pointer);

#endif //ARBOREUS_A_TIME_WEEKDAY_H
