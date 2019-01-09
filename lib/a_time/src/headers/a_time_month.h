// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2018, http://arboreus.system
// @doc Armoreus month name handler headers
//
// @end
// Created : 12/24/2018 at 18:17
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_MONTH_H
#define ARBOREUS_A_TIME_MONTH_H

#define A_MONTH_INT_JANUARY 0
#define A_MONTH_INT_FEBRUARY 1
#define A_MONTH_INT_MARCH 2
#define A_MONTH_INT_APRIL 3
#define A_MONTH_INT_MAY 4
#define A_MONTH_INT_JUNE 5
#define A_MONTH_INT_JULY 6
#define A_MONTH_INT_AUGUST 7
#define A_MONTH_INT_SEPTEMBER 8
#define A_MONTH_INT_OCTOBER 9
#define A_MONTH_INT_NOVEMBER 10
#define A_MONTH_INT_DECEMBER 11
#define A_MONTH_INT_NOTAMONTH -1

#define A_MONTH_FULL_JANUARY "January"
#define A_MONTH_FULL_FEBRUARY "February"
#define A_MONTH_FULL_MARCH "March"
#define A_MONTH_FULL_APRIL "April"
#define A_MONTH_FULL_MAY "May"
#define A_MONTH_FULL_JUNE "June"
#define A_MONTH_FULL_JULY "July"
#define A_MONTH_FULL_AUGUST "August"
#define A_MONTH_FULL_SEPTEMBER "September"
#define A_MONTH_FULL_OCTOBER "October"
#define A_MONTH_FULL_NOVEMBER "November"
#define A_MONTH_FULL_DECEMBER "December"
#define A_MONTH_FULL_NOTAMONTH "Notamonth"

#define A_MONTH_ALPHA3_JANUARY "Jan"
#define A_MONTH_ALPHA3_FEBRUARY "Feb"
#define A_MONTH_ALPHA3_MARCH "Mar"
#define A_MONTH_ALPHA3_APRIL "Apr"
#define A_MONTH_ALPHA3_MAY "May"
#define A_MONTH_ALPHA3_JUNE "Jun"
#define A_MONTH_ALPHA3_JULY "Jul"
#define A_MONTH_ALPHA3_AUGUST "Aug"
#define A_MONTH_ALPHA3_SEPTEMBER "Sep"
#define A_MONTH_ALPHA3_OCTOBER "Oct"
#define A_MONTH_ALPHA3_NOVEMBER "Nov"
#define A_MONTH_ALPHA3_DECEMBER "Dec"
#define A_MONTH_ALPHA3_NOTAMONTH "Not"

#define A_MONTH_ALPHA2_JANUARY "Ja"
#define A_MONTH_ALPHA2_FEBRUARY "Fb"
#define A_MONTH_ALPHA2_MARCH "Mr"
#define A_MONTH_ALPHA2_APRIL "Ap"
#define A_MONTH_ALPHA2_MAY "Ma"
#define A_MONTH_ALPHA2_JUNE "Jn"
#define A_MONTH_ALPHA2_JULY "Jl"
#define A_MONTH_ALPHA2_AUGUST "Ag"
#define A_MONTH_ALPHA2_SEPTEMBER "Sp"
#define A_MONTH_ALPHA2_OCTOBER "Oc"
#define A_MONTH_ALPHA2_NOVEMBER "Nv"
#define A_MONTH_ALPHA2_DECEMBER "De"
#define A_MONTH_ALPHA2_NOTAMONTH "Nt"

#define A_MONTH_NUMERIC_JANUARY "01"
#define A_MONTH_NUMERIC_FEBRUARY "02"
#define A_MONTH_NUMERIC_MARCH "03"
#define A_MONTH_NUMERIC_APRIL "04"
#define A_MONTH_NUMERIC_MAY "05"
#define A_MONTH_NUMERIC_JUNE "06"
#define A_MONTH_NUMERIC_JULY "07"
#define A_MONTH_NUMERIC_AUGUST "08"
#define A_MONTH_NUMERIC_SEPTEMBER "09"
#define A_MONTH_NUMERIC_OCTOBER "10"
#define A_MONTH_NUMERIC_NOVEMBER "11"
#define A_MONTH_NUMERIC_DECEMBER "12"
#define A_MONTH_NUMERIC_NOTAMONTH "00"

const char *atmn_integer_to_alpha3(int Month);
const char *atmn_integer_to_alpha2(int Month);
const char *atmn_integer_to_full(int Month);
const char *atmn_integer_to_numeric(int Month);

int atmn_alpha3_to_integer(char *Alpha3);
const char *atmn_alpha3_to_alpha2(char *Alpha3);
const char *atmn_alpha3_to_full(char *Alpha3);
const char *atmn_alpha3_to_numeric(char *Alpha3);

int atmn_alpha2_to_integer(char *Alpha2);
const char *atmn_alpha2_to_alpha3(char *Alpha2);
const char *atmn_alpha2_to_full(char *Alpha2);
const char *atmn_alpha2_to_numeric(char *Alpha2);

int atmn_numeric_to_integer(char *Numeric);
const char *atmn_numeric_to_alpha3(char *Numeric);
const char *atmn_numeric_to_alpha2(char *Numeric);
const char *atmn_numeric_to_full(char *Numeric);

#endif //ARBOREUS_A_TIME_MONTH_H
