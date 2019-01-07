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

#endif //ARBOREUS_A_TIME_MONTH_H
