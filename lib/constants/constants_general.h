// -------------------------------------------------------------------
// Arboreus Constants for CLang modules
// @author Alexandr KIRILOV (http://alexandr.kirilov.me)
// @copyright (C) 2015-2019, Arboreus, (http://arboreus.systems)
//
// Created at 18:14 03.12.2018
// -------------------------------------------------------------------


#ifndef ARBOREUS_CONSTANTS_GENERAL_H
#define ARBOREUS_CONSTANTS_GENERAL_H

#define SUCCESS return 0
#define FAIL_VALUE(Value) return Value
#define FAIL_ERROR(Value,Message) err(Value,"%s",Message)
#define FAIL_EXIT(Value) exit(Value)

#endif