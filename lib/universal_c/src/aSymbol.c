/* -------------------------------------------------------------------
 *  @doc UTF-8 String symbols handler
 *  @notice
 *  
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 02/01/2019 at 15:39
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/aSymbol.h"


// Checking UTF-8 Basic " "(space) symbol
int asymIsSpace(int Symbol) {
	
	return asymIsEqual(Symbol,32);
};


// Checking UTF-8 Basic "."(dot) symbol
int asymIsDot(int Symbol) {
	
	return asymIsEqual(Symbol,46);
};


// Checking UTF-8 Basic "@" symbol
int asymIsAt(int Symbol) {
	
	return asymIsEqual(Symbol,64);
};


// Checking UTF-8 Basic numeric symbol
int asymIsNumeric(int Symbol) {
	
	if (Symbol >= 48) {
		if (Symbol <= 57) {
			SUCCESS;
		} else {FAILURE;}
	} else {FAILURE;}
};


// Checking UTF-8 Basic Latin symbol
int asymIsLatin(int Symbol) {
	
	if (asymIsLatinUpper(Symbol) == EXIT_SUCCESS) {SUCCESS;}
	if (asymIsLatinLower(Symbol) == EXIT_SUCCESS) {SUCCESS;}
	FAILURE;
};


// Checking UTF-8 Basic Latin upper case symbol
int asymIsLatinUpper(int Symbol) {
	
	if (Symbol >= 65) {
		if (Symbol <= 90) {
			SUCCESS;
		} else {FAILURE;}
	} else {FAILURE;}
};


// Checking UTF-8 Basic Latin lower case symbol
int asymIsLatinLower(int Symbol) {
	
	if (Symbol >= 97 ) {
		if (Symbol <= 122) {
			SUCCESS;
		} else {FAILURE;}
	} else {FAILURE;}
};


// Cheking equality by UTF8 number
int asymIsEqual(int Symbol,int UTFNumber) {
	
	if (Symbol != UTFNumber) {FAILURE;}
	SUCCESS;
};