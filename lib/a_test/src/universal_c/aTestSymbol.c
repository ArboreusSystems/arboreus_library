/* -------------------------------------------------------------------
 *  @doc Testing aSymbol.c functionality
 *  @notice
 *  
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 02/01/2019 at 16:49
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>

// Constants
#include "../../../constants/aConstantsGeneral.h"

// Application includes
#include "headers/aTestSymbol.h"
#include "../../../universal_c/src/headers/aSymbol.h"


// Checking test of latin lower case string functionality
int atucIsLatinLower() {
	
	int i = 0;
	
	for (i = 1; i < 97; i++) {if (asymIsLatinLower((char)i) != EXIT_FAILURE) {FAILURE;}}
	for (i = 97; i < 123; i++) {if (asymIsLatinLower((char)i) != EXIT_SUCCESS) {FAILURE;}}
	for (i = 123; i < 320; i++) {if (asymIsLatinLower((char)i) != EXIT_FAILURE) {FAILURE;}}
	
	printf("Done! asymIsLatinLower() test passed.\n");
	
	SUCCESS;
}


// Checking test of latin upper case string functionality
int atucIsLatinUpper() {
	
	int i = 0;
	
	for (i = 1; i < 65; i++) {if (asymIsLatinUpper((char)i) != EXIT_FAILURE) {FAILURE;}}
	for (i = 65; i < 91; i++) {if (asymIsLatinUpper((char)i) != EXIT_SUCCESS) {FAILURE;}}
	for (i = 91; i < 320; i++) {if (asymIsLatinUpper((char)i) != EXIT_FAILURE) {FAILURE;}}
	
	printf("Done! asymIsLatinUpper() test passed.\n");
	
	SUCCESS;
}


// Checking test of latin string functionality
int atucIsLatin() {
	
	int i = 0;
	
	for (i = 1; i < 65; i++) {if (asymIsLatin((char)i) != EXIT_FAILURE) {FAILURE;}}
	for (i = 65; i < 91; i++) {if (asymIsLatin((char)i) != EXIT_SUCCESS) {FAILURE;}}
	for (i = 91; i < 97; i++) {if (asymIsLatin((char)i) != EXIT_FAILURE) {FAILURE;}}
	for (i = 97; i < 123; i++) {if (asymIsLatin((char)i) != EXIT_SUCCESS) {FAILURE;}}
	for (i = 123; i < 320; i++) {if (asymIsLatin((char)i) != EXIT_FAILURE) {FAILURE;}}
	
	printf("Done! asymIsLatin() test passed.\n");
	
	SUCCESS;
}


// Checking test of numeric string functionality
int atucIsNumeric() {
	
	int i = 0;
	
	for (i = 1; i < 48; i++) {if (asymIsNumeric((char)i) != EXIT_FAILURE) {FAILURE;}}
	for (i = 48; i < 58; i++) {if (asymIsNumeric((char)i) != EXIT_SUCCESS) {FAILURE;}}
	for (i = 58; i < 200; i++) {if (asymIsNumeric((char)i) != EXIT_FAILURE) {FAILURE;}}
	
	printf("Done! atucIsNumeric() test passed.\n");
	
	SUCCESS;
}


// Checking "@" (at-sign) letter functionality
int atucIsAt() {
	
	int i = 0;
	
	for (i = 1; i < 320; i++) {
		if (i == 64) {
			if (asymIsAt((char)i) != EXIT_SUCCESS) {FAILURE;}
		} else {
			if (asymIsAt((char)i) != EXIT_FAILURE) {FAILURE;}
		}
	}
	
	printf("Done! atucIsAt() test passed.\n");
	
	SUCCESS;
}


// Checking "." (dot) letter functionality
int atucIsDot() {
	
	int i = 0;
	
	for (i = 1; i < 200; i++) {
		if (i == 46) {
			if (asymIsDot((char)i) != EXIT_SUCCESS) {FAILURE;}
		} else {
			if (asymIsDot((char)i) != EXIT_FAILURE) {FAILURE;}
		}
	}
	
	printf("Done! atucIsDot() test passed.\n");
	
	SUCCESS;
}


// Checking " " (space) letter functionality
int atucIsSpace() {
	
	int i = 0;
	
	for (i = 1; i < 200; i++) {
		if (i == 32) {
			if (asymIsSpace((char)i) != EXIT_SUCCESS) {FAILURE;}
		} else {
			if (asymIsSpace((char)i) != EXIT_FAILURE) {FAILURE;}
		}
	}
	
	printf("Done! atucIsSpace() test passed.\n");
	
	SUCCESS;
}
