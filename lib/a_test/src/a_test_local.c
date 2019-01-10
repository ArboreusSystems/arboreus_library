// ------------------------------------------
// Arboreus test C application
// Created by Alexandr Kirilov on 2018-11-30.
//

#include <stdio.h>
#include <string.h>
#include "../../constants/constants_general.h"
#include "../../universal_c/src/headers/a_string.h"
#include "../../universal_c/src/headers/a_convert.h"
#include <sys/time.h>
#include <sys/timeb.h>
#include "../../a_time/src/headers/a_time_ansi.h"


int main(int n,char *Arguments[]){
	
	time_t Time = time(NULL);
	struct tm *Today;
	Today = localtime(&Time);
	char *ANSI;
	atansi_from_timestamp(0,&ANSI);
	printf("ANSI: %s\n",ANSI);
	
	SUCCESS;
}