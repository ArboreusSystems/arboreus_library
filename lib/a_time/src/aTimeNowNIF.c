/* -------------------------------------------------------------------
 *  @doc Arboreus a_time_nof NIF
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/11/2019 at 18:01
 * */// --------------------------------------------------------------

// System includes
#include <stdio.h>
#include <erl_nif.h>
#include <time.h>
#include <sys/time.h>
#include <sys/timeb.h>

// Application includes
#include "../../constants/aConstantsGeneral.h"
#include "headers/aTimeNowNIF.h"
#include "headers/aTimeNowHandler.h"


// NIF API Functionality
static ErlNifFunc nif_funcs[] = {
	
	{"microseconds",0,atNIFMicroseconds},
	{"milliseconds",0,atNIFMilliseconds},
	{"seconds",0,atNIFSeconds},
	{"integer",0,atNIFInteger},
	{"integer_date",0,atNIFIntegerDate},
	{"integer_full",0,atNIFIntegerFull},
	{"integer_extend",0,atNIFIntegerExtend},
	{"rfc_822",0,atNIFRFC822},
	{"rfc_850",0,atNIFRFC850},
	{"ansi",0,atNIFANSI}
};


// Return time in microseconds
static ERL_NIF_TERM atNIFMicroseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Microseconds = 0;
	atnhMicroseconds(&Microseconds);
	if (Microseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Microseconds);
	}
}


// Return time in milliseconds
static ERL_NIF_TERM atNIFMilliseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Milliseconds = 0;
	atnhMilliseconds(&Milliseconds);
	if (Milliseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Milliseconds);
	}
}


// Return time in seconds
static ERL_NIF_TERM atNIFSeconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Seconds = 0;
	atnhSeconds(&Seconds);
	if (Seconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Seconds);
	}
}


// Return time in integer
static ERL_NIF_TERM atNIFInteger(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Integer = 0;
	atnhInt(&Integer);
	if (Integer == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Integer);
	}
}


// Return date in integer
static ERL_NIF_TERM atNIFIntegerDate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Int_date = 0;
	atnhIntDate(&Int_date);
	if (Int_date == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Int_date);
	}
}


// Return full date in integer
static ERL_NIF_TERM atNIFIntegerFull(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Int_full = 0;
	atnhIntFull(&Int_full);
	if (Int_full == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Int_full);
	}
}


// Return extended date integer
static ERL_NIF_TERM atNIFIntegerExtend(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Int_full = 0;
	atnhIntExtend(&Int_full);
	if (Int_full == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Int_full);
	}
}


// Return RFC822 string within current time
static ERL_NIF_TERM atNIFRFC822(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *RFC_822;
	if (atnhRFC822(&RFC_822) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,RFC_822,ERL_NIF_LATIN1);
	}
}


// Return RFC850 string within current time
static ERL_NIF_TERM atNIFRFC850(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *RFC_850;
	if (atnhRFC850(&RFC_850) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,RFC_850,ERL_NIF_LATIN1);
	}
}


// Return ANSI string within current time
static ERL_NIF_TERM atNIFANSI(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *ANSI;
	if (atnhANSI(&ANSI) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,ANSI,ERL_NIF_LATIN1);
	}
}


// NIF initializer
ERL_NIF_INIT(a_time_now,nif_funcs,NULL,NULL,NULL,NULL)