// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus a_time_nof NIF
//
// @end
// Created : 01/11/2019 at 18:01
// -------------------------------------------------------------------

// System includes
#include <stdio.h>
#include <erl_nif.h>
#include <time.h>
#include <sys/time.h>
#include <sys/timeb.h>

// Application includes
#include "../../constants/constants_general.h"
#include "headers/a_time_now_nif.h"
#include "headers/a_time_now_handler.h"


// NIF API Functionality
static ErlNifFunc nif_funcs[] = {
	
	{"microseconds",0,atnif_microseconds},
	{"milliseconds",0,atnif_milliseconds},
	{"seconds",0,atnif_seconds},
	{"integer",0,atnif_integer},
	{"integer_date",0,atnif_integer_date},
	{"integer_full",0,atnif_integer_full},
	{"integer_extend",0,atnif_integer_extend},
	{"rfc_822",0,atnif_rfc_822},
	{"rfc_850",0,atnif_rfc_850},
	{"ansi",0,atnif_ansi}
};


// Return time in microseconds
static ERL_NIF_TERM atnif_microseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Microseconds = 0;
	atnh_microseconds(&Microseconds);
	if (Microseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Microseconds);
	}
}


// Return time in milliseconds
static ERL_NIF_TERM atnif_milliseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Milliseconds = 0;
	atnh_milliseconds(&Milliseconds);
	if (Milliseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Milliseconds);
	}
}


// Return time in seconds
static ERL_NIF_TERM atnif_seconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Seconds = 0;
	atnh_seconds(&Seconds);
	if (Seconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Seconds);
	}
}


// Return time in integer
static ERL_NIF_TERM atnif_integer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Integer = 0;
	atnh_int(&Integer);
	if (Integer == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Integer);
	}
}


// Return date in integer
static ERL_NIF_TERM atnif_integer_date(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Int_date = 0;
	atnh_int(&Int_date);
	if (Int_date == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Int_date);
	}
}


// Return full date in integer
static ERL_NIF_TERM atnif_integer_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Int_full = 0;
	atnh_int_full(&Int_full);
	if (Int_full == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Int_full);
	}
}


// Return extended date integer
static ERL_NIF_TERM atnif_integer_extend(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Int_full = 0;
	atnh_int_extend(&Int_full);
	if (Int_full == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)Int_full);
	}
}


// Return RFC822 string within current time
static ERL_NIF_TERM atnif_rfc_822(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *RFC_822;
	if (atnh_rfc_822(&RFC_822) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,RFC_822,ERL_NIF_LATIN1);
	}
}


// Return RFC850 string within current time
static ERL_NIF_TERM atnif_rfc_850(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *RFC_850;
	if (atnh_rfc_850(&RFC_850) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,RFC_850,ERL_NIF_LATIN1);
	}
}


// Return ANSI string within current time
static ERL_NIF_TERM atnif_ansi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *ANSI;
	if (atnh_ansi(&ANSI) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,ANSI,ERL_NIF_LATIN1);
	}
}


// NIF initializer
ERL_NIF_INIT(a_time_now,nif_funcs,NULL,NULL,NULL,NULL)