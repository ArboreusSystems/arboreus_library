// -------------------------------------------------------------------
// Arboreus A_time Erlang NIF module
// @author Alexandr KIRILOV (http://alexandr.kirilov.me)
// @copyright (C) 2015-2019, Arboreus, (http://arboreus.systems)
//
// Created at 18:23 03.12.2018
// -------------------------------------------------------------------

// System includes
#include <time.h>
#include <sys/time.h>
#include <sys/timeb.h>

// Application includes
#include "a_time_nif.h"
#include "a_time_handler.h"


// NIF API Functionality
static ErlNifFunc nif_funcs[] =
	{
		{"now_seconds",0,now_seconds},
		{"now_milliseconds",0,now_milliseconds},
		{"now_microseconds",0,now_microseconds},
		{"now_date_int",0,now_date_int},
		{"now_full_int",0,now_full_int},
		{"now_int",0,now_int},
		{"now_date",0,now_date}
	};


// Return time in seconds
static ERL_NIF_TERM now_seconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Seconds = 0;
	a_time_seconds(&Seconds);
	if (Seconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Seconds);
	}
}


// Return time in milliseconds
static ERL_NIF_TERM now_milliseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

	long long int Milliseconds = 0;
	a_time_milliseconds(&Milliseconds);
	if (Milliseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Milliseconds);
	}
}


// Return time in microseconds
static ERL_NIF_TERM now_microseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Microseconds = 0;
	a_time_microseconds(&Microseconds);
	if (Microseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Microseconds);
	}
}


// Return date integer
static ERL_NIF_TERM now_date_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Date_int = 0;
	a_time_date_int(&Date_int);
	if (Date_int == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Date_int);
	}
}


// Return full time integer
static ERL_NIF_TERM now_full_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Full_int = 0;
	a_time_full_int(&Full_int);
	if (Full_int == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Full_int);
	}
}


// Return time integer
static ERL_NIF_TERM now_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int Int = 0;
	a_time_int(&Int);
	if (Int == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Int);
	}
}


// Return tuple with year, month, day
static ERL_NIF_TERM now_date(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	int Date[] = {0,0,0};
	a_time_date(Date);
	return enif_make_tuple(env,3,
		enif_make_int(env,Date[0]),
		enif_make_int(env,Date[1]),
		enif_make_int(env,Date[2])
	);
}


// NIF initializer
ERL_NIF_INIT(a_time,nif_funcs,NULL,NULL,NULL,NULL)
