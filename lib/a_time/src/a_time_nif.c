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


// Return time in seconds
static ERL_NIF_TERM now_seconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	long long int Seconds = 0;
	
	time_t Time;
	Seconds = time(&Time);
	
	if (Seconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Seconds);
	}
}


// Return time in milliseconds
static ERL_NIF_TERM now_milliseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

	long long int Milliseconds = 0;
	
	struct timeb Time;
	if (!ftime(&Time)) {
		Milliseconds = ((long long int)Time.time)*1000ll + (long long int)Time.millitm;
	}
	
	if (Milliseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Milliseconds);
	}
}


// Return time in microseconds
static ERL_NIF_TERM now_microseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	
	long long int Microseconds = 0;
	
	struct timeval Time;
	if (!gettimeofday(&Time, NULL)) {
		Microseconds = ((long long int)Time.tv_sec)*1000000ll + (long long int)Time.tv_usec;
	}
	
	if (Microseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,Microseconds);
	}
}


// Module function list
static ErlNifFunc nif_funcs[] =
	{
		{"now_seconds",0,now_seconds},
		{"now_milliseconds",0,now_milliseconds},
		{"now_microseconds",0,now_microseconds}
	};


// NIF initializer
ERL_NIF_INIT(a_time,nif_funcs,NULL,NULL,NULL,NULL)
