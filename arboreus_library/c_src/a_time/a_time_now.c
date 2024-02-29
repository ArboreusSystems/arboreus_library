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
#include "../../../arboreus_library/c_src/constants/a_constants_general.h"
#include "headers/a_time_now.h"
#include "headers/a_time_now_handler.h"


// NIF API Functionality
static ErlNifFunc nif_funcs[] = {

	{"nanoseconds",0,atNIFNanoseconds},
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
static ERL_NIF_TERM atNIFNanoseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

	long long int oNanoseconds = 0;
	atnhNanoseconds(&oNanoseconds);
	if (oNanoseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oNanoseconds);
	}
}


// Return time in microseconds
static ERL_NIF_TERM atNIFMicroseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int oMicroseconds = 0;
	atnhMicroseconds(&oMicroseconds);
	if (oMicroseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oMicroseconds);
	}
}


// Return time in milliseconds
static ERL_NIF_TERM atNIFMilliseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int oMilliseconds = 0;
	atnhMilliseconds(&oMilliseconds);
	if (oMilliseconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oMilliseconds);
	}
}


// Return time in seconds
static ERL_NIF_TERM atNIFSeconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int oSeconds = 0;
	atnhSeconds(&oSeconds);
	if (oSeconds == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oSeconds);
	}
}


// Return time in integer
static ERL_NIF_TERM atNIFInteger(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int oInteger = 0;
	atnhInt(&oInteger);
	if (oInteger == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oInteger);
	}
}


// Return date in integer
static ERL_NIF_TERM atNIFIntegerDate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int oIntegrDate = 0;
	atnhIntDate(&oIntegrDate);
	if (oIntegrDate == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oIntegrDate);
	}
}


// Return full date in integer
static ERL_NIF_TERM atNIFIntegerFull(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int oIntegerFull = 0;
	atnhIntFull(&oIntegerFull);
	if (oIntegerFull == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oIntegerFull);
	}
}


// Return extended date integer
static ERL_NIF_TERM atNIFIntegerExtend(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	long long int oIntegerExtended = 0;
	atnhIntExtend(&oIntegerExtended);
	if (oIntegerExtended == 0){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_uint64(env,(unsigned long)oIntegerExtended);
	}
}


// Return RFC822 string within current time
static ERL_NIF_TERM atNIFRFC822(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *oRFC_822;
	if (atnhRFC822(&oRFC_822) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,oRFC_822,ERL_NIF_LATIN1);
	}
}


// Return RFC850 string within current time
static ERL_NIF_TERM atNIFRFC850(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *oRFC_850;
	if (atnhRFC850(&oRFC_850) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,oRFC_850,ERL_NIF_LATIN1);
	}
}


// Return ANSI string within current time
static ERL_NIF_TERM atNIFANSI(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
	
	char *oANSI;
	if (atnhANSI(&oANSI) != EXIT_SUCCESS){
		return enif_make_atom(env,"false");
	} else {
		return enif_make_string(env,oANSI,ERL_NIF_LATIN1);
	}
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {

    *priv_data = enif_open_resource_type(
        env,NULL,"cwm_utils_buf",NULL,ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,NULL
    );
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {

    *priv_data = enif_open_resource_type(
        env,NULL,"cwm_utils_buf",NULL,ERL_NIF_RT_TAKEOVER,NULL
    );
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {

    return ;
}

// NIF initializer
ERL_NIF_INIT(a_time_now,nif_funcs,&load,NULL,&upgrade,&unload)
