// -------------------------------------------------------------------
// @author Alexandr KIRILOV
// @copyright (C) 2019, http://arboreus.system
// @doc Arboreus a_time_nof NIF headers
//
// @end
// Created : 01/11/2019 at 18:01
// -------------------------------------------------------------------
#ifndef ARBOREUS_A_TIME_NOW_NIF_H
#define ARBOREUS_A_TIME_NOW_NIF_H

static ERL_NIF_TERM atnif_microseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_milliseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_seconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_integer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_integer_date(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_integer_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_integer_extend(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_rfc_822(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_rfc_850(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM atnif_ansi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif //ARBOREUS_A_TIME_NOW_NIF_H