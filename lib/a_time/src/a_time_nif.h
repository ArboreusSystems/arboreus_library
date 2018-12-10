//
// Created by Alexandr Kirilov on 2018-12-08.
//

#ifndef ARBOREUS_A_TIME_NIF_H
#define ARBOREUS_A_TIME_NIF_H

#include <erl_nif.h>

static ERL_NIF_TERM now_seconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM now_milliseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM now_microseconds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM now_date_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM now_full_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM now_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM now_date(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
