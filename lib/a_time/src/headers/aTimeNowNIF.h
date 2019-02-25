/* -------------------------------------------------------------------
 *  @doc Arboreus Time now NIF headers
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/11/2019 at 18:01
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_A_TIME_NOW_NIF_H
#define ARBOREUS_A_TIME_NOW_NIF_H

#ifdef __cplusplus
extern "C" {
#endif

// Functionality
static ERL_NIF_TERM atNIFMicroseconds(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFMilliseconds(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFSeconds(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFInteger(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFIntegerDate(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFIntegerFull(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFIntegerExtend(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFRFC822(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFRFC850(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);
static ERL_NIF_TERM atNIFANSI(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv);

#ifdef __cplusplus
}
#endif

#endif //ARBOREUS_A_TIME_NOW_NIF_H