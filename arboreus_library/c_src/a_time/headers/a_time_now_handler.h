/* -------------------------------------------------------------------
 *  @doc Arboreus Time now handler headers
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 12/24/2018 at 13:56
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_A_TIME_NOW_HANDLER_H
#define ARBOREUS_A_TIME_NOW_HANDLER_H

#ifdef __cplusplus
extern "C" {
#endif

// Functionality
int atnhNanoseconds(long long int *Pointer);
int atnhMicroseconds(long long int *Pointer);
int atnhMilliseconds(long long int *Pointer);
int atnhSeconds(long long int *Pointer);
int atnhInt(long long int *Pointer);
int atnhIntDate(long long int *Pointer);
int atnhIntFull(long long int *Pointer);
int atnhIntExtend(long long int *Pointer);
int atnhRFC822(char **RFC_822);
int atnhRFC850(char **RFC_850);
int atnhANSI(char **ANSI);

#ifdef __cplusplus
}
#endif

#endif //ARBOREUS_A_TIME_NOW_HANDLER_H
