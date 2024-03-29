/* -------------------------------------------------------------------
 *  @doc Arboreus ANSI time format handler headers
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/10/2019 at 15:34
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_A_TIME_ANSI_H
#define ARBOREUS_A_TIME_ANSI_H

// System include
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

// Functionality
int atANSIFromStruct(struct tm Time_struct, char **ANSI);
int atANSIFromTimestamp(long long int Timestamp, char **ANSI);

#ifdef __cplusplus
}
#endif

#endif //ARBOREUS_A_TIME_ANSI_H
