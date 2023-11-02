/* -------------------------------------------------------------------
 *  @doc Arboreus RFC850 time format handler headers
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/09/2019 at 19:02
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_A_TIME_RFC850_H
#define ARBOREUS_A_TIME_RFC850_H

// System include
#include <sys/time.h>
#include <sys/timeb.h>

#ifdef __cplusplus
extern "C" {
#endif

// Functionality
int atRFC850FromStruct(struct tm TimeStruct, char **RFC_850);
int atRFC850FromTimestamp(long long int Timestamp, char **RFC_850);

#ifdef __cplusplus
}
#endif

#endif //ARBOREUS_A_TIME_RFC850_H
