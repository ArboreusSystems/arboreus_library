/* -------------------------------------------------------------------
 *  @doc Arboreus RFC822 time format handler headers
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 01/07/2019 at 16:44
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_A_TIME_RFC822_H
#define ARBOREUS_A_TIME_RFC822_H

// System include
#include <sys/time.h>
#include <sys/timeb.h>

// Functionality
int atRFC822FromStruct(struct tm TimeStruct, char **RFC_822);
int atRFC822FromTimestamp(long long int Timestamp, char **RFC22);

#endif //ARBOREUS_A_TIME_RFC822_H
