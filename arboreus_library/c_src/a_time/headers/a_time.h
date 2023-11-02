/* -------------------------------------------------------------------
 *  @doc Arboreus UNIX current time handler headers
 *  @notice
 *
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 12/23/2018 at 20:49
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_A_TIME_NOW_H
#define ARBOREUS_A_TIME_NOW_H

#ifdef __cplusplus
extern "C" {
#endif

// Functionality
int aTimeNowMicroseconds(void);
int aTimeNowMilliseconds(void);
int aTimeNowSeconds(void);
int aTimeNowInt(void);
int aTimeNowIntDate(void);
int aTimeNowIntFull(void);
int aTimeNowIntExtend(void);
int aTimeNowRFC822(void);
int aTimeNowRFC850(void);
int aTimeNowANSI(void);

#ifdef __cplusplus
}
#endif

#endif //ARBOREUS_A_TIME_NOW_H
