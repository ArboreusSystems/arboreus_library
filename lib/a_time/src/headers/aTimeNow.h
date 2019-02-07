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

// Functionality
int aTimeNowMicroseconds();
int aTimeNowMilliseconds();
int aTimeNowSeconds();
int aTimeNowInt();
int aTimeNowIntDate();
int aTimeNowIntFull();
int aTimeNowIntExtend();
int aTimeNowRFC822();
int aTimeNowRFC850();
int aTimeNowANSI();

#endif //ARBOREUS_A_TIME_NOW_H
