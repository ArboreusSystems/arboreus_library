/* -------------------------------------------------------------------
 *  @doc UTF-8 String symbols handler headers
 *  @notice
 *  
 *  @copyright Arboreus (http://arboreus.systems)
 *  @author Alexandr Kirilov (http://alexandr.kirilov.me)
 *  @created 02/01/2019 at 15:39
 * */// --------------------------------------------------------------

#ifndef ARBOREUS_ASYMBOL_H
#define ARBOREUS_ASYMBOL_H

int asymIsEqual(int Symbol,int UTFNumber);

int asymIsLatinLower(int Symbol);
int asymIsLatinUpper(int Symbol);
int asymIsLatin(int Symbol);
int asymIsNumeric(int Symbol);
int asymIsAt(int Symbol);
int asymIsDot(int Symbol);
int asymIsSpace(int Symbol);

#endif //ARBOREUS_ASYMBOL_H
