/*
 *  This function takes the time from Julian day of year format and changes it 
 *  to year month day format.
 *
 *  Created by Yi Song
 *             4/3/2012
 *             RTI
 */

#include <stdio.h>

#if defined AIX
void julian_to_calendar (int *yyyy, int *jday, int *mm, int *dd) {
for (*mm=1; calendar_to_julian(*yyyy, *mm+1, 1) <= *jday; ++*mm);
for (*dd=1; calendar_to_julian(*yyyy, *mm, *dd+1) <=*jday; ++*dd);
#elif defined LINUX
void julian_to_calendar_ (int *yyyy, int *jday, int *mm, int *dd) {
for (*mm=1; calendar_to_julian_(*yyyy, *mm+1, 1) <= *jday; ++*mm);
for (*dd=1; calendar_to_julian_(*yyyy, *mm, *dd+1) <=*jday; ++*dd);
#endif
//printf("Calendar date is %d %d %d\n",*yyyy, *mm, *dd);
return;
} 
