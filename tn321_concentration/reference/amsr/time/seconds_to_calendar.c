/*
 *  This function takes the time from total seconds since January 1, 1970
 *  and changes it to a calendar date.
 *
 *  Created by Walter Wolf
 *             11/1/2000
 *             QSS Group, Inc
 */

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined AIX
void seconds_to_calendar (double *Total_Seconds, int *Year, int *Month, 
                          int *Day, int *Hour, int *Minute, int *Second)
#elif defined LINUX 
void seconds_to_calendar_ (double *Total_Seconds, int *Year, int *Month,
                          int *Day, int *Hour, int *Minute, int *Second)
#endif

{
        struct tm TIME;
        time_t Local_Seconds;
        int Status;
        static char *zone = NULL;

        if (!zone) {
           zone = strdup ("TZ=GMT");
           (void) putenv (zone);
        }

        Local_Seconds = (time_t) *Total_Seconds;
        TIME = *gmtime (&Local_Seconds);

        *Second = TIME.tm_sec;
        *Minute = TIME.tm_min;
        *Hour   = TIME.tm_hour;
        *Day    = TIME.tm_mday;
        *Month  = TIME.tm_mon + 1;            /* starts at 0 */
        *Year   = TIME.tm_year + 1900;  /* year - 1900 */

        return;
}
