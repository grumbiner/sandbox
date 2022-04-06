/*
 *  This function takes the time from a calender date and changes it to
 *  total seconds since January 1, 1970.
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
void convert_to_seconds (int *Year, int *Month, int *Day, int *Hour, 
                         int *Minute, int *Second, double *Total_Seconds)
#elif defined LINUX 
void convert_to_seconds_ (int *Year, int *Month, int *Day, int *Hour, 
                         int *Minute, int *Second, double *Total_Seconds)
#endif

{
        struct tm Time;
        time_t Local_Seconds;
        static char *zone = NULL;

        if (!zone) {
           zone = strdup ("TZ=GMT");
           (void) putenv (zone);
        }

        Time.tm_sec      = (time_t) *Second;
        Time.tm_min      = (time_t) *Minute;
        Time.tm_hour     = (time_t) *Hour;
        Time.tm_mday     = (time_t) *Day;
        Time.tm_mon      = (time_t) *Month - 1;        /* starts at 0 */
        Time.tm_year     = (time_t) *Year % 1900;      /* year - 1900 */
        Time.tm_isdst    = 0;

        Local_Seconds = mktime (&Time);
 
        *Total_Seconds = (double) Local_Seconds;
        return;
}

