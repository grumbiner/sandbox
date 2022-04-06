/*
 *  This function takes the time from a year/day of year type date and changes 
 *  it to total seconds since January 1, 1970.
 *
 *  Created by Thomas King 
 *             05/19/2005
 *             QSS Group, Inc
 */

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined AIX
void yeardoy_to_seconds (int *Year, int *Doy, int *Hour, 
                         int *Minute, int *Second, double *Total_Seconds)
#elif defined LINUX
void yeardoy_to_seconds_ (int *Year, int *Doy, int *Hour,
                         int *Minute, int *Second, double *Total_Seconds)
#endif

{

/*
A note about yeardoy_to_seconds_:
This function first calculates the number of seconds from Jan 01 of the selected year +
the additional number of seconds from *Hour, *Minute, *Second.  Then the number of days
in the year * 86400 seconds/day are added to that.  I do this because you can't define
Time.tm_yday and hand that off to mktime.  mktime can only generate Time.tm_yday from
first knowing Time.tm_mon and Time.tm_mday.
*/
 
        struct tm Time;
        time_t Local_Seconds;
        int *Day = 0;
        int *Month = 0;

        static char *zone = NULL;

        int theDay = 1;  /* Assume 1st day of the month*/
        int theMonth = 1;  /* Assume January */

        Day = &theDay;
        Month = &theMonth;


        if (!zone) {
           zone = strdup ("TZ=GMT");
           (void) putenv (zone);
        }

        Time.tm_sec      = (time_t) *Second;
        Time.tm_min      = (time_t) *Minute;
        Time.tm_hour     = (time_t) *Hour;

        Time.tm_mday     = (time_t) *Day;
        Time.tm_mon      = (time_t) *Month - 1; 

        Time.tm_year     = (time_t) *Year - 1900;      /* year - 1900 */
        Time.tm_isdst    = 0;

        Local_Seconds = mktime (&Time);

        *Total_Seconds = (double) Local_Seconds;
        *Total_Seconds = *Total_Seconds + ( (*Doy - 1) * 86400);

        return;
}
