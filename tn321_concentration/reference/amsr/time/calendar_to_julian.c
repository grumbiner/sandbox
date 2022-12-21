/*
 *  This function takes the time from year month day format and changes it to
 *  Julilan day.
 *
 *  Created by Yi Song
 *             4/3/2012
 *             RTI 
 */

#if defined AIX
int calendar_to_julian (int yyyy, int mm, int dd) {
#elif defined LINUX
int calendar_to_julian_ (int yyyy, int mm, int dd) {
#endif
int julian;
julian=dd;
mm=mm-1;
 switch(mm)
{
case 11 : julian=julian+30;
case 10 : julian=julian+31;
case 9 : julian=julian+30;
case 8 : julian=julian+31;
case 7 : julian=julian+31;
case 6: julian=julian+30;
case 5: julian=julian+31;
case 4 : julian=julian+30;
case 3 : julian=julian+31;
case 2 : if(yyyy%4==0)julian=julian+29;
             else julian=julian+28;
case 1 : julian=julian+31;
}
return julian;
} 
