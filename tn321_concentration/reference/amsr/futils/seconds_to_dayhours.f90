!------------------------------------------------------------------------------
!
! (C) QSS Group
!
! NAME:
!       Seconds_to_Dayhours
!
! PURPOSE:
!       This is a function which calculates hours with fractional minutes from milliseconds.
!
! CATEGORY:
!       Function Interface
!
! CALLING SEQUENCE:
!       USE sec_to_dayhours_interface
!
! INPUTS:
!       Seconds_from_Year - Milliseconds from any start date
!       Leap_Seconds - The number of leap seconds to subtract.  Just put 0 for UTC times.
!
! OUTPUTS:
!       Sec_to_Dayhrs - The hours (of the current day) with fractional minutes from milliseconds.
!
! CALLS:
!       Sec_to_Dayhrs = Seconds_to_DayHours(Seconds_from_Year, Leap_Seconds)
!
! MODULES:
!       type_kinds:      Module containing definitions for kinds of variable
!                        types
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! EXAMPLE:
!
!
! MODIFICATION HISTORY:
!       Written by:   Thomas King  (05/05/2005)
!                     QSS Group
!                     Lanham, MD
!                     Thomas.S.King@noaa.gov
!
!  $Date:$
!  $Id:$
!  $Log:$
!------------------------------------------------------------------------------

FUNCTION Seconds_to_DayHours (Seconds_from_Year, Leap_Seconds) RESULT (Sec_to_Dayhrs)

!
!  Determine the fractional hours of the day.
!

   USE type_kinds
   IMPLICIT NONE

   INTEGER(LONG), INTENT(IN) :: Leap_Seconds
   REAL(SINGLE),  INTENT(IN) :: Seconds_from_Year
   REAL(SINGLE)  :: Sec_to_DayHrs

   INTEGER(LONG) :: Seconds, Minutes, Hours
   REAL(SINGLE) :: New_Seconds
   REAL(SINGLE) :: Temp

   Temp = Seconds_from_Year - Leap_Seconds

!  Calculate the Seconds of the day

   New_Seconds = Temp / 1000.0
   Seconds = MOD (INT(New_Seconds), 60)

!  Calculate the Minutes of the day

   New_Seconds = New_Seconds / 60.0
   Minutes = MOD (INT(New_Seconds), 60)

!  Calculate the Hours of the day

   New_Seconds = New_Seconds / 60.0
   Hours = MOD (INT(New_Seconds), 24)
!  Convert the fractional Hours of the day

   Sec_to_DayHrs = REAL(HOURS) + REAL(Minutes) / 60.0

   RETURN
END FUNCTION
