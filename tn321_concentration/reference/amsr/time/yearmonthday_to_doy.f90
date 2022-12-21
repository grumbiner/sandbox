!------------------------------------------------------------------------------
!
! NAME:
!       YearMonthDay_to_DOY
!
! PURPOSE:
!       This is a function which calculates calender date to day of year
!
! CATEGORY:
!       Function Interface
!
! CALLING SEQUENCE:
!       USE YearMonthDay_to_DOY_interface
!
! INPUTS:
!       Year - Calender year
!       Month - Calender month
!       Day - Calender day
!
! OUTPUTS:
!       DOY - day of year
!
! CALLS:
!       DOY = YearMonthDay_to_DOY(Year, Month, Day)
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
!       Written by:   Wen Zhou  (03/15/2006)
!                     QSS Group
!                     Lanham, MD
!                     Wen.Zhou@noaa.gov
!
!  $Date:$
!  $Id:$
!  $Log:$
!------------------------------------------------------------------------------

FUNCTION YearMonthDay_to_DOY (Year, Month, Day) RESULT (DOY)

   USE type_kinds

   IMPLICIT NONE

!
! EXAMPLE: YearMonthDay(1984, 4, 22) = 113 
!

  INTEGER(LONG), INTENT(IN) :: Year, Month, Day

  INTEGER(LONG) :: DOY

  DOY = 3055*(Month+2)/100 - (Month+10)/13*2 -91 + (1-(MOD(Year, 4)+3)/4 +  &

       (MOD(Year, 100) + 99)/100 - (MOD(Year, 400)+399)/400)*(Month+10)/13 + Day 

  RETURN

END FUNCTION YearMonthDay_to_DOY
