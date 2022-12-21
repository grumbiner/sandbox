!------------------------------------------------------------------------------
!
! (C) QSS Group
!
! NAME:
!       Sec_to_Dayhours_Interface
!
! PURPOSE:
!       This is an interface to the Seconds_to_Dayhours function which calculates
!       hours with fractional minutes from milliseconds.
!
! CATEGORY:
!       Function Interface 
!
! CALLING SEQUENCE:
!       USE sec_to_dayhours_interface
!
! INPUTS:
!       None
!
! OUTPUTS:
!       None
!
! CALLS:
!       None
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


MODULE Sec_to_Dayhours_Interface
   INTERFACE

      Function Seconds_to_Dayhours(Seconds_from_Year, Leap_Seconds) RESULT (Sec_to_Dayhrs)

!------------------------------------------------------------------------------
!                          -- Declare modules used --
!------------------------------------------------------------------------------
 
      USE type_kinds
      IMPLICIT NONE

!------------------------------------------------------------------------------
!                           -- Type declarations --
!------------------------------------------------------------------------------
 
      INTEGER(LONG), INTENT(IN) :: Leap_Seconds
      REAL(SINGLE),  INTENT(IN) :: Seconds_from_Year
      REAL(SINGLE)  :: Sec_to_DayHrs

      END FUNCTION Seconds_to_Dayhours
   END INTERFACE
END MODULE Sec_to_Dayhours_Interface
