!------------------------------------------------------------------------------
!
!
! NAME:
!       YearMonthDay_to_DOY_Interface
!
! PURPOSE:
!       This is an interface to the YearMonthDay_to_DOY function
!
! CATEGORY:
!       Function Interface
!
! CALLING SEQUENCE:
!       USE yearmonthday_to_DOY_interface
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

MODULE YearMonthDay_to_DOY_Interface
  INTERFACE

     Function YearMonthDay_to_DOY (Year, Month, Day) RESULT (DOY)

!------------------------------------------------------------------------------
!                          -- Declare modules used --
!------------------------------------------------------------------------------
 
       USE type_kinds
       IMPLICIT NONE

!------------------------------------------------------------------------------
!                           -- Type declarations --
!------------------------------------------------------------------------------
 

       INTEGER(LONG), intent(in) :: Year
       INTEGER(LONG), intent(in) :: Month
       INTEGER(LONG), intent(in) :: Day
       INTEGER(LONG) :: DOY

     END FUNCTION YearMonthDay_to_DOY

  END INTERFACE
END MODULE YearMonthDay_to_DOY_Interface
