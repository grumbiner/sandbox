!------------------------------------------------------------------------------
!
! (C) QSS Corporation
!
!
! NAME:
!       Array_Index
!
! PURPOSE:
!     This subroutine is used to take indexes for a multi-dimensional
!     array and find its position in a one dimensional array.
!
! CATEGORY:
!    Utility
!
! CALLING SEQUENCE:
!    Array_Index(i, isize, j, jsize, k, ksize, l, lsize)
!
! INPUTS:
!    i, j, k, l -- Index of the dimensions
!    isize, jsize, ksize, lsize -- size of each dimension
!
! OUTPUTS:
!    Array_Point -- One dimensional array position
!
! CALLS:
!    Error_Messaging
!
! MODULES:
!    type_kinds
!
! SIDE EFFECTS:
!    None known
!
! RESTRICTIONS:
!    None
!
! PROCEDURE:
!
! EXAMPLE:
!
! MODIFICATION HISTORY:
!       Written by:   Walter Wolf  (01/10/2000)
!                     QSS Corporation
!                     Lanham, MD
!
!  $Date:$
!  $Id:$
!  $Log:$
!
!------------------------------------------------------------------------------

MODULE Array_Index_Interface
   INTERFACE
      Function Array_Index (i, isize, j, jsize, k, ksize, l, lsize, m, msize) &
               RESULT (Array_Point)

!------------------------------------------------------------------------------
!                          -- Declare modules used --
!------------------------------------------------------------------------------

         USE type_kinds
         USE errormsg_module
         IMPLICIT NONE

!------------------------------------------------------------------------------
!                          -- Variable Definitions --
!------------------------------------------------------------------------------

!
!  Inputs
!

         INTEGER(LONG), INTENT(IN) :: i, isize
         INTEGER(LONG), INTENT(IN) :: j, jsize
         INTEGER(LONG), OPTIONAL, INTENT(IN) :: k, ksize
         INTEGER(LONG), OPTIONAL, INTENT(IN) :: l, lsize
         INTEGER(LONG), OPTIONAL, INTENT(IN) :: m, msize
         INTEGER(LONG) :: Array_Point

      END FUNCTION Array_Index
   END INTERFACE
END MODULE
