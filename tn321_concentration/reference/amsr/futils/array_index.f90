!------------------------------------------------------------------------------
!
! (C) QSS Corporation
!
!
! NAME:
!       Array_Index
!
! PURPOSE:
!     This function is used to take indexes for a multi-dimensional
!     array and find its position in a one dimensional array.
!
! CATEGORY:
!    Utility
!
! CALLING SEQUENCE:
!    Array_Index(i, isize, j, jsize, k, ksize, l, lsize, m, msize)
!
! INPUTS:
!    i, j, k, l, m -- Index of the dimensions
!    isize, jsize, ksize, lsize, msize -- size of each dimension
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
   INTEGER(LONG) :: Index_2D, Index_3D, Index_4D, Index_5D


!------------------------------------------------------------------------------
!                          -- Data Processing --
!------------------------------------------------------------------------------

   IF (PRESENT(m) .AND. PRESENT(msize)) THEN
      Array_Point = Index_5D (i, isize, j, jsize, k, ksize, l, lsize, m, msize)
   ELSE IF (PRESENT(l) .AND. PRESENT(lsize)) THEN
      Array_Point = Index_4D (i, isize, j, jsize, k, ksize, l, lsize)
   ELSE IF (PRESENT(k) .AND. PRESENT(ksize)) THEN
      Array_Point = Index_3D (i, isize, j, jsize, k, ksize)
   ELSE
      Array_Point = Index_2D (i, isize, j, jsize)
   END IF

END Function Array_Index

Function Index_2D (i, isize, j, jsize) RESULT (Array_Point)

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

   INTEGER(LONG), INTENT(IN)  :: i, isize
   INTEGER(LONG), INTENT(IN)  :: j, jsize
   INTEGER(LONG) :: Array_Point


!------------------------------------------------------------------------------
!                          -- Data Processing --
!------------------------------------------------------------------------------

   Array_Point = (j - 1) * isize + i


END Function Index_2D

Function Index_3D (i, isize, j, jsize, k, ksize) &
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

   INTEGER(LONG), INTENT(IN)  :: i, isize
   INTEGER(LONG), INTENT(IN)  :: j, jsize
   INTEGER(LONG), INTENT(IN)  :: k, ksize
   INTEGER(LONG) :: Array_Point


!------------------------------------------------------------------------------
!                          -- Data Processing --
!------------------------------------------------------------------------------

   Array_Point = (k - 1) * isize * jsize + (j - 1) * isize + i
 
END Function Index_3D

Function Index_4D (i, isize, j, jsize, k, ksize, l, lsize) &
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

   INTEGER(LONG), INTENT(IN)  :: i, isize
   INTEGER(LONG), INTENT(IN)  :: j, jsize
   INTEGER(LONG), INTENT(IN)  :: k, ksize
   INTEGER(LONG), INTENT(IN)  :: l, lsize
   INTEGER(LONG) :: Array_Point


!------------------------------------------------------------------------------
!                          -- Data Processing --
!------------------------------------------------------------------------------

   Array_Point = (l - 1) * isize * jsize * ksize + (k - 1) * isize * jsize &
               + (j - 1) * isize + i
 
END Function Index_4D

Function Index_5D (i, isize, j, jsize, k, ksize, l, lsize, m, msize) &
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

   INTEGER(LONG), INTENT(IN)  :: i, isize
   INTEGER(LONG), INTENT(IN)  :: j, jsize
   INTEGER(LONG), INTENT(IN)  :: k, ksize
   INTEGER(LONG), INTENT(IN)  :: l, lsize
   INTEGER(LONG), INTENT(IN)  :: m, msize
   INTEGER(LONG) :: Array_Point


!------------------------------------------------------------------------------
!                          -- Data Processing --
!------------------------------------------------------------------------------

   Array_Point = (m - 1) * isize * jsize * ksize * lsize &
               + (l - 1) * isize * jsize * ksize + (k - 1) * isize * jsize &
               + (j - 1) * isize + i

END Function Index_5D

