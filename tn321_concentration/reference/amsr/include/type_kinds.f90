!==============================================================================
!==============================================================================
!
! Space Science and Engineering Center, University of Wisconsin-Madison
!
!
! NAME:
!       type_kinds
!
! PURPOSE:
!       Module to hold specification kinds, with parameter attributes, for
!       variable declarations.
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       use type_kinds
!
! GLOBAL VARIABLES:
!       Byte   - Specification kind for byte integer variable
!       Short  - Specification kind for short integer variable
!       Long   - Specification kind for long integer variable
!       Single - Specification kind for single precision real variable
!       Double - Specification kind for double precision real variable
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! MODIFICATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Jan-1996
!
!  $Author: walterw $
!  $Date: 1998/06/17 19:54:55 $
!  $Id: type_kinds.f,v 1.1 1998/06/17 19:54:55 walterw Exp $
!  $Log: type_kinds.f,v $
!  Revision 1.1  1998/06/17 19:54:55  walterw
!  Initial revision
!
!  Revision 1.1  1997/12/01 20:21:50  paulv
!  Initial revision
!
!
!
!==============================================================================
!==============================================================================
!
 
  
MODULE type_kinds
 
  IMPLICIT NONE 
 
  INTEGER, PARAMETER :: BYTE   = SELECTED_INT_KIND(1), &   ! Byte integer
                        SHORT  = SELECTED_INT_KIND(4), &   ! Short integer
                        LONG   = SELECTED_INT_KIND(8), &   ! Long integer
                        DLONG  = SELECTED_INT_KIND(10), &  ! Double Long integer
                        SINGLE = SELECTED_REAL_KIND(6), &  ! Single precision
                        DOUBLE = SELECTED_REAL_KIND(12)    ! Double precision
 
END MODULE type_kinds

