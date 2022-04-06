!==============================================================================
!==============================================================================
!
! QSS Group
!
!
! NAME:
!       Position_Index_Module
!
! PURPOSE:
!       Module to hold the inputs for where to start and skip 
!       for a number of arrays
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       use postion_index_module
!
! GLOBAL VARIABLES:
!       None
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! MODIFICATION HISTORY:
!       Written by:     Walter Wolf, QSS Group 29-Feb-2000
!
!  $Author: $
!  $Date: $
!  $Id: $
!  $Log: $
!
!
!
!==============================================================================
!==============================================================================
!

 
MODULE Position_Index_Module

  USE type_kinds
  IMPLICIT NONE

  TYPE Position_Index_Record

     INTEGER(LONG) :: Scan_Start
     INTEGER(LONG) :: Scan_Skip
     INTEGER(LONG) :: Channel_Start
     INTEGER(LONG) :: Channel_Skip
     INTEGER(LONG) :: FOV_Start
     INTEGER(LONG) :: FOV_Skip
     INTEGER(LONG) :: Scans_to_Process
      
  END TYPE Position_Index_Record

END MODULE Position_Index_Module

