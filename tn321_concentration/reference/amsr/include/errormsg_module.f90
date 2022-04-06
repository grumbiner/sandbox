!==============================================================================
!==============================================================================
!
! Space Science and Engineering Center, University of Wisconsin-Madison
!
!
! NAME:
!       errormsg_module
!
! PURPOSE:
!       Module to hold the error messaging definitions
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       use errormsg_module
!
! GLOBAL VARIABLES:
!       NOTICE  = 0  (Continue Processing, should be ok)
!       WARNING = 1  (Continue Processing at your own risk)
!       FATAL   = 2  (Exit the Program, major problems)
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! MODIFICATION HISTORY:
!       Written by:     Walter Wolf, CIMSS/SSEC 03-Nov-1998
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
 
  
MODULE Errormsg_Module
 
  USE type_kinds

  IMPLICIT NONE
                                                                                                            
  INTEGER(LONG), PARAMETER :: NOTICE   = 0   ! Continue Processing
  INTEGER(LONG), PARAMETER :: WARNING  = 1   ! Process at your own risk
  INTEGER(LONG), PARAMETER :: FATAL    = 2   ! Stop Processing
 
END MODULE Errormsg_Module

