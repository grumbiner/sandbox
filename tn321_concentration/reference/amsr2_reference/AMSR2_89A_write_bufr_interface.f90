!------------------------------------------------------------------------------
!
! (C) PSGS
!
!
! NAME:
!       AMSR2_89A_write_bufr_interface.f90
!
! PURPOSE:
!       This is a module interface for the AMSR2_89A_write_bufr subroutine.
!
! CATEGORY:
!       Subroutine interface
!
! CALLING SEQUENCE:
!       use AMSR2_89A_write_bufr_interface
!
! INPUTS:
!      None
!
! OUTPUTS:
!      None
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! CALLS:
!    OPENBF
!    MAXOUT
!    OPENMB
!    UFBSEQ
!    DRFINI
!    WRITCP
!    CLOSMG
!    CLOSBF
!    seconds_to_calendar
!
! MODULES:
!    type_kinds
!    common_parameters
!    nucaps_all_module
!
! SIDE EFFECTS:
!       None known
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!
! EXAMPLE:
!
! MODIFICATION HISTORY:
!
!       Written by: Yi Song (10/15/2013)
!                    IMSG
!                    Yi.Song@noaa.gov
!
!------------------------------------------------------------------------------
!

MODULE AMSR2_89A_write_bufr_interface

INTERFACE

   SUBROUTINE AMSR2_89A_write_bufr(BUFR_FileName,BUFR_Table_FileName,ORBN,AggregateBeginningTime)

!
! Declare modules used
!

   USE type_kinds
   USE errormsg_module
   USE common_parameters
   USE amsr2_1b_module

   IMPLICIT NONE

!
!  Variable declarations
!

   TYPE (amsr2_1b_record) :: amsr2_1b_rec
   CHARACTER(250) :: BUFR_FileName
   CHARACTER(250) :: BUFR_Table_FileName
   CHARACTER(250) :: AggregateBeginningTime
   INTEGER(LONG) ::  ORBN
   END SUBROUTINE AMSR2_89A_write_bufr

END INTERFACE

END MODULE AMSR2_89A_write_bufr_interface

