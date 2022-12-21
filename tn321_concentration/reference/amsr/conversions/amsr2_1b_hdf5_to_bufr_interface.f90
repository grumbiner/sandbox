!------------------------------------------------------------------------------
!
! NAME:
!     AMSR2_1B_hdf5_to_bufr_interface.f90
!
! PURPOSE:
!     This is a module interface for the AMSR2_1B_hdf5_to_bufr subroutine.
!
! CATEGORY:
!     Subroutine interface
!
! CALLING SEQUENCE:
!     use AMSR2_1B_hdf5_to_bufr_interface
!
! INPUTS:
!     None
!
! OUTPUTS:
!     None
!
! CALLS:
!     None
!
! MODULES:
!    type_kinds
!    errormsg_module
!    common_parameters
!    hdf5_defines
!    read_hdf5_interface
!
! MODIFICATION HISTORY:
!
!       Written by: Yi Song (09/30/2013)
!                    IMSG
!                    Yi.Song@noaa.gov
!
!  $Date:$
!  $Id:$
!  $Log:$
!
!------------------------------------------------------------------------------
!

MODULE AMSR2_1B_hdf5_to_bufr_interface

INTERFACE

   SUBROUTINE AMSR2_1B_hdf5_to_bufr (Unit_Number_Resource_File)

!
! Declare modules used
!

   USE type_kinds
   USE errormsg_module
   USE common_parameters
   USE HDF5
   USE hdf5_interface
   USE amsr2_1b_module

   IMPLICIT NONE

!
!  Variable declarations
!

   INTEGER(LONG) :: Unit_Number_Resource_File

   END SUBROUTINE AMSR2_1B_hdf5_to_bufr

END INTERFACE

END MODULE AMSR2_1B_hdf5_to_bufr_interface
