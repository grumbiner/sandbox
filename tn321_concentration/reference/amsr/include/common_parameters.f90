!==============================================================================
!==============================================================================
!
! (C) 
!
!
! NAME:
!       Common_Parameters 
!
! PURPOSE:
!       Module to hold some of the frequently used constants. 
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       use common_parameters 
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! MODIFICATION HISTORY:
!       Written by:     Thomas King (01/24/2005)
!                       QSS Group
!                       Lanham, MD
!                       Thomas.S.King@noaa.gov
!
!  $Author: $
!  $Date: $
!  $Id: $
!  $Log:  $
!
!
!==============================================================================
!==============================================================================
!
 
MODULE Common_Parameters 

  USE type_kinds
 
  IMPLICIT NONE 
 
  BYTE, PARAMETER :: BAD_UBYTE = 0 
  BYTE, PARAMETER :: BAD_BYTE = -128
  INTEGER(LONG), PARAMETER :: BAD_INT = -9999
  REAL(SINGLE), PARAMETER :: BAD_REAL = -9999.0

  REAL(SINGLE), PARAMETER :: PI = 3.14159265359                     ! pi
  REAL(SINGLE), PARAMETER :: Re = 6371.004                          ! radius in km
  REAL(SINGLE), PARAMETER :: Avogadros_Number = 6.02214199e23       ! # of things/mole
  REAL(SINGLE), PARAMETER :: Gas_Constant = 83140                   ! mb cm**3 K**-1 mole**-1
  REAL(SINGLE), PARAMETER :: Molecular_Wt_DryAir = 28.9644          ! gm mole**-1
  REAL(SINGLE), PARAMETER :: Molecular_Wt_Water = 18.0151           ! gm mole**-1
  REAL(SINGLE), PARAMETER :: Molecular_Wt_Ozone = 47.9982           ! gm mole**-1
  REAL(SINGLE), PARAMETER :: gstd = 980.665                         ! cm/s**2
  REAL(SINGLE), PARAMETER :: BOLTZMNS = 1.380658e-16

  INTEGER(LONG), PARAMETER :: U2_OFFSET = 65536
  INTEGER(DLONG), PARAMETER :: U4_OFFSET = 4294967296_DOUBLE
  
  REAL(SINGLE), PARAMETER :: WaveNumber_to_MHz = 299.8  ! a conversion coefficient to go from 
                                                        ! wavenumber to freq.

END MODULE Common_Parameters 

