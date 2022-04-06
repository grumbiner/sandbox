!==============================================================================
!==============================================================================
!
! (C) QSS Group
!
!
! NAME:
!       Gas_Units_Module
!
! PURPOSE:
!       A module to hold the parameters defining the units for gases.
!
! CATEGORY:
!       Dynamic data storage
!
! CALLING SEQUENCE:
!       USE Gas_Units_Module
!
! KNOWN BUGS AND LIMITATIONS:
!       None known.
!
! MODIFICATION HISTORY:
!       Written by:     Thomas King (08/11/2006)
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
!
!==============================================================================
!==============================================================================
!
  
MODULE Gas_Units_Module
  
  
!------------------------------------------------------------------------------
!       -- Declare type_kinds module and turn off implicit typing --
!------------------------------------------------------------------------------
  
  USE type_kinds
 
  IMPLICIT NONE

  INTEGER(LONG), PARAMETER :: LAYER_COLUMN_DENSITY = 1 ! molecules/cm**2 layer
  INTEGER(LONG), PARAMETER :: PPMV = 2                 ! micromoles/mole
  INTEGER(LONG), PARAMETER :: KGKG = 3                 ! kg/kg
  INTEGER(LONG), PARAMETER :: KGM3 = 4                 ! kg/m**3

END MODULE Gas_Units_Module
