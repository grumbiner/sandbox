!------------------------------------------------------------------------------
!
! (C) QSS Group, August 2000
!
!  Interface for the inverse planck function.
!
! NAME:
!       inv_planck_function
!
! PURPOSE:
!       Function to calculate Brightness temperature from frequency and 
!       radiance.  Units of radiance are mw/ster/m/m/(cm-1)
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       CALL inv_planck_function (radiance, frequency)
!
! INPUTS:
!       Radiance: Radiance Values of measurement (mW/m2/cm-1/sr)
!       Frequency: Frequency at which the measurement was taken (wavenumbers)
!
! OUTPUTS:
!       Brightness Temperature of the measurement
!
! CALLS:
!       None
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
!------------------------------------------------------------------------------


MODULE Inv_Planck_Interface
   INTERFACE

      FUNCTION Inv_Planck_Function(Radiance, Frequency) &
               RESULT(Brightness_Temperature)

!------------------------------------------------------------------------------
!                          -- Declare modules used --
!------------------------------------------------------------------------------
 
      USE type_kinds
      IMPLICIT NONE

!------------------------------------------------------------------------------
!                           -- Type declarations --
!------------------------------------------------------------------------------
 
      REAL(SINGLE), INTENT(IN)  :: Frequency
      REAL(SINGLE), INTENT(IN)  :: Radiance

      REAL(SINGLE) :: Brightness_Temperature

      END FUNCTION Inv_Planck_Function
   END INTERFACE
END MODULE Inv_Planck_Interface
