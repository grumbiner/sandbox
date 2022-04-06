!------------------------------------------------------------------------------
!
! (C) QSS Group, August 2000
!
!  Interface for the planck function.
!
! NAME:
!       planck_function
!
! PURPOSE:
!       Function to calculate radiance from frequency and temperature.
!       Units of radiance are mw/ster/m/m/(cm-1)
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       CALL planck_function (brightness_temperature, frequency)
!
! INPUTS:
!       Brightness_Temp: Temperure of measurement (K)
!       Frequency: Frequency at which the measurement was taken (wavenumbers)
!
! OUTPUTS:
!       Radiance of the measurement
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


MODULE Planck_Interface
   INTERFACE

      Function Planck_Function(Brightness_Temp, Frequency) RESULT(Radiance)

!------------------------------------------------------------------------------
!                          -- Declare modules used --
!------------------------------------------------------------------------------
 
      USE type_kinds
      IMPLICIT NONE

!------------------------------------------------------------------------------
!                           -- Type declarations --
!------------------------------------------------------------------------------
 
      REAL(SINGLE), INTENT(IN)  :: Frequency
      REAL(SINGLE), INTENT(IN)  :: Brightness_Temp

      REAL(SINGLE) :: Radiance

      END FUNCTION Planck_Function
   END INTERFACE
END MODULE Planck_Interface
