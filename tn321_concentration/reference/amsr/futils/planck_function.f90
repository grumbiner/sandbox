!------------------------------------------------------------------------------
!
! (C) QSS Group, Inc, 2000
!
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


FUNCTION Planck_Function(Brightness_Temp, Frequency) RESULT(Radiance)

!------------------------------------------------------------------------------
!                          -- Declare modules used --
!------------------------------------------------------------------------------
 
   USE type_kinds
   USE common_parameters

!------------------------------------------------------------------------------
!                           -- Type declarations --
!------------------------------------------------------------------------------
 
   IMPLICIT NONE
 
 
! ---------
! Arguments
! ---------

   REAL(SINGLE), INTENT(IN)  :: Frequency
   REAL(SINGLE), INTENT(IN)  :: Brightness_Temp

   REAL(SINGLE) :: Radiance

   REAL(DOUBLE), PARAMETER :: Avagadros_Number = 6.02214199E+23_double 
   REAL(DOUBLE), PARAMETER :: Boltzmans_Constant = 1.3806503E-23_double
   REAL(DOUBLE), PARAMETER :: Plancks_Constant = 6.62606876E-34_double
   REAL(DOUBLE), PARAMETER :: Speed_of_Light = 2.99792458E+10_double

   REAL(DOUBLE) :: Constant1, Constant2

   Constant1 = 2.0_double * Plancks_Constant * Speed_of_Light * Speed_of_Light &
             * 1.0E7_double
   Constant2 = Plancks_Constant / Boltzmans_Constant * Speed_of_Light

   IF(Brightness_Temp > 0 )THEN

      Radiance = Constant1 * Frequency * Frequency * Frequency &
            / (EXP(REAL(Constant2 * Frequency / Brightness_Temp)) - 1.0_double)

   ELSE

      Radiance = BAD_REAL

   ENDIF

   RETURN
   END
