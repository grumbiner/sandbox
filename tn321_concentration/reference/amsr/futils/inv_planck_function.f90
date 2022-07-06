!------------------------------------------------------------------------------
!
! (C) QSS Group, Inc, 2000
!
!
! NAME:
!       inv_planck_function
!
! PURPOSE:
!       Function to calculate temperature from frequency and radiance.
!       Units of radiance are mw/ster/m/m/(cm-1)
!
! CATEGORY:
!       General
!
! CALLING SEQUENCE:
!       CALL inv_planck_function (radiance, frequency)
!
! INPUTS:
!       Radiance: Radiance Values (mW/m2/cm-1/sr)
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


Function Inv_Planck_Function(Radiance, Frequency) RESULT(Brightness_Temperature)

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
   REAL(SINGLE), INTENT(IN)  :: Radiance

   REAL(SINGLE) :: Brightness_Temperature

   REAL(DOUBLE), PARAMETER :: Avagadros_Number = 6.02214199E+23_double
   REAL(DOUBLE), PARAMETER :: Boltzmans_Constant = 1.3806503E-23_double
   REAL(DOUBLE), PARAMETER :: Plancks_Constant = 6.62606876E-34_double
   REAL(DOUBLE), PARAMETER :: Speed_of_Light = 2.99792458E+10_double

   REAL(DOUBLE) :: Constant1, Constant2

   Constant1 = 2.0_double * Plancks_Constant * Speed_of_Light &
             * Speed_of_Light * 1.0E7_double
   Constant2 = Plancks_Constant * Speed_of_Light / Boltzmans_Constant

   IF (Radiance > 0) THEN
      Brightness_Temperature = Constant2 * Frequency &
                             / LOG (1.0_double + Constant1 * Frequency * Frequency & 
                             * Frequency / Radiance)
   ELSE
      Brightness_Temperature = BAD_REAL 
   END IF

   RETURN
END
