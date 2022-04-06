!==============================================================================
!==============================================================================
!
! NAME:
!       amsr2_1b_module
!
! DESCRIPTION:
!       A module to hold the variable declarations and initialization routine
!       for AMSR2 1B related data.
!
! FUNCTION:
!       Dynamic data storage
!
! REFERENCE:
!       None
!
! CALLING SEQUENCE:
!       USE amsr2_1b_module
!
! DEPENDENCIES:
!       type_kinds
!
!
! MODIFICATION HISTORY:
!       Written by:     Yi SOng, IMSG, 08/02/2013
!                       Yi.Song@noaa.gov
!
!
!==============================================================================
!==============================================================================
!

MODULE amsr2_1b_module

   !---------------------------------------------------------------------------
   !       -- Declare type_kinds module and turn off implicit typing --
   !---------------------------------------------------------------------------

   USE type_kinds
   USE common_parameters

   IMPLICIT NONE


   !
   !###########################################################################
   !
   !               HEADER RECORD
   !
   !###########################################################################
   !

   !
   ! Define the amsr2_1b data structure
   !

   TYPE :: amsr2_1b_record

      ! Character Strings

      CHARACTER(250) :: HDF5FileName   ! Name of input HDF5 file for read
      CHARACTER(250) :: FileName       ! Name of input/output NetCDF/HDF5 file
      CHARACTER(100) :: group          ! HDF5 group name

      ! Allocation Return Status

      INTEGER(LONG) :: Allocate_Status
      INTEGER(LONG) :: Deallocate_Status

      INTEGER(LONG) :: AlongTrack
      INTEGER(LONG) :: CrossTrack1
      INTEGER(LONG) :: CrossTrack2
      INTEGER(LONG) :: Sector1
      INTEGER(LONG) :: Sector2
      INTEGER(LONG) :: Sector3
  
      REAL(SINGLE) :: BT_107GHz_H_SF
      REAL(SINGLE) :: BT_107GHz_V_SF
      REAL(SINGLE) :: BT_187GHz_H_SF
      REAL(SINGLE) :: BT_187GHz_V_SF 
      REAL(SINGLE) :: BT_238GHz_H_SF
      REAL(SINGLE) :: BT_238GHz_V_SF
      REAL(SINGLE) :: BT_365GHz_H_SF 
      REAL(SINGLE) :: BT_365GHz_V_SF
      REAL(SINGLE) :: BT_69GHz_H_SF 
      REAL(SINGLE) :: BT_69GHz_V_SF
      REAL(SINGLE) :: BT_73GHz_H_SF
      REAL(SINGLE) :: BT_73GHz_V_SF
      REAL(SINGLE) :: Earth_Azimuth_SF
      REAL(SINGLE) :: Earth_Incidence_SF
      REAL(SINGLE) :: Sun_Azimuth_SF
      REAL(SINGLE) :: Sun_Elevation_SF
      REAL(SINGLE) :: BT_890GHzA_H_SF
      REAL(SINGLE) :: BT_890GHzA_V_SF
      REAL(SINGLE) :: BT_890GHzB_H_SF
      REAL(SINGLE) :: BT_890GHzB_V_SF
      REAL(SINGLE) :: Latitude_89A_SF
      REAL(SINGLE) :: Latitude_89B_SF 
      REAL(SINGLE) :: Longitude_89A_SF
      REAL(SINGLE) :: Longitude_89B_SF
      REAL(SINGLE) :: Land_Ocean_6_SF
      REAL(SINGLE) :: Land_Ocean_89_SF 
      REAL(SINGLE) :: Scan_Time_SF 

      !
      ! Allocatable Arrays
      !

      ! The following arrays are allocatable

      !
      ! ARRAY_SIZE = AlongTrack * CrossTrack1
      !

      ! This is the first array

      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_107GHz_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_107GHz_V
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_187GHz_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_187GHz_V
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_238GHz_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_238GHz_V
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_365GHz_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_365GHz_V
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_69GHz_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_69GHz_V
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_73GHz_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_73GHz_V
      INTEGER(SHORT), DIMENSION(:), POINTER :: Earth_Azimuth
      INTEGER(SHORT), DIMENSION(:), POINTER :: Earth_Incidence
      INTEGER(SHORT), DIMENSION(:), POINTER :: Sun_Azimuth
      INTEGER(SHORT), DIMENSION(:), POINTER :: Sun_Elevation
      !
      ! ARRAY_SIZE = AlongTrack * CrossTrack2
      !
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_890GHzA_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_890GHzA_V
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_890GHzB_H
      INTEGER(SHORT), DIMENSION(:), POINTER :: BT_890GHzB_V
      REAL(SINGLE), DIMENSION(:), POINTER :: Latitude_for_89A
      REAL(SINGLE), DIMENSION(:), POINTER :: Latitude_for_89B
      REAL(SINGLE), DIMENSION(:), POINTER :: Longitude_for_89A
      REAL(SINGLE), DIMENSION(:), POINTER :: Longitude_for_89B
      INTEGER(SHORT), DIMENSION(:), POINTER :: Pixel_Data_Quality_6_to_36
      INTEGER(SHORT), DIMENSION(:), POINTER :: Pixel_Data_Quality_89
      !
      ! ARRAY_SIZE = Sector1 * AlongTrack * CrossTrack1
      !
      INTEGER(SHORT), DIMENSION(:), POINTER :: Land_Ocean_Flag_6_to_36
      !
      ! ARRAY_SIZE = Sector2 * AlongTrack * CrossTrack2
      !
      INTEGER(SHORT), DIMENSION(:), POINTER :: Land_Ocean_Flag_89
      !
      ! ARRAY_SIZE = AlongTrack * Sector3
      !
      INTEGER(SHORT), DIMENSION(:), POINTER :: Scan_Data_Quality

      !
      ! ARRAY_SIZE = AlongTrack 
      !
      ! This is the last array

      REAL(DOUBLE), DIMENSION(:), POINTER :: Scan_Time

   END TYPE amsr2_1b_record
END MODULE amsr2_1b_module
