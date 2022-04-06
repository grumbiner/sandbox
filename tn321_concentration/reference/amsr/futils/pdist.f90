!**********************************************************************
!
!   NAME - PDIST
!
!   LANGUAGE- FORTRAN     TYPE- SUBROUTINE
!
!   VERSION- 1.0    DATE- 24 NOV 87   PROGRAMMER- DAVID NIVER (STX)
!
!   FUNCTION - CALCULATES DISTANCE (IN KILOMETERS) BETWEEN
!        TWO LAT/LON PAIRS ON THE EARTH'S SURFACE.
!
!   INPUT PARAMETERS (ALL IN DEGREES):
!      R1_Latitude - LATITUDE OF FIRST POINT
!      R1_Longitude - LONGITUDE OF FIRST POINT
!      R2_Latitude - LATITUDE OF SECOND POINT
!      R2_Longitude - LONGITUDE OF SECOND POINT
!
!   OUTPUT PARAMETER:
!      DIS   - DISTANCE BETWEEN POINTS (IN KILOMETERS)
!
!**********************************************************************
!
!
!    THIS PROGRAM CALCULATES THE DISTANCE BETWEEN ANY TWO POINTS
!    ON THE EARTH'S SURFACE GIVEN THEIR LATITUDE AND LONGITUDE.
!

      SUBROUTINE PDist(R1_Latitude, R1_Longitude, R2_Latitude, R2_Longitude, DIS)

      USE type_kinds
      USE common_parameters

      IMPLICIT NONE

      REAL(SINGLE) :: R1_Latitude
      REAL(SINGLE) :: R1_Longitude
      REAL(SINGLE) :: R2_Latitude
      REAL(SINGLE) :: R2_Longitude

      REAL(SINGLE) :: RADCV, DLON, CFAC, DISKM, R1LAT, R2LAT
      REAL(SINGLE) :: R1LON, R2LON, DIS, CCFAC
      REAL(SINGLE) :: RADERT = 6371.2 

      RADCV = 180.0 / PI

      R1LAT = R1_Latitude
      R1LON = R1_Longitude
      R2LAT = R2_Latitude
      R2LON = R2_Longitude

      R1LAT = (90.0 - R1LAT)/RADCV
      R2LAT = (90.0 - R2LAT)/RADCV
      DLON = ABS(R1LON - R2LON)/RADCV
      CFAC = COS(R1LAT) * COS(R2LAT) + SIN(R1LAT) * SIN(R2LAT) * COS(DLON)
      CCFAC = CFAC
      DISKM = ACOS(CCFAC) * RADERT
      DIS = DISKM

      RETURN

      END SUBROUTINE PDist
