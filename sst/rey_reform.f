      PROGRAM reyread
      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 1440)
      PARAMETER (ny =  720)

      INTEGER*4 year, month, day
! IEEE, big-endian
      INTEGER*2 sst(nx, ny), sstanom(nx, ny), errors(nx, ny)
      INTEGER*2 icec(nx, ny)
! Anomaly w.r.t. 1971-2000
! SST and anomalies are in 0.01 degrees, multiply by that
! icec is 0-100
! missing = -999
      INTEGER missing
      REAL sstout(nx, ny)

      missing = -999

      OPEN(10, FILE="fort.10", FORM="UNFORMATTED", STATUS="OLD")

      READ (10) year, month, day, sst
!      PRINT *,year, month, day, " sst max min ",
!     1                  MAXVAL(sst), MINVAL(sst)
      READ (10) year, month, day, sstanom
!      PRINT *,year, month, day, " sstanom max min ",
!     1                  MAXVAL(sstanom), MINVAL(sstanom)
      READ (10) year, month, day, errors
!      PRINT *,year, month, day, " err max min ",
!     1                  MAXVAL(errors), MINVAL(errors)
      READ (10) year, month, day, icec
!      PRINT *,year, month, day, " icec max min ",
!     1                  MAXVAL(icec), MINVAL(icec)

      sstout = 0.01 * sst
      PRINT *,year, month, day, " sst max min ",
     1                  MAXVAL(sstout), MINVAL(sstout)
      OPEN(12, FORM="UNFORMATTED", STATUS="NEW")
      WRITE(12) sstout
      
      END
