      PROGRAM dummy
! Set up a dummy analysis and data file for testing scaling of 
!   Cressman-like program
      IMPLICIT none

      INTEGER nx, ny, nk
      PARAMETER (nx = 360*2)
      PARAMETER (ny = 180*2)
      REAL first(nx, ny)

      INTEGER maxobs
      PARAMETER (maxobs = 1000*1000*10)
      REAL lat(maxobs), lon(maxobs), obs(maxobs)
      REAL rgrand, ran2

      INTEGER i, lim
      
      lat = 0.0
      lon = 0.0
      obs = 0.0
      OPEN (11, FILE="obs",FORM="UNFORMATTED", STATUS="UNKNOWN")
      DO i = 1, maxobs
        READ(11) lat(i), lon(i), obs(i)
!        PRINT *,lat(i), lon(i), obs(i)
      ENDDO
      PRINT *,'lat ', maxval(lat), minval(lat)
      PRINT *,'lon ', maxval(lon), minval(lon)
      PRINT *,'obs ', maxval(obs), minval(obs)

      CLOSE(11)

      STOP
      END
