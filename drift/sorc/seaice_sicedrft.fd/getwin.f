      SUBROUTINE getwin(ua, va, uunit, vunit, nlat, nlon) 
C  Get the 10 m winds for use by the drift routine(s)
C  This being a subroutine rather than embedded in the main 
C    program is largely a matter of historical legacy, from
C    the days when the drift program computed its own 
C    geostrophic winds, rather than relying on an external
C    program to manage the winds, and using 10 m winds.
C  Flip winds from mrf convention to drift model convention 
C    (S-N rather than N-S)
C  Robert Grumbine 14 March 2007

      IMPLICIT none

      INTEGER uunit, vunit, nlat, nlon
      REAL ua(nlon, nlat), va(nlon, nlat)
      REAL utmp(nlon, nlat), vtmp(nlon, nlat)
      REAL maxwin
      INTEGER i, j

      READ (uunit) utmp
      READ (vunit) vtmp

      DO j = 1, nlat
      DO i = 1, nlon
        ua(i,nlat+1-j) = utmp(i,j)
        va(i,nlat+1-j) = vtmp(i,j) 
      ENDDO
      ENDDO

CD      PRINT *,'maxwin u = ',maxwin(ua, nlon, nlat)
CD      PRINT *,'maxwin v = ',maxwin(va, nlon, nlat)
      RETURN
      END

      REAL FUNCTION maxwin(ua, nlon, nlat)
      IMPLICIT none
      REAL tmp
      INTEGER i, j
      INTEGER nlon, nlat
      REAL ua(nlon, nlat)
      tmp = 0
      DO j = 1, nlat
      DO i = 1, nlon
        tmp = MAX(tmp, ua(i,j))
      ENDDO
      ENDDO
      maxwin = tmp
      RETURN
      END 
