      SUBROUTINE aset(a, nstep, iunit)
C     Initialize the ice concentration for the inversion theory
C       test program.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nstep
      REAL a(nstep)
      INTEGER i, iunit

      DO 1000 i = 1, nstep
        a(i) = 0.0
 1000 CONTINUE

      RETURN
      END
