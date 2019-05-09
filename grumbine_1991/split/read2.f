C***********************************************************----------!!
      SUBROUTINE read2(x, nx, ny, unit, fname, ope, clos)
C     Read in a 2d, unformatted array, with external control on
C       opening and closing.
C     Bob Grumbine 5 April 1994.

      IMPLICIT none

      INTEGER nx, ny, unit
      REAL x(nx, ny)
      CHARACTER*60 fname
      LOGICAL ope, clos

      IF (ope) OPEN(unit, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      READ (unit) x
      IF (clos) CLOSE(unit, STATUS='KEEP')

      RETURN
      END
