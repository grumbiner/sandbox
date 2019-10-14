      SUBROUTINE POSN(i, iunit, j)
C     Emulate the IBM system routine POSN, which opens
C       and positions files on tapes.
C     For compatibility with Parkinson '79 and '83 model.
C       By Bob Grumbine 2/13/92.

      INTEGER i, j, iunit

      OPEN (UNIT=iunit, FILE='claire.dat',
     1  FORM='UNFORMATTED', STATUS='OLD')

      RETURN
      END
