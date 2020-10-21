      PROGRAM delta

      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)
      REAL t1(nx, ny), t2(nx, ny)
      REAL s1(nx, ny), s2(nx, ny)

      CHARACTER*80 fname

 9001 FORMAT (A80)

      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')

      READ (10) t1
      READ (10) s1
      READ (11) t2
      READ (11) s2

      s1 = s1 - s2
      t1 = t1 - t2

      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW') 
      WRITE (12) t1
      WRITE (12) s1

      STOP
      END
