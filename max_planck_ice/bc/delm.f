      PROGRAM delm
C     Difference two land masks on the northern hemisphere 127 km grid
C     Robert Grumbine
C     Last Modified 9 November 1995

      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 76)
      PARAMETER (ny = 92)

      INTEGER b1(0:76, 0:92), b2(0:nx, 0:ny)
      INTEGER i, j, count

      OPEN (10, FILE='maskfull', FORM='FORMATTED', STATUS='OLD')
      OPEN (11, FILE='old/maskfull', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='delta', FORM='FORMATTED', STATUS='NEW')
      count = 0

      DO 1000 j = 0, ny
        READ (10, 9001) (b1(i,j),i=0, nx) 
        READ (11, 9001) (b2(i,j),i=0, nx) 
 1000 CONTINUE
 9001 FORMAT (93I1)

      DO 2000 j = 0, ny
        DO 2000 i = 0, nx
          IF (b1(i,j) .NE. b2(i,j) ) count = count + 1
 2000 CONTINUE
      PRINT *,count,' different point decisions'

      b2 = b2 - b1

      DO 1100 j = 0, ny
        WRITE (12, 9001) (b2(i,j),i=0, nx)
 1100 CONTINUE

      STOP
      END
