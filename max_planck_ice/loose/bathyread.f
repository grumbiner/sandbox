      PROGRAM bathy
C     Auxiliary to H. Tolman program.  Read in his integer formatted 
C       output and rewrite to reals on grid of the zmask data set.

      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 360)
      PARAMETER (ny = 180)

      INTEGER ht(nx*ny)
      REAL depth(nx, ny)

      INTEGER i, j, k

      OPEN (10, FILE='bathy.ww3', FORM='FORMATTED', STATUS='OLD')

      DO 1000 k = 0, nx*ny-1, 12
        READ (10, 9001) (ht(k+i),i=1,12)
 1000 CONTINUE
 9001 FORMAT (12I6)

      k = 0
C    Need to flip from original for correspondence with downstream convention
      DO 2000 j = ny, 1, -1
        DO 2100 i = 1, nx
          k = k + 1
          depth(i,j) = ht(k)/10.
 2100   CONTINUE
 2000 CONTINUE
 
      OPEN (11, FILE='bathy.bin', FORM='UNFORMATTED', STATUS='NEW')
      WRITE (11) depth

      STOP
      END
