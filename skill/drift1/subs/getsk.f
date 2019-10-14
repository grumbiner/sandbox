C================================================================   
      SUBROUTINE getsk(dir, dist, nskile, ntot, iunit)
C     Version for reading in the skile1 forecast files.
C     Bob Grumbine 2 June 1994.

      IMPLICIT none

      INTEGER iunit, nskile, ntot
      REAL dir(nskile+1), dist(nskile+1)
       
      INTEGER i, j, k, l, n1, n2, n3, n4
      CHARACTER*60 header
      
      DO 1000 i = 1, 6
        READ (iunit,9001) header
 1000 CONTINUE

 9001 FORMAT (A60)

      DO 2000 i = 1, 52
         j = i + 52
         k = j + 52
         l = k + 52
         READ(iunit, 9002, END=2001, ERR=2001) n1, dir(i), dist(i),
     1     n2, dir(j), dist(j), n3, dir(k), dist(k), 
     1     n4, dir(l), dist(l)
 2000 CONTINUE
 
 2001 CONTINUE

 9002 FORMAT (2x, I3, 3x, F5.1, 4x, F5.1, 8x,
     1            I3, 3x, F5.1, 4x, F5.1, 8x,
     2            I3, 3x, F5.1, 4x, F5.1, 8x,
     3            I3, 3x, F5.1, 4x, F5.1     )

      RETURN
      END
