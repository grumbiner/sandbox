      PROGRAM magnify
C     Special purpose program to magnify an image by repeating 
C     data points.
C     Bob Grumbine 16 June 1994.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      INTEGER nx, ny
      PARAMETER (nx = LP)
      PARAMETER (ny = MP)
CD      CHARACTER*1 cin(nx,ny)
      REAL cin(nx,ny)

C     This value of mag is chosen to keep the larger map on the
C       screen of a 640/480 display.  As the maps are nearly 
C       square, the 480 is a bigger constraint.
      INTEGER mag
CD      PARAMETER (mag = 480/ny)
      PARAMETER (mag = divisor)
      CHARACTER*1 cout(mag*nx, mag*ny)

      INTEGER i, j, k, kj

      PRINT *,'mag = ',mag, mag*nx, mag*ny
      

      READ (10) cin

      DO 1000 j = 1, ny
        DO 2000 kj = 1, mag
          DO 1100 i = 1, nx
CD          READ (10) cin(i,j)
            DO 2100 k = 1, mag
CD              cout((i-1)*mag + k , (j-1)*mag + kj) = cin(i,j)
              cout((i-1)*mag + k , (j-1)*mag + kj) = 
     1                    CHAR (INT ( cin(i,j)*100. + 0.5 ) )
 2100 CONTINUE
 1100 CONTINUE
 2000 CONTINUE
 1000 CONTINUE
      
CD      WRITE (11) ((cout(i,j),i=1,mag*nx),j=1,mag*ny)
      WRITE (11) cout

      STOP
      END
