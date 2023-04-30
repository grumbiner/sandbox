      PROGRAM averager

      IMPLICIT NONE

      INTEGER nx, ny, nt
      PARAMETER (nx = 192)
      PARAMETER (ny =  94)
      PARAMETER (nt = 68668)

      REAL x(nt), sum(nt)
      REAL mean

      INTEGER i, j
      CHARACTER*90 fname
      

! Average at constant latitude bands
      DO j = 1, ny
        sum = 0
        DO i = 1, nx
          WRITE (fname, 9002) i,j
          OPEN (10, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")
          READ (10) x
          CLOSE(10)
          sum = sum + x
        ENDDO
        sum = sum / nx
        WRITE (fname, 9003) j
        OPEN (11, FILE=fname, FORM="UNFORMATTED", STATUS="UNKNOWN")
        WRITE (11) sum
        WRITE (*,9009) j, mean(sum, nt)
        CLOSE (11) 
      ENDDO
 9009 FORMAT(I3.3,",",E13.6)
        
! Average at constant longitude bands
      DO i = 1, nx
        sum = 0
        DO j = 1, ny
          WRITE (fname, 9002) i,j
          OPEN (10, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")
          READ (10) x
          CLOSE(10)
          sum = sum + x
        ENDDO
        sum = sum / ny
        WRITE (fname, 9004) i
        OPEN (11, FILE=fname, FORM="UNFORMATTED", STATUS="UNKNOWN")
        WRITE (11) sum
        WRITE (*,9009) i, mean(sum, nt)
        CLOSE (11) 
      ENDDO

      
 9002 FORMAT("trans.",I3.3,".",I2.2)
 9003 FORMAT("avgi.",I2.2)
 9004 FORMAT("avgj.",I3.3)

      END
      REAL FUNCTION mean(x, nx)
      IMPLICIT none
      INTEGER nx
      REAL x(nx)
   
      DOUBLE PRECISION sum
      INTEGER i

      sum = 0
      DO i = 1, nx
        sum = sum + x(i)
      ENDDO

      mean = sum/nx

      RETURN
      END
