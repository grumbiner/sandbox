      PROGRAM ttest
C     Program to run a t test on a set of forecast experiments
      IMPLICIT none
      INTEGER nx, ny, nday
      PARAMETER (nx = 360)
      PARAMETER (ny = 181)
      PARAMETER (nday = 9)

      REAL x(nx, ny, nday), y(nx, ny, nday), z(nx, ny, nday)
      REAL sumx, sumy, sumz
      REAL sumsqx, sumsqy, sumsqz
      REAL sigmay, sigmaz
      REAL ty(nx, ny), tz(nx, ny)
      INTEGER count

      INTEGER i, j, k

      CHARACTER*60 fname

      DO 1000 k = 1, nday

        WRITE (fname, 9001) k
        OPEN (10, FILE=fname, FORM="UNFORMATTED")
        READ (10) ty
        CALL areq(x(1,1,k), ty, nx, ny)
   
        WRITE (fname, 9002) k
        OPEN (11, FILE=fname, FORM="UNFORMATTED")
        READ (11) ty
        CALL areq(y(1,1,k), ty, nx, ny)
 
        WRITE (fname, 9003) k
        OPEN (12, FILE=fname, FORM="UNFORMATTED")
        READ (12) ty
        CALL areq(z(1,1,k), ty, nx, ny)
 
 1000 CONTINUE
 9001 FORMAT ("ll1.1",I1,"168")
 9002 FORMAT ("ll2.1",I1,"168")
 9003 FORMAT ("ll3.1",I1,"168")

      PRINT *,'beginning computation'
      DO 2000 j = 2, ny-1
        DO 2100 i = 1, nx
          count = 0
          sumx = 0.
          sumy = 0.
          sumz = 0.
          sumsqx = 0.
          sumsqy = 0.
          sumsqz = 0.
          DO 2200 k = 1, nday
            IF (ABS(x(i,j,k)) .LT. 1.E10 .AND. 
     1          ABS(y(i,j,k)) .LT. 1.E10 .AND.
     2          ABS(z(i,j,k)) .LT. 1.E10 ) THEN
              count = count + 1 
              sumx = sumx + x(i,j,k)
              sumy = sumy + y(i,j,k)
              sumz = sumz + z(i,j,k)
              sumsqx = sumsqx + x(i,j,k)*x(i,j,k)
              sumsqy = sumsqy + y(i,j,k)*y(i,j,k)
              sumsqz = sumsqz + z(i,j,k)*z(i,j,k)
            ENDIF
 2200     CONTINUE
          IF (count .GT. 1) THEN
          sigmay = SQRT( (sumsqy - sumy**2 / FLOAT(count) )/
     1                  FLOAT(count-1) )
          sigmaz = SQRT( (sumsqz - sumz**2 / FLOAT(count) )/
     1                  FLOAT(count-1) )
           ELSE
           sigmay = 0.0
           sigmaz = 0.0
          ENDIF
          IF (sigmay .NE. 0 .AND. sigmaz .NE. 0) THEN
            ty(i,j) = (sumy/SQRT(nday) / sigmay)
            tz(i,j) = (sumz/SQRT(nday) / sigmaz)
              
              WRITE (*,9010) 91-j, i, sumy/nday, ty(i,j)
CD            IF (ABS(ty(i,j)) .GT. 3.3) THEN
CD              WRITE (*,9010) 91-j, i, ty(i,j)
CD            ENDIF
CD            IF (ABS(tz(i,j)) .GT. 3.3) THEN
CD              WRITE (*,9011) 91-j, i, tz(i,j)
CD            ENDIF
          ENDIF
       
 2100   CONTINUE
 2000 CONTINUE
      WRITE (20) ty
      WRITE (21) tz

 9010 FORMAT ("y ",I4, I4,2x, F11.6,2x,F8.3)
 9011 FORMAT ("z ",I4, I4,2x, 2F6.3)

      END         
      SUBROUTINE areq(x, y, nx, ny)
      INTEGER nx, ny, i, j
      REAL x(nx, ny), y(nx, ny)
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        x(i,j) = y(i,j)
 1000 CONTINUE
      RETURN
      END
