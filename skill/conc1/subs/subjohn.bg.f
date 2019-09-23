      SUBROUTINE getxyz(x, y, z, i)
C     Subroutine to return vectors with
C       ice concentrations from three consecutive days
C       to John's regression program.
C     Robert Grumbine 27 November 1996

      INTEGER nx, ny
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)

      REAL sumx, x(nx*ny), y(nx*ny), z(nx*ny)
      REAL xt, yt, zt
      INTEGER i, j

      OPEN (99, FILE="test.out", FORM="FORMATTED", STATUS="OLD")

      i = 0
 1000 CONTINUE
        READ (99, *, END=2000) xt, yt, zt
        IF (xt .LT. 128 .AND. yt .LT. 128 .AND. zt .LT. 128) THEN
          i = i + 1
CD          x(i) = xt
CD          y(i) = zt
          x(i) = (xt + zt)/2.
          y(i) = (xt - zt)/2.
          z(i) = yt
        ENDIF
        GO TO 1000

 2000 CONTINUE

C     Subtract the mean
      xt = sumx(x, i)/float(i)
      yt = sumx(y, i)/float(i)
      zt = sumx(z, i)/float(i)
      PRINT *,'means ',xt, yt, zt

      DO 3000 j = 1, i
        x(j) = x(j) - xt
        y(j) = y(j) - yt
        z(j) = z(j) - zt
 3000 CONTINUE

       
CD      PRINT *,"i = ",i

      RETURN
      END

      REAL FUNCTION sumx(x, n)
      INTEGER n
      REAL x(n)
      REAL tmp
      INTEGER i
      tmp = 0
      DO 1000 i = 1, n
        tmp = tmp + x(i)
 1000 CONTINUE
      sumx = tmp
      RETURN 
      END
