  
      SUBROUTINE getxyz(x, y, z, i)
C     Subroutine to return vectors with
C       ice concentrations from three consecutive days
C       to John's regression program.
C     Robert Grumbine 27 November 1996

      IMPLICIT NONE
      INTEGER nx, ny
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)

      REAL x(nx*ny), y(nx*ny), z(nx*ny)
      REAL xt, yt, zt 
      REAL XX,YY,ZZ,meanx1 
      INTEGER i,n

      OPEN (99, FILE="test.out", FORM="FORMATTED", STATUS="OLD")


      i = 0
 1000 CONTINUE
        READ (99, *, END=2000) xt, yt, zt
        IF (xt .LT. 128 .AND. yt .LT. 128 .AND. zt .LT. 128) THEN
          i = i + 1
          x(i) = xt    
          y(i) = yt  
          z(i) = zt   
        ENDIF
        GO TO 1000

 2000 CONTINUE

      PRINT *,"i = ",i

      XX = meanx1 (x,n)
      YY = meanx1 (y,n) 
      ZZ = meanx1 (z,n)  

 
C      PRINT *,'mean X =  ' XX
C      PRINT *,'mean Y =  ' YY
C      PRINT *,'mean Z =  ' ZZ

   
      DO 3000 i = 1,n

      x(i) = x(i) - XX 

 3000 CONTINUE

      DO 4000 i = 1,n

      y(i) = y(i) - YY

 4000 CONTINUE

      DO 5000 i = 1,n

      z(i) = z(i) - ZZ

 5000 CONTINUE


      PRINT *,'x(i) =  ', x(n)
      PRINT *,'y(i) =  ', y(n)
      PRINT *,'z(i) =  ', z(n)


      END

      REAL FUNCTION meanx1 (x,n)
      INTEGER i,n
      REAL x(n),sum

      sum = 0. 

      DO 1000 i= 1,n
         sum = sum + x(i)
 1000 CONTINUE

      XX = (sum)/n 
      RETURN
      END
