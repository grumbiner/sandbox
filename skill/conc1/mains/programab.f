      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(200000),Y(200000),Z(200000)
      REAL a, b, XX, XY, YY, XZ, YZ, sumXY
      INTEGER i, n

      CALL GETXYZ (X,Y,Z,n)

      XY = sumXY (X,Y,n)
      XX = sumXY (X,X,n)
      XZ = sumXY (X,Z,n)
      YY = sumXY (Y,Y,n)
      YZ = sumXY (Y,Z,n)

      a = (YY*XZ - YZ*XY)/(XX*YY - XY*XY)
      b = (YZ*XX - XY*XZ)/(XX*YY - XY*XY)  

      PRINT *, 'a =  ',a
      PRINT *, 'b =  ',b

      DO 1000 i = 1, n
CD        Z(i) = Z(i) - a*X(i) - b*Y(i)
        WRITE (*,9001) i, X(i), Y(i), Z(i), Z(i) - a*X(i) - b*Y(i) 
 1000 CONTINUE 
 9001 FORMAT (I7, 4F7.2)

      END

      REAL FUNCTION sumXY (X,Y,n)
      INTEGER i,n
      REAL X(n),Y(n),sum

      sum = 0
 
      DO 1000 i = 1,n
         sum = sum + X(i)*Y(i)
 1000 CONTINUE

      sumXY = sum
      RETURN 
      END
