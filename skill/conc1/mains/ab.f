      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(200000),Y(200000),Z(200000),a,b,XX,XY,YY,XZ,YZ,sumXY
      INTEGER n

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
