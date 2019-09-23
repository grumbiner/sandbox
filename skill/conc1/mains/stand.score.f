      PROGRAM  test
      IMPLICIT NONE
 
      REAL X(200000),Y(200000),Z(200000)
      REAL sumX,SDx,SDy,SDz,R1,R2,averageX,standX 
      INTEGER n,i

      CALL GETXYZ1 (X,Y,Z,n)

      PRINT *,  'sumX (X,n) =  ', sumX (X,n) 
      PRINT *,  'sumX (Y,n) =  ', sumX (Y,n) 
      PRINT *,  'sumX (Z,n) =  ', sumX (Z,n) 

      PRINT *,  'average X =  ', averageX (X,n) 
      PRINT *,  'average Y =  ', averageX (Y,n)  
      PRINT *,  'average Z =  ', averageX (Z,n)  

C      SDx = standard deviation of x
C      SDy = standard deviation of y
C      SDz = standard deviation of z

   
      SDx = SQRT(standX (X,n))
      SDy = SQRT(standX (Y,n))
      SDz = SQRT(standX (Z,n))


      PRINT *,  ' standard deviation x =  ', SDx 
      PRINT *,  ' standard deviation y =  ', SDy 
      PRINT *,  ' standard deviation z =  ', SDz 
 
      DO 6000 i = 1,n
         R1 = R1 + ((SDx*SDy)/n)
 6000 CONTINUE

      DO 7000 i = 1,n
         R2 = R2 + ((SDx*SDz)/n)
 7000 CONTINUE


      PRINT *,  'R1 =  ', R1 
      PRINT *,  'R2 =  ', R2 


      END

      REAL FUNCTION sumX (X,n)
      INTEGER i,n
      REAL X(n), sum

      sum = 0.


      DO 2000 i = 1,n
         sum = sum + X(i)
 2000 CONTINUE

      sumX = sum
      RETURN
      END


      REAL FUNCTION averageX (X,n)
      INTEGER i,n
      REAL X(n), average 

      average = 0.


      DO 8000 i = 1,n
         average = average + X(i)
 8000 CONTINUE

      averageX = average/n
      RETURN
      END


      REAL FUNCTION standX (X,n)
      INTEGER i,n
      REAL X(n), stand

      stand = 0

      DO 3001 i = 1,n
         stand = stand + ((X(i) - averageX (X,n))**2)
 3001 CONTINUE

      standX = stand/(n-1)
      RETURN
      END 
