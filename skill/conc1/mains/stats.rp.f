      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(20000),Y(20000)
      REAL R1,R2,sumXY,sumA,sumX
      REAL SDX,averageX
      INTEGER n,i


      CALL GETXYZ1 (X,Y,Z,n)

C      PRINT *,  'sumX (X,n) =  ', sumX (X,n) 
C      PRINT *,  'sumX (Y,n) =  ', sumX (Y,n) 
      
      PRINT *,  'mean pers=    ',averageX (X,n) 
      PRINT *,  'mean model=    ',averageX (Y,n) 


      PRINT *,  'SD pers =   ', SDX (X,n)  
      PRINT *,  'SD model =   ', SDX (Y,n)  

      R1 = ((n * sumXY (Y,X,n)) - (sumX (Y,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA (Y,n) - ((sumX (Y,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))

      R2 = ((n * sumXY (Z,X,n)) - (sumX (Z,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA  (Z,n) - ((sumX (Z,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))


      PRINT *,  'correl btw pers =  ',R1
      PRINT *,  'correl btw model =  ',R2



      END

      REAL FUNCTION sumA (A,n)
      INTEGER i,n
      REAL A(n),sum

      sum = 0

      DO 1000 i = 1,n
         sum = sum + A(i)*A(i)
 1000 CONTINUE

      sumA = sum
      RETURN 
      END

      REAL FUNCTION sumXY (X,Y,n)
      INTEGER i,n
      REAL X(n), Y(n), sum

      sum = 0

      DO 2000 i = 1,n
         sum = sum + X(i)*Y(i)
 2000 CONTINUE

      sumXY = sum
      RETURN
      END


      REAL FUNCTION sumX (X,n)
      INTEGER i,n
      REAL X(n), sum

      sum = 0

      DO 3000 i = 1,n
         sum = sum + X(i)
 3000 CONTINUE

      sumX = sum
      RETURN
      END

      REAL FUNCTION averageX (X,n)
      INTEGER i,n  
      REAL X(n), average

      average = 0.

      DO 4000 i = 1,n
         average = average + X(i) 
 4000 CONTINUE

      averageX = average/n

      RETURN 
      END

      REAL FUNCTION SDX (X,n)
      INTEGER i,n
      REAL X(i),SD,averageX

      SD = 0.
      DO 6000 i = 1,n
         SD = SD + ((X(i) - averageX (X,n))**2)
 6000 CONTINUE 

      SDX = SD 
      RETURN
      END
