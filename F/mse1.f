      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(30000),Y(30000),Z(30000)
      REAL A(30000),B(30000),R1,R2,C,D,sumXY,sumA,sumX
      REAL SS1, SS2,refX,averageX
      INTEGER n,i


      CALL GETXYZ1 (X,Y,Z,n)
C      X = observations
C      Y,Z = forecasts

      PRINT *,  'sumX (X,n) =  ', sumX (X,n) 
      PRINT *,  'sumX (Y,n) =  ', sumX (Y,n) 
      PRINT *,  'sumX (Z,n) =  ', sumX (Z,n) 
      
      PRINT *,  'averageX =    ',averageX (X,n) 
      PRINT *,  'averageY =    ',averageX (Y,n) 
      PRINT *,  'averageZ =    ',averageX (Z,n) 

      DO 4000 i = 1,n
         A(i) = A(i) + (Y(i) - X(i))  
 4000 CONTINUE
      
      DO 5000 i = 1,n
         B(i) = B(i) + (Z(i) - X(i))
 5000 CONTINUE

 
C      C = MSE (Y,X)
C      D = MSE (Z,X)

      C = (sumA (A,n))/n
      D = (sumA (B,n))/n

      PRINT *,  'MSE (Y,X) =  ', C
      PRINT *,  'MSE (Z,X) =  ', D

C     SS1 = skill score btw Y,X
C     SS2 = skill score btw Z,X

      SS1 = 1 - (C/(refX (X,n)))
      SS2 = 1 - (D/(refX (X,n)))

      PRINT *,  'refX =   ', refX (X,n)  

      PRINT *,  'skill score for Y,X =  ',SS1
      PRINT *,  'skill score for Z,X =  ',SS2

      R1 = ((n * sumXY (Y,X,n)) - (sumX (Y,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA (Y,n) - ((sumX (Y,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))

      R2 = ((n * sumXY (Z,X,n)) - (sumX (Z,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA  (Z,n) - ((sumX (Z,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))


      PRINT *,  'correl btw Y,X =  ',R1
      PRINT *,  'correl btw Z,X =  ',R2



      END
