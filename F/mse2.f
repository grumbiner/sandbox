      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(35000),Y(35000),Z(35000)
      REAL A(35000),B(35000),R1,R2,C,D,sumXY,sumA,sumX
      REAL SS1, SS2,ref,averageX,aver,ref1
      INTEGER n,i


      CALL GETXYZ1 (X,Y,Z,n)
C      X = observations
C      Y = persistence forecast
C      Z = model forecast

      PRINT *,  'sumX (X,n) =  ', sumX (X,n) 
      PRINT *,  'sumX (Y,n) =  ', sumX (Y,n) 
      PRINT *,  'sumX (Z,n) =  ', sumX (Z,n) 
      
      PRINT *,  'averageX =    ',averageX (X,n) 
      PRINT *,  'averageY =    ',averageX (Y,n) 
      PRINT *,  'averageZ =    ',averageX (Z,n) 

      aver = averageX (X,n)
      PRINT *,  'aver =  ',aver

      DO 4000 i = 1,n
         A(i) = A(i) + (Y(i) - X(i))  
 4000 CONTINUE
   
      DO 5000 i = 1,n
         B(i) = B(i) + (Z(i) - X(i))
 5000 CONTINUE

       PRINT *, 'A(i) =  ', A(i)
       PRINT *, 'B(i) =  ', B(i) 
 
C      C = MSE (Y,X)
C      D = MSE (Z,X)

      C = (sumA (A,n))/n
      D = (sumA (B,n))/n

      PRINT *,  'MSE (Y,X) =  ', C
      PRINT *,  'MSE (Z,X) =  ', D

      ref = 0.

      DO 6000 i = 1,n
         ref = ref + ((X(i) - (aver))**2)
 6000 CONTINUE 

      ref1 = ref/n       

C     SS1 = skill score btw Y,X
C     SS2 = skill score btw Z,X


      SS1 = 1 - (C/(ref1))
      SS2 = 1 - (D/(ref1))

      PRINT *,  'refX1 =   ', ref1

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
      


      WRITE (15,9001) C
 9001 FORMAT ('mse (y,x) =',F8.6)     

      WRITE (15,9002) D
 9002 FORMAT ('mse (z,x) =',F8.6)     


      WRITE (15,9003) SS1
 9003 FORMAT ('SS1 (y,x) =',F7.4)    

      WRITE (15,9004) SS2
 9004 FORMAT ('SS2 (z,x) =',F7.4)    


      WRITE (15,9005) R1
 9005 FORMAT ('CORREL 1  =',F7.4) 

      WRITE (15,9006) R2
 9006 FORMAT ('CORREL 2  =',F7.4) 



      END
