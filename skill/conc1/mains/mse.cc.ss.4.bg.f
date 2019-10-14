      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(35000),Y(35000),Z(35000),clim(35000)
      REAL A(35000),B(35000),R1,R2,C,D,sumXY,sumA,sumX
      REAL climer(35000), climmse
      REAL SS1, SS2,ref,averageX,aver,ref1
      INTEGER n,i


      CALL GETXYZ1 (X,Y,Z,clim, n)
C      X = observations
C      Y = persistence forecast
C      Z = model forecast
C      clim = climatology

CD      PRINT *,  'sumX, sumY, sumZ =  ', sumX (X,n), sumX(Y,n),sumX(Z,n)
CD      
CD      PRINT *,  'avgX, avgY, avgZ =  ',averageX (X,n), averageX(Y,n),
CD     1         averageX(Z,n) 

      aver = averageX (X,n)
CD      PRINT *,  'aver =  ',aver

      DO 4000 i = 1,n
CBG         A(i) = A(i) + (Y(i) - X(i))  
         A(i) =  (Y(i) - X(i))  
 4000 CONTINUE
   
      DO 5000 i = 1,n
CBG         B(i) = B(i) + (Z(i) - X(i))
         B(i) =  (Z(i) - X(i))
 5000 CONTINUE

      DO 5100 i = 1, n
        climer(i) = clim(i) - x(i)
 5100 CONTINUE

CD       PRINT *, 'A(i) =  ', A(i)
CD       PRINT *, 'B(i) =  ', B(i) 
 
C      C = MSE (Y,X)
C      D = MSE (Z,X)

      C = (sumA (A,n))/n
      D = (sumA (B,n))/n
      climmse = (sumA (climer, n) ) / n

      PRINT *,  'MSE (Y,X) , (Z,X) =  ', C, D, climmse

CD      ref = 0.
CD
CD      DO 6000 i = 1,n
CD         ref = ref + ((X(i) - (aver))**2)
CD 6000 CONTINUE 
CD
CD      ref1 = ref/n       

C     SS1 = skill score btw Y,X
C     SS2 = skill score btw Z,X


      SS1 = 1 - (C/(climmse))
      SS2 = 1 - (D/(climmse))

CD      PRINT *,  'refX1 =   ', ref1

      PRINT *,  'skill score for Y,X ; Z,X =  ',SS1, SS2

      R1 = ((n * sumXY (Y,X,n)) - (sumX (Y,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA (Y,n) - ((sumX (Y,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))

      R2 = ((n * sumXY (Z,X,n)) - (sumX (Z,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA  (Z,n) - ((sumX (Z,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))


      PRINT *,  'correl btw Y,X ; Z, X=  ',R1, R2


      WRITE (15,9001) C, D

CD      WRITE (15,9002) D
CD 9002 FORMAT ('mse (z,x) =',F8.6)     


      WRITE (15,9003) SS1, SS2

CD      WRITE (15,9004) SS2
CD 9004 FORMAT ('SS2 (z,x) =',F7.4)    


      WRITE (15,9005) R1, R2
 9001 FORMAT ('mse (y,x) (z,x) =',2F8.5)     
 9003 FORMAT ('SS1 (y,x) (z,x) =',2F8.4)    
 9005 FORMAT ('CORREL 1  2     =',2F8.4) 

CD      WRITE (15,9006) R2
CD 9006 FORMAT ('CORREL 2  =',F7.4) 


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



 
