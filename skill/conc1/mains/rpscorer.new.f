      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(20000),Y(20000),Z(20000)
      REAL R1,R2,sumXY,sumA,sumX
      REAL SDY,SDZ,averageY,averageZ
      INTEGER n


      CALL GETXYZ1 (X,Y,Z,n)

C      PRINT *,  'sumX (X,n) =  ', sumX (X,n) 
C      PRINT *,  'sumX (Y,n) =  ', sumX (Y,n) 
      
      PRINT *,  'mean pers=    ',averageY (Y,n) 
      PRINT *,  'mean model=    ',averageZ (Z,n) 


      PRINT *,  'SD pers =   ', SDY (Y,n)  
      PRINT *,  'SD model =   ', SDZ (Z,n)  

      R1 = ((n * sumXY (Y,X,n)) - (sumX (Y,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA (Y,n) - ((sumX (Y,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))

      R2 = ((n * sumXY (Z,X,n)) - (sumX (Z,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA  (Z,n) - ((sumX (Z,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))


      PRINT *,  'correl btw pers =  ',R1
      PRINT *,  'correl btw model =  ',R2

      WRITE (15,9001) R1, R2 
      WRITE (15,9003) SDY (Y,n), SDZ (Z,n)  
      WRITE (15,9005) averageY (Y,n), averageZ (Z,n)

 9001 FORMAT ('corr 1, 2 =  ',2F8.5)
 9003 FORMAT ('sd   1, 2 =  ',2F8.5)
 9005 FORMAT ('mean 1, 2 =  ',2F8.5)





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


      REAL FUNCTION averageY (Y,n)
      INTEGER i,n  
      REAL Y(n), average

      average = 0.

      DO 4000 i = 1,n
         average = average + Y(i) 
 4000 CONTINUE

      averageY = average/n

      RETURN 
      END
      

      REAL FUNCTION averageZ (Z,n)
      INTEGER i,n  
      REAL Z(n), average

      average = 0.

      DO 4000 i = 1,n
         average = average + Z(i) 
 4000 CONTINUE

      averageZ = average/n

      RETURN 
      END



      REAL FUNCTION SDY (Y,n)
      INTEGER i,n
      REAL Y(i),SD,averageY

      SD = 0.
      DO 6000 i = 1,n
         SD = SD + SQRT(((Y(i) - averageY (Y,n))**2))
 6000 CONTINUE 

      SDY = SD 
      RETURN
      END


      REAL FUNCTION SDZ (Z,n)
      INTEGER i,n
      REAL Z(i),SD,averageZ

      SD = 0.
      DO 6000 i = 1,n
         SD = SD + SQRT(((Z(i) - averageZ (Z,n))**2))
 6000 CONTINUE 

      SDZ = SD 
      RETURN
      END
