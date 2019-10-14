      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(20000),Y(20000),Z(20000)
      REAL R1,R2,sumXY,sumA,sumX
      REAL SD1Z,SD2Y,averageZ,averageY
      INTEGER n


      CALL GETXYZ1 (X,Y,Z,n)

      
      PRINT *,  'averageZ =    ',averageZ (Z,n) 
      PRINT *,  'averageY =    ',averageY (Y,n) 

      PRINT *,  'SD1Z =   ',SD1Z (Z,n)  
      PRINT *,  'SD2Y =   ',SD2Y (Y,n)  

      R1 = ((n * sumXY (Y,X,n)) - (sumX (Y,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA (Y,n) - ((sumX (Y,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))

      R2 = ((n * sumXY (Z,X,n)) - (sumX (Z,n) * sumX (X,n)))/
     1 ((SQRT(n*sumA  (Z,n) - ((sumX (Z,n))**2)))*
     2 (SQRT(n*sumA (X,n) - ((sumX (X,n))**2))))


      PRINT *,  'correl btw Y,X =  ',R1
      PRINT *,  'correl btw Z,X =  ',R2

      WRITE (15,9001) R1
 9001 FORMAT ('R1 =  ',F10.7)

      WRITE (15,9002) R2
 9002 FORMAT ('R2 =  ',F10.6)

      WRITE (15,9003) meany
 9003 FORMAT ('meany =  ',F10.6)

      WRITE (15,9004) meanz
 9004 FORMAT ('meanz =  ',F10.6)
     
      WRITE (15,9005) SD1Z
 9005 FORMAT ('SD1Z =  ',F10.6)

      WRITE (15,9006) SD2Y
 9006 FORMAT ('SD2Y =  ',F10.6)





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


      REAL FUNCTION averageY (Y,n)
      INTEGER i,n  
      REAL Y(n), average1

      average1 = 0.

      DO 4000 i = 1,n
         average1 = average1 + Y(i) 
 4000 CONTINUE

      averageY = average1/n

      RETURN 
      END


      REAL FUNCTION sd1Z (Z,n)
      INTEGER i,n
      REAL Z(i),sd1,averageZ

      sd1 = 0.
      DO 6000 i = 1,n
         sd1 = sd1 + SQRT(((Z(i) - averageZ (Z,n))**2)/(n-1))
 6000 CONTINUE 

      SD1Z = sd1 
      RETURN
      END


      REAL FUNCTION SD2Y (Y,n)
      INTEGER i,n
      REAL Y(i),sd2,averageY

      sd2 = 0.
      DO 6000 i = 1,n
         sd2 = sd2 + SQRT(((Y(i) - averageY (Y,n))**2)/(n-1))
 6000 CONTINUE 

      SD2Y = sd2 
      RETURN
      END
