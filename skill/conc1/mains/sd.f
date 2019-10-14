C This program will take the standard deviation of a list of 
C numbers.  It will give the standard deviation of the 2nd
C and the 4th columns of numbers and put into a separate
C file. 

C Subprograms needed: subjohn.f

      PROGRAM AB test
      IMPLICIT NONE
 
      REAL Y(16000),Z(16000)
      REAL averageX,SD,averz,avery
      INTEGER n


      CALL GETYZ1 (Y,Z, n)


      WRITE (15,9001) SD (Y,n), SD (Z,n) 
    
 9001 FORMAT ('  ',2F7.4)     

C      PRINT *, "average Z =  ",averageX (Z,n)
C      PRINT *, "average Y =  ",averageX (Y,n)
 

      averz = averageX (Z,n)
      avery = averageX (Y,n)

      END
C ---------------------------------------------------------------------------
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
C ---------------------------------------------------------------------------

      REAL FUNCTION SD (Z,n)
      INTEGER i,n
      REAL var,avg,Z(n)
   
      avg = averageX (Z,n) 

      var = 0
      DO 6000 i = 1,n
      var = var + ((Z(i) - avg)**2) 
 6000 CONTINUE

      SD = SQRT(var/n)
      RETURN
      END
 
