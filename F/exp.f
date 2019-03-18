      PROGRAM test
      INTEGER x

      DO 1000 x = 1, 10000, 10
        PRINT *,'x, exp(x) ',x, exp(FLOAT(x) )
 1000 CONTINUE

      END 
     
