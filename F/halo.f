      PROGRAM halo
   
      IMPLICIT none
      REAL, DIMENSION (1000) :: data
      REAL x(1000)
      INTEGER i      
      REAL A(1)
      REAL sum
      REAL variance
      REAL SD

        sum = 0
         DO  i= 1, 1000
          x(i) = 2*i           
          sum = sum + x(i) 
          A(1) = sum/1000
         
         VARIANCE = ((sum*(x(i)-A(1))**2))/1000 
         SD = SQRT(VARIANCE)
         
         END DO
     
      PRINT *, A(1), VARIANCE, SD
      

      END PROGRAM
