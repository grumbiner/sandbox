      PROGRAM SPECANY
  
C     SPECTRAL ANALYSIS USING THE IMSL ROUTINES 
C       FROM GEBPLTZ.  ORIGINALLY WRITTEN BY ALBERT LUNDE, MODIFIED 
C       BY BOB GRUMBINE 8-85
  
      REAL DELTA, X(20000)
  
      INTEGER LEN, LAG, IU, IER, I

      PRINT *,'How many points do you have?'  
      READ (*,9001) LEN 
      PRINT *,'What lag do you want?'
      READ (*,9001) LAG 
      PRINT *,'What was the time step?'
      READ(*,9002) DELTA

      CALL readin(x, len)
 
      IU  = 7 
      IER = 0 
  
      CALL SPEC(X, LEN, LAG, DELTA, IU, IER)
  
 9001 FORMAT (I5) 
  
 9002 FORMAT (E15.7) 
  
 9003 FORMAT (6I6)
  
      END 