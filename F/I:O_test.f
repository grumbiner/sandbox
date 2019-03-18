C  ---------------PROGRAM TEST----------

      IMPLICIT none
      DOUBLE PRECISION phos(76)
      INTEGER l, m, n, step
      CHARACTER*60 merg, add
      
      
C   ----INPUT FROM USER-----

      PRINT *, 'WHAT IS THE NAME OF THE OUPUT FILE'
      READ (*,9001) add
      PRINT *, 'WHAT FILE DO YOU WANT TO READ FROM'
      READ (*,9001) merg
      PRINT *, 'WHAT STEP DO YOU WANT TO START AT'
      READ (*,9002) step
      
      OPEN (UNIT = 13, FILE = merg, FORM='FORMATTED', STATUS='OLD')

C   ----READING IN INFORMATION------
      
      DO 200 l=1,step
        DO 250 m=1,76
	      READ (13,9010) phos(m)
  250   CONTINUE
  200 CONTINUE
  
C   ----OUTPUT INFORMATION------
      OPEN (UNIT = 14, FILE = add, FORM='formatted', STATUS='NEW')
      DO 300 n=1,76
        WRITE (14,9003) phos(n)
  300 CONTINUE
      
 9001 FORMAT (A60)
 9002 FORMAT (I2)
 9003 FORMAT (1X, E13.6)
 9010 FORMAT (10X, E13.6)
  
      stop 
      end