      SUBROUTINE frecmp (q, n, mult, freq, m)
C     Subroutine to find the frequency of occurrence for an input
C       vector.
C     Robert Grumbine 2 May 1995

      IMPLICIT none

      INTEGER n, m, last, first
      REAL q(n), freq(-m:m), mult
      
      CHARACTER*60 fname
      INTEGER i

      DO 100 i = -m, m
        freq(i) = 0.0
  100 CONTINUE

      first = INT( q(1) )
      last  = INT( q(1) )

      DO 1000 i = 1, n

        freq( INT(q(i)*mult) ) = freq( INT(q(i)*mult) )+1.
        first = MIN0(INT(mult*q(i)), first)
        last  = MAX0(INT(mult*q(i)), last)
 
 1000 CONTINUE

      PRINT *,'What do you want to call the data file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      DO 2000 i = first, last
        WRITE(11,9003) FLOAT(i), CHAR(9), freq(i)
 2000 CONTINUE
      CLOSE(11, STATUS='KEEP')

      PRINT *,'There were ', last-first+1 ,' points'

 9001 FORMAT (A60)

 9002 FORMAT (2E13.5)
 
 9003 FORMAT (E12.6,A1,E12.6)

      RETURN
      END      
