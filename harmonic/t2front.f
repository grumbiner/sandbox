      PROGRAM tfront
C     Program to compute tidal frequency for an input frequency.
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      INTEGER k(6)
      LOGICAL match
      REAL toler, per, tide

      INTEGER i, n, type
      REAL ampl

      PRINT *,'Do you want to estimate the Doodson # (1), or'
      PRINT *,'   compute the frequency, given the Doodson # (2)'
      READ (*,9005) type
      write (*,9005) type

      PRINT *,'How many components of the frequency vector do you want?'
      READ (*,9005) n
      write (*,9005) n

      IF (type .NE. 2) THEN
      PRINT *,'What tolerance do you want to use in comparison?'
      READ (*, 9001) toler

 1000 CONTINUE
        READ (*,9001, END=2000) per
        CALL tidefr(per, k, n, match, tide, toler)
        WRITE (*,9002) per, (k(i), i=1,n), match, tide
        GO TO 1000

       ELSE
 1100   CONTINUE 
D         PRINT *,'in computation loop'
          READ (*, 9003, END=2000) (k(i), i=1,n), ampl
          CALL tidmak(per, k, n, match, tide, toler)
          WRITE (*,9004) (k(i), i=1,n), ampl, per, 1./per 
        GO TO 1100
      ENDIF

 2000 CONTINUE

 9001 FORMAT (E14.7)

 9002 FORMAT (E13.6,6I4,L4,E13.6)

 9003 FORMAT (I1, 5I3, F10.5)

 9004 FORMAT (3I3, 1X, 3I3, F10.5, 2X, E12.5, F12.4) 
 
 9005 FORMAT (I4)
      END
