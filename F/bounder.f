      SUBROUTINE bounder(x, npts, lo, hi)
C     Noisily force variables in a vector to lie within bounds.
C     The noise is that a message is printed for each change.
C     Robert Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER npts
      REAL x(npts), lo, hi
      INTEGER i

      DO 1000 i = 1, npts
        IF (x(i) .LT. lo) THEN
          PRINT *,'too low ',i,x(i),' reset to ',lo
          x(i) = lo
         ELSE IF (x(i) .GT. hi) THEN
          PRINT *,'too high ',i,x(i),' reset to ',hi
          x(i) = hi
        ENDIF
 1000 CONTINUE
 
      RETURN
      END 
