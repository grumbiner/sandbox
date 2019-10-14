C***********************************************************__________!!
      SUBROUTINE armxmn (x, size, xmax, xmin)
C     Subroutine to find the max, min of a vector.
C     Robert Grumbine 2 May 1995
     
      IMPLICIT none

C     Declare arguments.
      INTEGER size
      REAL x(size), xmax, xmin

C     Local variable.
      INTEGER i

      xmax = -1.0E+36
      xmin = +1.0E+36

      DO 1000 i = 1, size
        xmax = AMAX1( xmax, x(i) )
        xmin = AMIN1( xmin, x(i) )
 1000 CONTINUE

      RETURN
      END
