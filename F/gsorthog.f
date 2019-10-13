      SUBROUTINE orthog(u, vector, nlines, npts)
C     Orthogonalize the matrix columnwise with respect
C       to the input vector.
C     Robert Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER npts, nlines
      REAL u(nlines, npts), vector(nlines)
      REAL vdot

      INTEGER i, j
      REAL x, y
      
C     Now orthogonalize with respect to the reference:
      x = vdot(vector,vector,nlines)
      DO 3000 j = 1, npts
        y = vdot(vector,u(1,j),nlines)
        DO 3100 i= 1, nlines
          u(i,j) = u(i,j) - y*vector(i)/x
 3100   CONTINUE
 3000 CONTINUE

      RETURN
      END
