      SUBROUTINE tridig(a, b, c, r, u, n)
C     Subroutine to solve a tri-diagonal matrix problem
C       Au = r, where the diagonals of A are a, b, c
C     From Numerical Recipes
C     Robert Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER n
      REAL a(n), b(n), c(n), r(n), u(n)

      INTEGER nmax 
      PARAMETER (nmax = 1364)
      REAL bet, gam(nmax)
      INTEGER j

      IF (nmax .LT. n) THEN
        PRINT *,'Need more work space'
        STOP
      ENDIF
      IF (b(1) .EQ. 0) THEN
        PRINT *,'b1 = 0, cannot invert'
        STOP
      ENDIF

      bet = b(1)
      u(1) = r(1)/bet
      DO 1000 j = 2, N
        gam(j) = c(j-1)/bet
        bet    = b(j)-a(j)*gam(j)
        IF (bet .EQ. 0.) THEN
          PRINT *,'Beta = 0 in inversion, cannot invert'
          STOP
        ENDIF
        u(j) = (r(j)-a(j)*u(j-1))/bet
 1000 CONTINUE

      DO 1100 j = n-1, 1, -1
        u(j) = u(j)-gam(j+1)*u(j+1)
 1100 CONTINUE

      RETURN
      END
