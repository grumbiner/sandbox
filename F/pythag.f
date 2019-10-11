      SUBROUTINE pythag(u, v, r, n)
C     Subroutine to compute (u**2+v**2)**.5.
C     Robert Grumbine 27 Sep 1995

C     Declare arguments:
      INTEGER n
      REAL u(n), v(n), r(n)

C     Declare local variable:
      INTEGER i


      DO 1000 i = 1, n
        r(i) = SQRT( u(i)*u(i) + v(i)*v(i) )
 1000 CONTINUE


      RETURN
      END
