C***********************************************************----------!!
      SUBROUTINE ftrc(f, c, re_c, im_c, buffer, n, iwk, wk)
C     Compute the fast fourier transform of the input vector using the
C       IMSL routine FFTRC (which also calls FFTCC and FFT2C).
C     Robert Grumbine 25 Sep 1996

      IMPLICIT none

      INTEGER n
      COMPLEX c(n)
      REAL re_c(n), im_c(n)
      REAL f(n), wk(3*n+150)
      INTEGER iwk(3*n+150), i
      REAL buffer

      CALL FFTRC(f, n, c, iwk, wk)
      DO 1000 i = 1, n/2 + 1
        c(i) = conjg(c(i))/FLOAT(n)
 1000 CONTINUE

      DO 1100 i = 2, n/2
        c(n+2-i) = CONJG(c(i))
 1100 CONTINUE

      DO 1200 i = 1, n
        im_c(i) = AIMAG(c(i))
        re_c(i) = REAL(c(i))
 1200 CONTINUE

      RETURN
      END
