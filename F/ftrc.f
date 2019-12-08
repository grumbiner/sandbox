C***********************************************************----------!!
      SUBROUTINE ftrc(f, c, buffer, n, iwk, wk)

      INTEGER n
      COMPLEX c(n)
      REAL f(n), wk(3*n+150)
      INTEGER iwk(3*n+150), i
      REAL buffer

C     Compute the fast fourier transform of the input vector using the
C       IMSL routine FFTRC (which also calls FFTCC and FFT2C).

      CALL FFTRC(f, n, c, iwk, wk)
      DO 1000 i = 1, n/2 + 1
        c(i) = conjg(c(i))/FLOAT(n)
 1000 CONTINUE

      DO 1100 i = 2, n/2
        c(n+2-i) = CONJG(c(i))
 1100 CONTINUE

      RETURN
      END
