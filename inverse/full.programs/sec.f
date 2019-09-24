      PROGRAM sec
      INTEGER nstep
      PARAMETER (nstep = 64)

      REAL delta(nstep), second(nstep)
      INTEGER i

      DO 1000 i = 1, nstep
        READ (*,9002) delta(i)
 1000 CONTINUE

      DO 2000 i = 2, nstep-1
        second(i) = delta(i+1) + delta(i-1) - 2.*delta(i)
        WRITE (*,9003) i, second(i)*1.-delta(i) 
 2000 CONTINUE

     
 9002 FORMAT (8x,F8.4)
 9003 FORMAT (I4, E13.6)
      END
