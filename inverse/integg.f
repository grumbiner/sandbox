      SUBROUTINE intega(g, nthick, nstep, dh, tstep, ia, h0, hbar)
C     Subroutine to integrate the thickness distribution to get the
C       related ice concentration.
      REAL ia, dh, hbar
      INTEGER i, nthick, nstep, tstep
      REAL g(nthick, nstep)

      DOUBLE PRECISION sum, sum2

      sum  = 0.0
      sum2 = 0.0

      DO 1000 i = 1, nthick
        sum  = sum  + DBLE(dh*                g(i,tstep) )
        sum2 = sum2 + DBLE(dh* (h0+(i-1)*dh) *g(i,tstep) )
 1000 CONTINUE

      ia   = SNGL(sum)
      hbar = SNGL(sum2)

      RETURN
      END
