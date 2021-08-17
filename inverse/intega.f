      SUBROUTINE intega(g, nx, ny, nthick, nstep, dh, tstep, ia, 
     1    h0, hbar)
C     Subroutine to integrate the thickness distribution to get the
C       related ice concentration.
      IMPLICIT none

      INTEGER nx, ny
      REAL ia(nx, ny), dh, h0, hbar(nx, ny)
      INTEGER i, j, k, nthick, nstep, tstep
      REAL g(nthick, nx, ny, nstep)

      DOUBLE PRECISION sum, sum2

      DO 2000 j = 1, ny
      DO 2100 k = 1, nx
        sum  = 0.0
        sum2 = 0.0

        DO 1000 i = 1, nthick-1
CD          sum  = sum  + DBLE(dh*                g(i,k, j, tstep) )
CD          sum2 = sum2 + DBLE(dh* (h0+(i-1)*dh) *g(i,k, j, tstep) )
          sum = sum + DBLE(dh * (g(i,k,j,tstep)+g(i+1,k,j,tstep))/2. )
          sum2 = sum2 + DBLE(dh * (g(i,k,j,tstep)+g(i+1,k,j,tstep))/2.
     1             * (dh*(i-1)+h0 )  )
 1000   CONTINUE
        ia(k,j)   = SNGL(sum)
        hbar(k,j) = SNGL(sum2)
 2100 CONTINUE
 2000 CONTINUE


      RETURN
      END
