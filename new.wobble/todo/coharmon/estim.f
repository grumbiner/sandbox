      SUBROUTINE estim(x, n, dt, per, g, nsum, over, nrealize)
      IMPLICIT none
      INTEGER n, nsum, nrealize, over
      REAL dt, per, x(n)
      COMPLEX g(nsum/4*over, nrealize)
      INTEGER i, j, r, k
      DOUBLE PRECISION sumcos, sumsin
      REAL t, psi, pi
      
      pi = 3.141592654
      DO k = 1, nrealize
        DO r = 1, nsum/4*over
          sumsin = 0
          sumcos = 0
          DO j = 1, nsum
            t = -per + j*dt + (k-1)*nsum*dt !k bit is to keep absolute phase
            psi = 1. + cos(pi*t/per) 
            sumcos = sumcos + psi*x(j+nsum*(k-1))*
     1                              cos(2.*pi*r/over*t/per)
            sumsin = sumsin + psi*x(j+nsum*(k-1))*
     1                              sin(2.*pi*r/over*t/per)
          ENDDO
          g(r,k) = CMPLX(sumcos, sumsin)/2./nsum
CD          PRINT *,r/over,k,REAL(g(r,k)), IMAG(g(r,k)), 
CD     1                      sqrt(REAL(g(r,k)*conjg(g(r,k)))),per/(r/over)
        ENDDO
      ENDDO

      RETURN
      END