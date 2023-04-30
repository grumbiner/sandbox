
      SUBROUTINE ensemble(g, h, nsum, nreal, z, cohere, period)
      IMPLICIT none
      INTEGER nsum, nreal
      COMPLEX g(nsum, nreal), h(nsum, nreal)
      COMPLEX z(nsum)
      COMPLEX cohere(nsum)
      COMPLEX eir(nsum), eor(nsum), tmp(nsum)
      REAL period

   
      INTEGER i, j

      DO j = 1, nsum
        eir(j) = CMPLX(0,0)
        eor(j) = CMPLX(0,0)
        tmp(j) = CMPLX(0,0)
        DO i = 1, nreal
          eir(j) = eir(j) + g(j,i)*CONJG(g(j,i))
          eor(j) = eor(j) + h(j,i)*CONJG(h(j,i))
          tmp(j) = tmp(j) + g(j,i)*CONJG(h(j,i))
        ENDDO
        eir(j) = eir(j) / 2 / nreal
        eor(j) = eor(j) / 2 / nreal
        tmp(j) = tmp(j) / 2 / nreal
        z(j)   = tmp(j) / eir(j)
        cohere(j) = tmp(j)*CONJG(tmp(j)) / eir(j)/eor(j)
        PRINT *,period/j, sqrt(REAL(eir(j))), sqrt(REAL(eor(j))), 
     1                    sqrt(REAL(z(j)*CONJG(z(j))) ),
!     1                       tmp(j), z(j), 
     1                       REAL(cohere(j))
      ENDDO  

      RETURN
      END
