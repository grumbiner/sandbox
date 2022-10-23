
      SUBROUTINE ensemble(g, h, nsum, nreal, z, cohere, per, over)
      IMPLICIT none
      REAL per
      INTEGER nsum, nreal, over
      COMPLEX g(nsum, nreal), h(nsum, nreal)
      COMPLEX z(nsum)
      COMPLEX cohere(nsum)

      COMPLEX eir(nsum), ero(nsum), tmp(nsum)

      INTEGER i, j

      DO j = over, nsum
        eir(j) = CMPLX(0,0)
        ero(j) = CMPLX(0,0)
        tmp(j) = CMPLX(0,0)
        DO i = 1, nreal
          eir(j) = eir(j) + g(j,i)*CONJG(g(j,i))
          ero(j) = ero(j) + h(j,i)*CONJG(h(j,i))
          tmp(j) = tmp(j) + g(j,i)*CONJG(h(j,i))
        ENDDO
        eir(j) = eir(j) / 2 / nreal
        ero(j) = ero(j) / 2 / nreal
        tmp(j) = tmp(j) / 2 / nreal
        z(j)   = tmp(j) / eir(j)
        cohere(j) = tmp(j)*CONJG(tmp(j)) / eir(j)/ero(j)
        PRINT *,per/(float(j)/float(over)), sqrt(REAL(eir(j))), 
     1                       sqrt(REAL(ero(j))), 
!     1                       tmp(j), z(j), 
     1                       REAL(cohere(j))
      ENDDO  

      RETURN
      END
