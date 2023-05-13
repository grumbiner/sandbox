      PROGRAM aaa
      IMPLICIT none
      INTEGER nt
      PARAMETER (nt = 24628)
      REAL t(nt)
      INTEGER histo(-300:300)
      DOUBLE PRECISION alpha, beta, gamma, delta, epsi
      DOUBLE PRECISION d
      REAL i, tmp, dum
      INTEGER j
      REAL ar11, ar21, ar22
      
      histo = 0;

      DO j = 1, nt
        READ(*,*) i, tmp, dum
        t(j) = tmp
        histo( NINT(0.5 + tmp) ) = histo( NINT(0.5 + tmp) ) +1 
      ENDDO
      DO j = -300, 300
        IF (histo(j) .NE. 0) THEN
          PRINT *,j,histo(j)
        ENDIF
      ENDDO
      END
