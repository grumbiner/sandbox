      PROGRAM sample
      IMPLICIT none
      INTEGER i
      REAL period
      PARAMETER (period = 433)
      REAL pi, x

      pi = ABS(ACOS(-1.0))
      DO i = 1, 17595
        print *,cos(2.*pi*(i-1)/period)
      ENDDO

      END 
