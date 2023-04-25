      PROGRAM aaa
      IMPLICIT none
      INTEGER nt
      PARAMETER (nt = 24628)
      REAL t(nt)

      REAL  tmp, dum
      INTEGER i,j

      DOUBLE PRECISION pi, freq(2), a(2), b(2)

      pi = DABS(DACOS(-1.D0))

      freq(1) = 0
      freq(2) = 2.*pi/365.256
      
      DO j = 1, nt
        READ(*,*) i, dum, tmp
        t(j) = tmp
      ENDDO
      CALL harmrm(t, nt, freq, a, b, 2)
    
      PRINT *,'mean = ',SQRT(a(1)*a(1)+b(1)*b(1))
      i = 2
      PRINT *,'annual = ',SQRT(a(i)*a(i)+b(i)*b(i)),
     1   ATAN2(b(i),a(i))*180./pi

      t = t - SQRT(a(1)*a(1)+b(1)*b(1))
      i = 2
      DO j = 1, nt
        t(j) = t(j) - a(i)*cos(freq(i)*DBLE(j))
     1              - b(i)*sin(freq(i)*DBLE(j))
        PRINT *,j,t(j)
      ENDDO

      END
