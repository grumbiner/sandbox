      PROGRAM weights
      IMPLICIT none

      INTEGER i, j, k, n, nmax, ny
      PARAMETER (ny = 1613)
      PARAMETER (nmax = 4)

      REAL dum, y, t, x, dx, legendre, months(ny), temp(ny/12)
      DOUBLE PRECISION sum(5), norm(5)

      OPEN (10, FILE="1880-2014.csv", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, ny
        READ (10,*) y, t
        months(i) = t
      ENDDO

      temp = 0
      DO i = 1, ny
        temp((i-1)/12) = temp((i-1)/12) + months(i)
      ENDDO
      temp = temp / 12

!      k = 15.
      PRINT *,'what is k'
      READ (*,*) k
      dx = 1./FLOAT(k)

      norm = 0
      DO i = -k, k
        x = FLOAT(i) / FLOAT(k)
        IF (x .EQ. -1 .OR. x .EQ. 1.0) THEN
          DO n = 0, nmax
            norm(n+1) = norm(n+1) + legendre(n,x)*legendre(n,x)*1/2
          ENDDO
        ELSE
          DO n = 0, nmax
            norm(n+1) = norm(n+1) + legendre(n,x)*legendre(n,x)
          ENDDO
        ENDIF
      ENDDO
      norm = norm*dx
        
      DO j = k+1, ny/12-k
        sum = 0.0
        DO i = -k, k
          x = FLOAT(i) / FLOAT(k)
          IF (x .EQ. -1 .OR. x .EQ. 1.0) THEN
            DO n = 0, nmax
              sum(n+1) = sum(n+1) + legendre(n,x)*temp(j+i-1)/2
            ENDDO
          ELSE
            DO n = 0, nmax
              sum(n+1) = sum(n+1) + legendre(n,x)*temp(j+i-1)
            ENDDO
          ENDIF
        ENDDO
        DO n = 0, nmax
          sum(n+1) = sum(n+1)*dx / norm(n+1)
        ENDDO
        WRITE (*,9001) j, sum(1), sum(2), sum(3), sum(4), sum(5)
     1    ,sum(1) - 0.5*sum(3) + 0.375*sum(5),
     2     sum(2) - 1.5*sum(4),
     3     sum(3) - 7.5*sum(5)
!        WRITE (*,*) j, sum(1), sum(2), sum(3), sum(4), sum(5)

      ENDDO

 9001 FORMAT (I04,",",8(F9.4,",") )

      END

      REAL FUNCTION legendre(n, x)
      IMPLICIT none
      INTEGER n
      REAL x
      SELECT CASE (n)
        CASE (0)
          legendre = 1
        CASE (1)
          legendre = x
        CASE (2)
          legendre = 0.5*(3*x*x - 1)
        CASE (3)
          legendre = 0.5*(5*x*x*x-3*x)
        CASE (4)
          legendre = (35*x**4 - 30*x**2 + 3)/8
      END SELECT
      RETURN
      END 
