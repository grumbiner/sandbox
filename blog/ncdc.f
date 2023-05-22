      PROGRAM weights
      IMPLICIT none

      INTEGER i, j, k, n, nmax, ny
      PARAMETER (ny = 1613)
      PARAMETER (nmax = 4)

      REAL dum, y, t, x, dx, legendre, temp(ny)
      DOUBLE PRECISION sum(5), norm(5)

      OPEN (10, FILE="1880-2014.csv", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, ny
        READ (10,*) y, t
        temp(i) = t
      ENDDO

      k = 15.*12  ! monthly data!
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
        
      DO j = k+1, ny-k
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
!        WRITE (*,*) j, sum(1), sum(2), sum(3), sum(4), sum(5)

      ENDDO

 9001 FORMAT (I04,",",5(F9.4,",") )

      END
