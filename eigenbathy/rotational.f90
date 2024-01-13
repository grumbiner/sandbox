      PROGRAM eigen
      IMPLICIT none
!     Demo/test for finding eigenvalues of a matrix
! Vorticity modes per Rao + Schwab 1975
      INTEGER n, ntopo, navg
      PARAMETER (ntopo = 4320/2)
      PARAMETER (navg = 8)
      PARAMETER (n = ntopo/navg + 1)

      DOUBLE PRECISION a(n,n), ldz(n)
      DOUBLE PRECISION wr(n), wi(n)
      INTEGER iopt
      LOGICAL select(n)
      INTEGER naux
      PARAMETER (naux = n)
      DOUBLE PRECISION aux(naux)

      INTEGER i, j, method, index
!     Some physical variables
      REAL g, length, dx, PI
      PARAMETER (g = 9.8) !g in m/s^2
      PARAMETER (PI = 3.141592654)
      REAL h(ntopo)
      DOUBLE PRECISION htmp(n-1),count(n-1)
      DOUBLE PRECISION h1(n-1), h2(n-1), h3(n-1), h4(n-1)
      DOUBLE PRECISION h5(n-1), h6(n-1)
!      REAL tau(n), omega(n)
      DOUBLE PRECISION sum, sumhalf, suminv, suminvhalf
      DOUBLE PRECISION sumsq, suminvsq
      CHARACTER*80 fname

      iopt = 0

      length = (180./ntopo)*111.1e3    
      dx = length / (n - 1)
      PRINT *,'length, dx = ',length, dx

!     Set up from bathymetry
!      OPEN (10, FILE="topos", FORM="FORMATTED", STATUS="OLD")
      count = 0.0
      PRINT *,'jmax = ',n-1
      DO j = 1, n-1

        sum = 0
        sumhalf = 0
        suminvhalf = 0
        suminv     = 0
        sumsq      = 0
        suminvsq   = 0

        DO i = 1, navg
          index = i+(j-1)*navg
!          READ (10,*) h(index)
          h(index) = -4000.
          IF (h(index) < 0.) THEN
            count(j) = count(j) + 1
            h(index) = -h(index)     ! Flip since etopo lists negative for ocean
            sum        = sum + h(index)
            sumhalf    = sumhalf + sqrt(h(index))
            suminv     = suminv  + 1./h(index)
            suminvhalf = suminvhalf + 1./sqrt(h(index))
            sumsq      = sumsq    + h(index)**2
            suminvsq   = suminvsq + 1./h(index)**2
          ENDIF
        ENDDO

        IF (count(j) > 0 ) THEN
          h1(j) = sum / count(j)
          h2(j) = (sumhalf / count(j))**2
          h3(j) = count(j) / suminv 
          h4(j) = (count(j) / suminvhalf)**2
          h5(j) = (sumsq / count(j))**0.5
          h6(j) = (count(j) / suminvsq)**0.5
        ELSE
          h1(j) = 1.
          h2(j) = 1.
          h3(j) = 1.
          h4(j) = 1.
          h5(j) = 1.
          h6(j) = 1.
        ENDIF

        PRINT *,j, h1(j), h2(j), h3(j), h4(j), h5(j), h6(j)
      ENDDO
      PRINT *,'done averaging bathy'


!     Repeat for each topography:
      DO method = 1, 6
        IF (method .EQ. 1) THEN
          htmp = h1
        ELSEIF (method .EQ. 2) THEN
          htmp = h2
        ELSEIF (method .EQ. 3) THEN
          htmp = h3
        ELSEIF (method .EQ. 4) THEN
          htmp = h4
        ELSEIF (method .EQ. 5) THEN
          htmp = h5
        ELSEIF (method .EQ. 6) THEN
          htmp = h6
        ELSE
          PRINT *,'error -- method out of range'
          STOP
        ENDIF
        PRINT *,'done assigning bathy',method

! unused (?)
!        DO i = 1, n
!          tau(i) = sqrt(g*htmp(i)/dx/dx)
!        ENDDO

!     Set up dummy arrays/clear arrays from prior round
        a = 0.d0
        ldz = 1
        aux = 0.d0
        wr = 0.d0
        wi = 0.d0
        PRINT *,' set dummy arrays to 0'

!       Construct the finite differencing:
! Boundary conditions u = 0 at 1, n:
        a(2,1) =  1.0/htmp(1)
        a(1,1) = -1.0/htmp(1)
        a(n,n)   =  1.0/htmp(n-1)
        a(n-1,n) = -1.0/htmp(n-1)
        DO j = 2, n-1
          a(j-1,j) =  1/htmp(j-1)
          a(j,  j) = -1/htmp(j-1) - 1/htmp(j)
          a(j+1,j) =  1/htmp(j)
        ENDDO
        a = a / dx**2
        PRINT *,'have filled a'

!     Call ESSL function:
!D      CALL SGEEV(iopt, a, n, wr, wi, ldz, select, n, aux, naux)
        CALL DGEEV(iopt, a, n, wr, wi, ldz, select, n, aux, naux)

        IF (navg .LT. 10) THEN
          WRITE(fname,9001) navg, method
 9001     FORMAT("avg_",I1,"_",I1)
        ELSE IF (navg .GE. 10 .AND. navg .LT. 100) THEN
          WRITE(fname,9002) navg, method
 9002     FORMAT("avg_",I2,"_",I1)
        ELSE IF (navg .GT. 100) THEN
          WRITE(fname,9003) navg, method
 9003     FORMAT("avg_",I3,"_",I1)
        ENDIF

        OPEN (10+method, FILE=fname, FORM="FORMATTED", STATUS="NEW")
! Note that for vorticity modes, don't have neat relationship.  W is the only thing
!   to bother with.
        DO i = 1, n
          WRITE (10+method,*) REAL(1./wr(i)), wi(i), REAL(1./wi(i)), &
               wi(i)
        ENDDO
        CLOSE (10+method)

      ENDDO !method

      STOP
      END
