      PROGRAM getfreqs
      IMPLICIT none
C Extract a set of frequencies from a data set.  Specify by way of Doodson-like
C   numbers

C Math
      DOUBLE PRECISION pi

! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001

      DOUBLE PRECISION omega_day, omega_month
      DOUBLE PRECISION omega_e_sidereal, omega_e_anom, omega_e_tropical
      PARAMETER (omega_day = 1./1.0)
      PARAMETER (omega_e_sidereal  = 1./365.256363) ! sidereal year
      PARAMETER (omega_e_anom = 1./365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = 1./365.24190) !tropical year
      DOUBLE PRECISION  omega_prec
      PARAMETER (omega_prec = omega_e_sidereal / 20940. )

      DOUBLE PRECISION omega_j, omega_s, omega_v
      DOUBLE PRECISION omega_n, omega_u, omega_mars 
      PARAMETER (omega_v    = 1./224.701)
      PARAMETER (omega_mars = 1./ 686.980)  
      PARAMETER (omega_j    = 1./4332.589)
      PARAMETER (omega_s    = 1./10759.22)
      PARAMETER (omega_u    = 1./30685.4)
      PARAMETER (omega_n    = 1./60189.0)

      DOUBLE PRECISION  omega_moon, omega_perigee, omega_node
      PARAMETER (omega_moon    = 1./27.3217)
      PARAMETER (omega_perigee = 1./365.256363/8.85)
      PARAMETER (omega_node    = 1./365.256363/18.613)

C Time series
      DOUBLE PRECISION, ALLOCATABLE :: x(:), xfilt(:)
      DOUBLE PRECISION, ALLOCATABLE :: omega(:), a(:), b(:)
      DOUBLE PRECISION, ALLOCATABLE :: a2(:), b2(:)
      DOUBLE PRECISION, ALLOCATABLE :: freq(:), amplitude(:), phase(:)
      DOUBLE PRECISION, ALLOCATABLE :: amplitude2(:), phase2(:)
      INTEGER, ALLOCATABLE :: day(:), month(:)
      INTEGER, ALLOCATABLE :: anom(:), sidereal(:)
      INTEGER, ALLOCATABLE :: venus(:), jupiter(:), saturn(:)

C Misc
      INTEGER i, todo
      INTEGER npts
      DOUBLE PRECISION dt, sum, mean, sum2, mean2, d1, d2, d3, d4
      CHARACTER*70 fname
      DOUBLE PRECISION intercept, slope, correl
      DOUBLE PRECISION, ALLOCATABLE :: y(:)


C ------------- Start executable section ------------------------------
C -------------- Get data to analyze
      READ (*,*) fname
      OPEN (11, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      READ (11,*) dt
      dt = 1./100.
      READ (11,*) npts
      READ (11,*) fname
      ALLOCATE (x(npts))
      ALLOCATE (xfilt(npts))
      ALLOCATE (y(npts))
      CLOSE (11)

      pi = DABS(DACOS(-1.D0))

      todo = npts/2 - 1
      PRINT *,"upper end of todo is ",todo
      todo = 1000
      ALLOCATE (omega(todo), a(todo), a2(todo), b(todo), b2(todo))
      ALLOCATE (freq(todo), amplitude(todo), amplitude2(todo))
      ALLOCATE (phase(todo), phase2(todo))
      DO i = 1, todo
        omega(i) = 2.*pi*FLOAT(i)/FLOAT(npts)
        PRINT *,'analysing ',omega(i), 2.*pi/omega(i)
      ENDDO


      !Read in data and ensure mean is zero:
      OPEN (12, FILE=fname, FORM="FORMATTED", STATUS="OLD")
      sum = 0.D0
      DO i = 1, npts
        READ (12, *) d1, x(i), xfilt(i)
        sum = sum + x(i)
        sum2 = sum2 + xfilt(i)
        y(i) = i
      ENDDO
      CLOSE (12)
      mean = sum / npts
      mean2 = sum2 / npts
      PRINT *,"means were ", mean, mean2

        x = x - mean
        xfilt = xfilt - (sum2 / npts)

C---------------------------------------------------------------------------

      PRINT *,"series 1 then series 2"
      CALL harmrm(x, npts, omega, a, b, todo)
      PRINT *,"now working on series2"
      CALL harmrm(xfilt, npts, omega, a2, b2, todo)
      PRINT *,"now working on series2"
      DO i = 1, todo
        amplitude(i)  = SQRT(a(i)* a(i) +b(i)* b(i))
        amplitude2(i) = SQRT(a2(i)*a2(i)+b2(i)*b2(i))
        phase(i)      = ATAN2(b(i), a(i)) *180./pi
        phase2(i)     = ATAN2(b2(i),a2(i))*180./pi
        WRITE (*, *) omega(i)/(2*pi*dt),",",
     1    amplitude(i),",", amplitude2(i),",",
     2    phase(i),",", phase2(i),",",
     3               2.D0*pi/omega(i)*dt
      ENDDO

      END
C----------------------------------------------------------------------------
      SUBROUTINE extract(x, n, omega, a, b, m, dt)
      INTEGER n, m
      DOUBLE PRECISION x(n), omega(0:m-1), a(0:m-1), b(0:m-1)

      DO i = 1, n
        DO j = 0, m-1
          x(i) = x(i) - a(j)*cos(omega(j)*DBLE(i)) 
     1                - b(j)*sin(omega(j)*DBLE(i))
        ENDDO
      ENDDO

      RETURN
      END


C----------------------------------------------------------------------------
      SUBROUTINE harmrm(x, n, omega, a, b, m)
C     Subroutine to get harmonic amplitudes via a least square fit,
C       for an input data vector, at the specified frequencies.
C     x  is the data vector, of length n.
C     omega is the vector of frequencies, length m.
C     a is the aoefficient of the cosine term.
C     b is the coefficient of the sine term.
C     m is the number of frequencies to analyze.
C     lda is the maximum size of the coefficient matrix.  It is
C       twice the size of the largest allowed input vector.
C     Modified to handle analysis of the mean.  11-30-86.  Robert Grumbine
C     omega(0) = 0 ( mean, frequency = 0)
C     b(0)     = 0

      INTEGER n, m
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:m-1), a(0:m-1), b(0:m-1)

C     Variables for the LINPACK routines.
      INTEGER lda, info
      PARAMETER (lda = 25 000)
      INTEGER ipvt(lda)

C     Local variables:
      DOUBLE PRECISION freq1, freq2
      INTEGER i, j, t
      DOUBLE PRECISION coeff( 0:lda-1, 0:lda-1 ), y(0:lda-1)
      DOUBLE PRECISION cossum, cosdif, sinsum, sindif
      DOUBLE PRECISION sumsin, sumcos
      DOUBLE PRECISION sum1, sum2

C     Statement functions used in filling the coefficient matrix.
C***********************************************************----------!!
      sumcos(freq1)        = dcos( DBLE(n-1)*DBLE(freq1) / 2.D0 ) *
     1    dsin( DBLE(n)*DBLE(freq1)/2.D0)                /
     2    dsin( DBLE(freq1)/ 2.D0)


      sumsin(freq1)        = dsin( DBLE(n-1)*DBLE(freq1) / 2.D0 ) *
     1    dsin( DBLE(n)*DBLE(freq1)/2.D0)                /
     2    dsin( DBLE(freq1)/ 2.D0)

      cossum(freq1, freq2) = dcos(DBLE(n-1)*DBLE(freq1+freq2)/ 2.D0) *
     1    dsin( DBLE(n) * DBLE( freq1+freq2 )/2.D0 )      /
     2    dsin( DBLE(freq1 + freq2)/2.D0)

      cosdif(freq1, freq2) = dcos(DBLE(n-1)*DBLE(freq1-freq2)/ 2.D0) *
     1    dsin(DBLE(n) * DBLE(freq1-freq2) /2.D0)        /
     2    dsin(DBLE(freq1-freq2)/2.D0)

      sinsum(freq1, freq2) = dsin(DBLE(n-1)*DBLE(freq1+freq2)/ 2.D0) *
     1     dsin( DBLE(n) *DBLE(freq1+freq2) /2.D0)          /
     2     dsin(DBLE(freq1+freq2)/ 2.D0)

      sindif(freq1, freq2) = dsin(DBLE(n-1)*DBLE(freq1-freq2)/ 2.D0) *
     1     dsin( DBLE(n) *DBLE(freq1-freq2) /2.D0)          /
     2     dsin(DBLE(freq1-freq2)/ 2.D0)
C***********************************************************----------!!

      !PRINT *,'entered harmrm'
C     Fill the coefficient and y matrices.
C     First take care of the j=0, i=0 terms (the terms due to the mean
C       velocity.
      coeff(0, 0) = FLOAT(n)
      DO 100 j = 1, m-1
        coeff(2*j-1, 0) = ( sumcos(omega(j)) )
        coeff(2*j  , 0) = ( sumsin(omega(j)) )
        coeff(0, 2*j  ) = coeff(2*j  , 0)
        coeff(0, 2*j-1) = coeff(2*j-1, 0)
  100 CONTINUE

      sum1 = 0.0
      DO 200 j = 1, n
        sum1 = sum1 + x(j)
  200 CONTINUE
      y(0) = sum1
C     Now find the coefficients for the terms for which neither frequency
C       is 0.

      DO 1000 j = 1, m-1
        DO 1010 i = 1, m-1
         IF ( i .NE. j) THEN
          coeff(2*i-1, 2*j-1)= .5*(cossum( omega(i),omega(j))+
     1                              cosdif( omega(i), omega(j)) )
          coeff(2*i, 2*j-1) = .5*(sinsum( omega(j), omega(i)) -
     1                              sindif( omega(j), omega(i)) )
          coeff(2*i-1, 2*j) = .5*(sinsum( omega(i), omega(j)) -
     1                              sindif(omega(i), omega(j)) )
          coeff(2*i, 2*j) = .5*(cosdif( omega(i), omega(j)) -
     1                              cossum( omega(i), omega(j)) )
         ELSE IF (omega(i) .NE. 0.) THEN
C         Must include special cases because for i=j the xdif terms
C           involve sin(n0)/sin(0).
          coeff(2*i-1, 2*j-1)= .5*(cossum( omega(i),omega(j))+
     1                               DBLE(n)             )
          coeff(2*i, 2*j-1) = .5 *(sinsum( omega(j), omega(i)) -
     1                               0.              )
          coeff(2*i-1, 2*j) = .5 *(sinsum( omega(i), omega(j)) -
     1                               0.                   )
          coeff(2*i, 2*j) = .5 *(  DBLE(n)       -
     1                              cossum( omega(i), omega(j)) )
         ELSE
          PRINT *,'error in cases for harmonic analysis.'
         ENDIF
 1010   CONTINUE

        sum1 = 0.D0
        sum2 = 0.D0
        DO 2000 t = 0, n-1
          sum1 = sum1 +DBLE( x(INT(t) + 1)) * 
     1                 dcos(DBLE(omega(j)) * DBLE(t) )
          sum2 = sum2 +DBLE( x(INT(t) + 1)) * 
     1                 dsin(DBLE(omega(j)) * DBLE(t) )
 2000   CONTINUE
        y( 2*j-1 ) = (sum1)
        y( 2*j   ) = (sum2)
C       ENDIF

 1000 CONTINUE


      !PRINT *,'calling lapack'
C     Call LAPACK routine
      CALL DGESV(2*m-1, 1, coeff, lda, ipvt, y, 2*m-1, info)
      !PRINT *,'back from lapack'
      IF (info .NE. 0) PRINT *,'Nearly singular matrix.  info=',info

C     Enter the coefficients in the proper array. (a, b)
      DO 3000 j = 1, m-1
        a(j) = y(2*j-1)
        b(j) = y(2*j  )
 3000 CONTINUE
      a(0) = y(0)
      b(0) = 0.0

      RETURN
      END
