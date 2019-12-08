      PROGRAM testing
      IMPLICIT none
      DOUBLE PRECISION PI
!      PARAMETER (PI = 3.141592654)
      PARAMETER (PI = 3.14159265358979323846 )
      INTEGER i, j, n, m, nfreq
      PARAMETER (n = 75972) !Reanalysis to dec 31 1999
      PARAMETER (nfreq = 2000)
      DOUBLE PRECISION omega_e_sid, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_v, omega_j, omega_s, omega_u, omega_n
      DOUBLE PRECISION omega_mars
! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001
      REAL stepsize
      PARAMETER (stepsize = 0.25)
      PARAMETER (omega_e_sid  = stepsize*2.*PI/365.256363) ! sidereal year
      PARAMETER (omega_e_anom = stepsize*2.*PI/365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = stepsize*2.*PI/365.24190) !tropical year
      PARAMETER (omega_mars = stepsize*2.*PI/ 686.980)
      PARAMETER (omega_v = stepsize*2.*PI/224.701)
      PARAMETER (omega_j = stepsize*2.*PI/4332.589)
      PARAMETER (omega_s = stepsize*2.*PI/10759.22)
      PARAMETER (omega_u = stepsize*2.*PI/30685.4)
      PARAMETER (omega_n = stepsize*2.*PI/60189.)
      REAL omega_prec
      PARAMETER (omega_prec = omega_e_sid / 20940. )
      REAL omega_moon, omega_perigee, omega_node
      PARAMETER (omega_moon    = stepsize*2.*PI/27.32)
      PARAMETER (omega_perigee = stepsize*2.*PI/365.256/8.85)
      PARAMETER (omega_node    = stepsize*2.*PI/365.256/18.6)
      REAL omega_day
      PARAMETER (omega_day     = stepsize*2.*PI/1)
      DOUBLE PRECISION jd(n), x(n), y(n), utc(n), lod(n), r(n), rdot(n)
      DOUBLE PRECISION omega(0:nfreq-1), a(0:nfreq-1), b(0:nfreq-1)
      INTEGER todo
      INTEGER day(nfreq), month(nfreq), year(nfreq), venus(nfreq)
      INTEGER jupiter(nfreq)

!Want to go to double precision linpack call
      OPEN (10,FILE="doodson", FORM="FORMATTED")
      READ (10,*) todo
      DO i = 1, todo
        READ (10,*) day(i), month(i), year(i), venus(i), jupiter(i)
        !  PRINT *, i, day(i), month(i), year(i), venus(i), jupiter(i)
        !  PRINT *, omega_day, omega_moon, omega_e_anom, omega_v, omega_j
        omega(i) = day(i)     * omega_day +
     1             month(i)   * omega_moon +
     2             year(i)    * omega_e_anom +
     3             venus(i)   * omega_v +
     4             jupiter(i) * omega_j
        IF (omega(i) .NE. 0.0) THEN
          PRINT *,i,2.*PI*stepsize/omega(i), omega(i)
        ELSE
          PRINT *,i,"mean"
        ENDIF
       
      ENDDO
      CLOSE(10)

      m = todo
C now get data and beat tar out of it
      OPEN (11, FILE="dist", FORM="FORMATTED")
      DO i = 1, n
        READ (11,*) r(i)
      ENDDO
      CALL extract(r, n, omega, a, b, m, stepsize)

      
      STOP
      END
      SUBROUTINE extract(x, n, omega, a, b, m, stepsize)
      IMPLICIT none
      INTEGER n, m
      REAL stepsize
      DOUBLE PRECISION x(n), a(0:m-1), b(0:m-1), omega(0:m-1)
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.14159265358979323846 )
      INTEGER i, j

      CALL harmrm(x, n, omega, a, b, m)
      DO i = 0, m-1
        IF (omega(i) .NE. 0) THEN
        WRITE (*,*) stepsize*2.*PI/omega(i), omega(i), a(i), b(i),
     1         sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
        ELSE
        WRITE (*,*) "mean", omega(i), a(i), b(i),
     1         sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
        ENDIF
      ENDDO
      DO i = 1, n
        DO j = 0, m-1
          x(i) = x(i) - a(j)*cos(omega(j)*DBLE(i)) - 
     1                  b(j)*sin(omega(j)*DBLE(i))
        ENDDO
      ENDDO

 9001 FORMAT (6E22.13)

      RETURN
      END
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
      PARAMETER (lda = 400)
      INTEGER ipvt(lda)

C     Local variables:
      DOUBLE PRECISION freq1, freq2
      INTEGER i, j
      REAL coeff( 0:lda-1, 0:lda-1 ), y(0:lda-1)
      DOUBLE PRECISION cossum, cosdif, sinsum, sindif
      DOUBLE PRECISION sumsin, sumcos
      DOUBLE PRECISION sum1, sum2, t

CHP$EMA coeff

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

C     Fill the coefficient and y matrices.
C     First take care of the j=0, i=0 terms (the terms due to the mean
C       velocity.
      coeff(0, 0) = FLOAT(n)
      DO 100 j = 1, m-1
        coeff(2*j-1, 0) = SNGL( sumcos(omega(j)) )
        coeff(2*j  , 0) = SNGL( sumsin(omega(j)) )
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
          coeff(2*i-1, 2*j-1)= .5*SNGL(cossum( omega(i),omega(j))+
     1                              cosdif( omega(i), omega(j)) )
          coeff(2*i, 2*j-1) = .5*SNGL(sinsum( omega(j), omega(i)) -
     1                              sindif( omega(j), omega(i)) )
          coeff(2*i-1, 2*j) = .5*SNGL(sinsum( omega(i), omega(j)) -
     1                              sindif(omega(i), omega(j)) )
          coeff(2*i, 2*j) = .5*SNGL(cosdif( omega(i), omega(j)) -
     1                              cossum( omega(i), omega(j)) )
         ELSE IF (omega(i) .NE. 0.) THEN
C         Must include special cases because for i=j the xdif terms
C           involve sin(n0)/sin(0). 
          coeff(2*i-1, 2*j-1)= .5*SNGL (cossum( omega(i),omega(j))+
     1                               DBLE(n)             )
          coeff(2*i, 2*j-1) = .5 *SNGL (sinsum( omega(j), omega(i)) -
     1                               0.              )
          coeff(2*i-1, 2*j) = .5 *SNGL (sinsum( omega(i), omega(j)) -
     1                               0.                   )
          coeff(2*i, 2*j) = .5 *SNGL (  DBLE(n)       -
     1                              cossum( omega(i), omega(j)) )
         ELSE
          PRINT *,'error in cases for harmonic analysis.'
         ENDIF
 1010   CONTINUE

        sum1 = 0.D0
        sum2 = 0.D0
        DO 2000 t = 0.D0, DBLE(n-1), 1.D0
          sum1 = sum1 +DBLE( x(INT(t) + 1)) * cos(DBLE(omega(j)) * t)
          sum2 = sum2 +DBLE( x(INT(t) + 1)) * sin(DBLE(omega(j)) * t)
 2000   CONTINUE
        y( 2*j-1 ) = SNGL(sum1)
        y( 2*j   ) = SNGL(sum2)
C       ENDIF

 1000 CONTINUE


C     Call LINPACK routine to factor the matrix.  Would like to use
C       SGECO to find the condition number as well, but that is not
C       available on the HP.
      CALL SGEFA( coeff, lda, 2*m-1, ipvt, info)
      IF (info .NE. 0) PRINT *,'Nearly singular matrix.  info=',info

C     Solve for the coefficients.
      CALL SGESL(coeff, lda, 2*m-1, ipvt, y, 0)

C     Enter the coefficients in the proper array. (a, b)
      DO 3000 j = 1, m-1
        a(j) = y(2*j-1)
        b(j) = y(2*j  )
 3000 CONTINUE
      a(0) = y(0)
      b(0) = 0.0

      RETURN
      END
