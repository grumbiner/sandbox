

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
      REAL x(n)
      DOUBLE PRECISION omega(0:m-1), a(0:m-1), b(0:m-1)

C     Variables for the LINPACK routines.
      INTEGER lda, info
      PARAMETER (lda = 400)
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
          PRINT *,'i, j, m omega = ',i,j,m, omega(j)
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
