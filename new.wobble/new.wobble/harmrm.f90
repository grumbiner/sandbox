      SUBROUTINE harmrm(x, n, omega, a, b, m)
!     Subroutine to get harmonic amplitudes via a least square fit,
!       for an input data vector, at the specified frequencies.
!     x  is the data vector, of length n.
!     omega is the vector of frequencies, length m.
!     a is the aoefficient of the cosine term.
!     b is the coefficient of the sine term.
!     m is the number of frequencies to analyze.
!     lda is the maximum size of the coefficient matrix.  It is 
!       twice the size of the largest allowed input vector.
!     Modified to handle analysis of the mean.  11-30-86.  Robert Grumbine
!     omega(0) = 0 ( mean, frequency = 0)
!     b(0)     = 0

      INTEGER n, m
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:m-1), a(0:m-1), b(0:m-1)

!     Variables for the LINPACK routines.
      INTEGER lda, info
      PARAMETER (lda = 400)
      INTEGER ipvt(lda)

!     Local variables:
      DOUBLE PRECISION freq1, freq2
      INTEGER i, j
      DOUBLE PRECISION coeff( 0:lda-1, 0:lda-1 ), y(0:lda-1)
      DOUBLE PRECISION cossum, cosdif, sinsum, sindif
      DOUBLE PRECISION sumsin, sumcos
      DOUBLE PRECISION sum1, sum2, t

!HP$EMA coeff

!     Statement functions used in filling the coefficient matrix.
!***********************************************************----------!!
      sumcos(freq1)        = dcos( DBLE(n-1)*DBLE(freq1) / 2.D0 ) * &
          dsin( DBLE(n)*DBLE(freq1)/2.D0)                /          &
          dsin( DBLE(freq1)/ 2.D0)

      sumsin(freq1)        = dsin( DBLE(n-1)*DBLE(freq1) / 2.D0 ) * &
          dsin( DBLE(n)*DBLE(freq1)/2.D0)                /          &
          dsin( DBLE(freq1)/ 2.D0)
      
      cossum(freq1, freq2) = dcos(DBLE(n-1)*DBLE(freq1+freq2)/ 2.D0) * &
          dsin( DBLE(n) * DBLE( freq1+freq2 )/2.D0 )      /          &
          dsin( DBLE(freq1 + freq2)/2.D0)

      cosdif(freq1, freq2) = dcos(DBLE(n-1)*DBLE(freq1-freq2)/ 2.D0) * &
          dsin(DBLE(n) * DBLE(freq1-freq2) /2.D0)        /          &
          dsin(DBLE(freq1-freq2)/2.D0)

      sinsum(freq1, freq2) = dsin(DBLE(n-1)*DBLE(freq1+freq2)/ 2.D0) * &
           dsin( DBLE(n) *DBLE(freq1+freq2) /2.D0)          /          &
           dsin(DBLE(freq1+freq2)/ 2.D0)

      sindif(freq1, freq2) = dsin(DBLE(n-1)*DBLE(freq1-freq2)/ 2.D0) * &
           dsin( DBLE(n) *DBLE(freq1-freq2) /2.D0)          /          &
           dsin(DBLE(freq1-freq2)/ 2.D0)
!***********************************************************----------!!

!     Fill the coefficient and y matrices.
!     First take care of the j=0, i=0 terms (the terms due to the mean
!       velocity.
      coeff(0, 0) = FLOAT(n)
      DO j = 1, m-1
        coeff(2*j-1, 0) = ( sumcos(omega(j)) )
        coeff(2*j  , 0) = ( sumsin(omega(j)) )
        coeff(0, 2*j  ) = coeff(2*j  , 0)
        coeff(0, 2*j-1) = coeff(2*j-1, 0)
      ENDDO
      
      sum1 = 0.0
      DO j = 1, n
        sum1 = sum1 + x(j)
      ENDDO
      y(0) = sum1
!     Now find the coefficients for the terms for which neither frequency 
!       is 0.

      DO j = 1, m-1
        DO i = 1, m-1
         IF ( i .NE. j) THEN
          coeff(2*i-1, 2*j-1)= .5*(cossum( omega(i),omega(j)) +   &
                                    cosdif( omega(i), omega(j)) )
          coeff(2*i, 2*j-1) = .5*(sinsum( omega(j), omega(i)) -   &
                                    sindif( omega(j), omega(i)) )
          coeff(2*i-1, 2*j) = .5*(sinsum( omega(i), omega(j)) -   &
                                    sindif(omega(i), omega(j)) )
          coeff(2*i, 2*j) = .5*(cosdif( omega(i), omega(j)) -   &
                                    cossum( omega(i), omega(j)) )
         ELSE IF (omega(i) .NE. 0.) THEN
!         Must include special cases because for i=j the xdif terms
!           involve sin(n0)/sin(0). 
          coeff(2*i-1, 2*j-1)= .5*(cossum( omega(i),omega(j)) +   &
                                     DBLE(n)             )
          coeff(2*i, 2*j-1) = .5 *(sinsum( omega(j), omega(i)) -   &
                                     0.              )
          coeff(2*i-1, 2*j) = .5 *(sinsum( omega(i), omega(j)) -   &
                                     0.                   )
          coeff(2*i, 2*j) = .5 *(  DBLE(n)       -   &
                                    cossum( omega(i), omega(j)) )
         ELSE
          PRINT *,'error in cases for harmonic analysis.'
         ENDIF
        ENDDO

        sum1 = 0.D0
        sum2 = 0.D0
        DO t = 0.D0, DBLE(n-1), 1.D0
          sum1 = sum1 +DBLE( x(INT(t) + 1)) * cos(DBLE(omega(j)) * t)
          sum2 = sum2 +DBLE( x(INT(t) + 1)) * sin(DBLE(omega(j)) * t)
        ENDDO
        y( 2*j-1 ) = (sum1)
        y( 2*j   ) = (sum2)
!       ENDIF

      ENDDO 

!     Call LAPACK routine
      !PRINT *,"calling sgesv, m = ",m
      CALL DGESV(2*m-1, 1, coeff, lda, ipvt, y, 2*m-1, info)
      IF (info .NE. 0) PRINT *,'Nearly singular matrix.  info=',info

!     Enter the coefficients in the proper array. (a, b)
      DO j = 1, m-1
        a(j) = y(2*j-1)
        b(j) = y(2*j  )
      ENDDO
      a(0) = y(0)
      b(0) = 0.0

      RETURN
      END
