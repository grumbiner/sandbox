      SUBROUTINE harmim(x, time, n, omega, a, b, m, rcond)
C     Subroutine to get harmonic amplitudes via a least square fit,
C       for an input data vector, at the specified frequencies.
C     x  is the data vector, of length n.
C     omega is the vector of frequencies, length m.
C     a is the coefficient of the cosine term.
C     b is the coefficient of the sine term.
C     m is the number of frequencies to analyze.
C     lda is the maximum size of the coefficient matrix.  It is 
C       twice the size of the largest allowed input vector.
C     Modified to handle analysis of the mean.  11-30-86.  BG
C     omega(0) = 0 ( mean, frequency = 0)
C     b(0)     = 0
C     Version harmim to analyze irregularly spaced data with a mean:
C       started 3-2-87.  Robert Grumbine

      INTEGER n, m
      REAL x(n), time(n)
      REAL omega(0:m-1), a(0:m-1), b(0:m-1)

C     Variables for the LINPACK routines.
      INTEGER lda, info
      PARAMETER (lda = 400)
      INTEGER ipvt(lda)
      REAL rcond, z(lda)

C     Local variables:
      REAL freq1, freq2
      INTEGER i, j, t
      REAL coeff( 0:lda-1, 0:lda-1 ), y(0:lda-1)
      REAL sumcos, sumsin, ccsum, sssum, scsum
      DOUBLE PRECISION sum1, sum2

CHP$EMA coeff

C***********************************************************----------!!

C     Fill the coefficient and y matrices.
C     First take care of the j=0, i=0 terms (the terms due to the mean
C       velocity.
      coeff(0, 0) = FLOAT(n)
      DO 100 j = 1, m-1
        coeff(2*j-1, 0) = sumcos(omega(j), time, n) 
        coeff(2*j  , 0) = sumsin(omega(j), time, n)
        coeff(0, 2*j  ) = coeff(2*j  , 0)
        coeff(0, 2*j-1) = coeff(2*j-1, 0)
  100 CONTINUE
      
      sum1 = 0.D0
      DO 200 j = 1, n
        sum1 = sum1 + DBLE(x(j))
  200 CONTINUE
      y(0) = SNGL(sum1)
C     Now find the coefficients for the terms for which neither frequency 
C       is 0.

      DO 1000 j = 1, m-1
        DO 1010 i = 1, m-1
          coeff(2*i-1, 2*j-1) = ccsum(omega(i), omega(j), time, n)
          coeff(2*i, 2*j-1)   = scsum(omega(j), omega(i), time, n)
          coeff(2*i-1, 2*j)   = scsum(omega(i), omega(j), time, n)
          coeff(2*i,   2*j)   = sssum(omega(i), omega(j), time, n)
 1010   CONTINUE

        sum1 = 0.D0
        sum2 = 0.D0
        DO 2000 t = 1, n
          sum1 = sum1 + DBLE( x(INT(t)) ) *
     1                  dcos(DBLE(omega(j)) * time(t))
          sum2 = sum2 + DBLE( x(INT(t)) ) * 
     1                  dsin(DBLE(omega(j)) * time(t))
 2000   CONTINUE
        y( 2*j-1 ) = SNGL(sum1)
        y( 2*j   ) = SNGL(sum2)
C       ENDIF

 1000 CONTINUE


C     Call LINPACK routine to factor the matrix.  Would like to use
C       SGECO to find the condition number as well, but that is not
C       available on the HP.
C     Added 3-17-87.
      CALL SGECO( coeff, lda, 2*m-1, ipvt, rcond, z)
C     IF (info .NE. 0) PRINT *,'Nearly singular matrix.  info=',info
C     PRINT *,'The condition number was',rcond
C     PRINT *,'Probably no more than', -ALOG10(rcond),'digits of accurac
C    1y were lost.'
 
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
