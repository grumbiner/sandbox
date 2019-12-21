      SUBROUTINE FOUR1(data, nn, isign)
C     Replaces DATA by its discrete fourier transform if ISIGN is +1
C     Or computes NN times its inverse discrete if ISIGN is -1.  Data
C     is a complex array of length NN, or a real array of length 2*NN
C     NN must be a power of two.
C     Original from Numerical Recipes.
C     Robert Grumbine 30 September 1998

      IMPLICIT none

      INTEGER isign, nn
      REAL data(2*nn)
      
      DOUBLE PRECISION wr, wi, wpr, wpi, wtemp, theta
      INTEGER n, i, j, m, nmax, istep
      REAL tempr, tempi
      
      i = INT(0.5 + LOG(FLOAT(nn))/LOG(2.0) )
      IF (nn .NE. 2**i) THEN
        PRINT *,'Input vector is not a power of two!'
        PRINT *,'Aborting from FOUR1'
        RETURN
      ENDIF

      n = 2 * nn
      j = 1
      DO 11 i = 1, n, 2
        IF (j .GT. i) THEN
          tempr = data(j)
          tempi = data(j+1)
          data(j)   = data(i)
          data(j+1) = data(i+1)
          data(i)   = tempr
          data(i+1) = tempi
        ENDIF
        m = n/2
   1    CONTINUE
        IF ( (m .GE. 2) .AND. (j .GT. m) ) THEN
          j = j - m
          m = m / 2
          GO TO 1
        ENDIF
        j = j + m
  11  CONTINUE

      nmax = 2
    2 CONTINUE
      IF (n .GT. nmax) THEN
        istep = 2*nmax
        theta = 6.28318530717959D0  / (isign*nmax)
        wpr = -2.D0*DSIN(0.5D0*theta)**2
        wpi = DSIN(theta)
        wr = 1.D0
        wi = 0.D0
        DO 13 m = 1, nmax, 2
          DO 12 i = m, n, istep
            j = i + nmax
            tempr=SNGL(wr)*data(j)  -SNGL(wi)*data(j+1)
            tempi=SNGL(wr)*data(j+1)+SNGL(wi)*data(j)
            data(j)   = data(i)   - tempr
            data(j+1) = data(i+1) - tempi
            data(i)   = data(i)   + tempr
            data(i+1) = data(i+1) + tempi
   12     CONTINUE
          wtemp = wr
          wr = wr*wpr - wi*wpi    + wr
          wi = wi*wpr + wtemp*wpi + wi
   13   CONTINUE
        nmax = istep
        GO TO 2
      ENDIF

      isign = 0
      RETURN
      END
