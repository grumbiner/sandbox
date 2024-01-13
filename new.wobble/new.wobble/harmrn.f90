
!-------------------------------------------------------
! Aged descendant of original, 11/1986, harmrn:
      SUBROUTINE harmrn(x, npts, omega, a, b, m)
      IMPLICIT none
      INTEGER npts, m
      DOUBLE PRECISION x(npts), omega(m), a(m), b(m)

!     Variables for the LINPACK routines.
      INTEGER lda, info
      PARAMETER (lda = 500)
      INTEGER ipvt(lda)

!     Local variables:
      DOUBLE PRECISION freq1, freq2
      INTEGER i, j, t
      DOUBLE PRECISION coeff( lda, lda ), y(lda)
      DOUBLE PRECISION cossum, cosdif, sinsum, sindif
      DOUBLE PRECISION sumsin, sumcos
      DOUBLE PRECISION sum1, sum2

!     Statement functions used in filling the coefficient matrix.
!***********************************************************----------!!

      cossum(freq1, freq2) = dcos(DBLE(npts-1)*DBLE(freq1+freq2)/ 2.D0)*  &
         dsin( DBLE(npts) * DBLE( freq1+freq2 )/2.D0 )      /  &
         dsin( DBLE(freq1 + freq2)/2.D0)

      cosdif(freq1, freq2) = dcos(DBLE(npts-1)*DBLE(freq1-freq2)/ 2.D0)*  &
         dsin(DBLE(npts) * DBLE(freq1-freq2) /2.D0)        /  &
         dsin(DBLE(freq1-freq2)/2.D0)

      sinsum(freq1, freq2) = dsin(DBLE(npts-1)*DBLE(freq1+freq2)/ 2.D0)*  &
          dsin( DBLE(npts) *DBLE(freq1+freq2) /2.D0)          /  &
          dsin(DBLE(freq1+freq2)/ 2.D0)

      sindif(freq1, freq2) = dsin(DBLE(npts-1)*DBLE(freq1-freq2)/ 2.D0)*  &
           dsin( DBLE(npts) *DBLE(freq1-freq2) /2.D0)          /  &
           dsin(DBLE(freq1-freq2)/ 2.D0)

!***********************************************************----------!!

!     Fill the coefficient and y matrices.
!     Find the coefficients for the terms for which neither frequency
!       is 0.
      DO 1000 j = 1, m
        DO i = 1, m
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
          coeff(2*i-1, 2*j-1)= .5*(cossum( omega(i),omega(j)) +  &
                                     DBLE(npts)             )
          coeff(2*i, 2*j-1) = .5 *(sinsum( omega(j), omega(i)) -   &
                                     0.              )
          coeff(2*i-1, 2*j) = .5 *(sinsum( omega(i), omega(j)) -   &
                                     0.                   )
          coeff(2*i, 2*j) = .5 *(  DBLE(npts)       -   &
                                    cossum( omega(i), omega(j)) )
         ELSE
          PRINT *,'error in cases for harmonic analysis.'
         ENDIF
        ENDDO 

        sum1 = 0.D0
        sum2 = 0.D0
        DO t = 0, npts-1
          sum1 = sum1 +DBLE( x((t) + 1)) * cos(DBLE(omega(j)) * DBLE(t))
          sum2 = sum2 +DBLE( x((t) + 1)) * sin(DBLE(omega(j)) * DBLE(t))
        ENDDO
        y( 2*j-1 ) = (sum1)
        y( 2*j   ) = (sum2)
!       ENDIF

 1000 CONTINUE

!     Call LAPACK routine
      !PRINT *,"calling sgesv, m = ",m
      CALL DGESV(2*m, 1, coeff, lda, ipvt, y, 2*m, info)
      IF (info .NE. 0) PRINT *,'Nearly singular matrix.  info=',info

!     Enter the coefficients in the proper array. (a, b)
      DO j = 1, m
        a(j) = y(2*j-1)
        b(j) = y(2*j  )
      ENDDO

      RETURN
      END
