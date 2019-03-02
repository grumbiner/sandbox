      SUBROUTINE arho(slp, temp, q, rho, nlat, nlong)
C     Compute the atmospheric density field on a 2d mesh.
C     Equation of state from Wallace and Hobbs, pp. 48.
C     Implemented by Robert Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER nlat, nlong
      REAL slp(nlong, nlat), temp(nlong, nlat), q(nlong, nlat)
      REAL rho(nlong, nlat)

      REAL Rd, epsiln
      PARAMETER (Rd = 287.06)
      PARAMETER (epsiln = 0.62199)

      INTEGER i, j

C     Bullet-proofing variables:
      REAL plow, phi, tlow, thi, qlow, qhi, rholow, rhohi
      PARAMETER (plow = 8.50E4)
      PARAMETER (phi  = 1.15E5)
      PARAMETER (tlow = 220.0 )
      PARAMETER (thi  = 325.0 )
      PARAMETER (qlow =  0.0  )
      PARAMETER (qhi  = 25.0E-3)
      PARAMETER (rholow = 0.95)
      PARAMETER (rhohi  = 1.85)

C     Bullet-proofing: Verify that the thermodynamic inputs are
C       all within reasonable ranges.
      DO 9000 j = 1, nlat
        DO 9010 i = 1, nlong
          IF (slp(i,j) .GT. phi) PRINT *,'slp too high ',i,j,slp(i,j)
          IF (slp(i,j) .LT. plow) PRINT *,'slp too low ',i,j,slp(i,j)
          IF (temp(i,j) .GT. thi) PRINT *,'temp too high ',i,j,temp(i,j)
          IF (temp(i,j) .LT. tlow) PRINT *,'temp too low ',i,j,temp(i,j)
          IF (q(i,j) .GT. qhi) PRINT *,'q too high ',i,j,q(i,j)
          IF (q(i,j) .LT. qlow) PRINT *,'q too low ',i,j,q(i,j)
 9010   CONTINUE
 9000 CONTINUE

C     Operational Code
      DO 1000 j = 1, nlat
        DO 1010 i = 1, nlong
          rho(i,j) = slp(i,j)/Rd/temp(i,j) * (1.-q(i,j)*(1.-epsiln))
 1010   CONTINUE
 1000 CONTINUE

C     Bullet proofing: Verify that density output is within reasonable
C       range.
      DO 9100 j = 1, nlat
        DO 9110 i = 1, nlong
          IF (rho(i,j) .GT. rhohi) THEN
            PRINT *,'computed rho to large ',i,j,rho(i,j)
            rho(i,j) = rhohi
            PRINT *,'rho reset to ',rhohi
          ENDIF
          IF (rho(i,j) .LT. rholow) THEN
            PRINT *,'computed rho too low ',i,j,rho(i,j)
            rho(i,j) = rholow
            PRINT *,'rho reset to ',rholow
          ENDIF
 9110   CONTINUE
 9100 CONTINUE

      RETURN
      END
