C*************************************************----------++++++++++!!
      SUBROUTINE upwell(fna, faa, fiw, deltaz, nlayer, w, region)
C     Compute Upwelling velocity given volume fluxes.
C       Note!! The sign is reversed from expectation.  That is,
C         upwelling velocities are negative.  This is because the
C         sea floor has been placed at +3800 meters, rather than
C         -3800.  BG 7-22-91.

      IMPLICIT none

      INTEGER nlayer, region

      DOUBLE PRECISION deltaz
      DOUBLE PRECISION fna(nlayer, region), faa(nlayer, region)
      DOUBLE PRECISION fiw(nlayer, region)
      DOUBLE PRECISION w(nlayer, region)

      INTEGER i, r

      INTEGER regs
      PARAMETER (regs = 3)
      DOUBLE PRECISION area(regs), rho(regs)
      INCLUDE "arrho.inc"

      DO 2000 r = 1, region
        w(nlayer, r) = 0.0D0
        w(1,      r) = 0.0D0
        DO 1000 i = 2, nlayer
C         Note that this summation is dependent, will not vectorize.
          w(i, r) = w(i-1, r) + ( rho(3)*faa(i-1, r)
     1                           +rho(2)*fna(i-1, r)
     2                           +rho(3)*fiw(i-1, r) )
     3          /rho(r)/area(r)
 1000   CONTINUE
        IF (ABS(w(nlayer, r)) .GT. 1.D-10 .OR.
     1      ABS(w(1     , r)) .GT. 1.D-10      ) THEN
          PRINT *,'mass conservation failure, w=',
     1                            w(nlayer,r), w(1,r), r
        ENDIF
 2000 CONTINUE

CD      DO 3010 i= 10, 16
CD        WRITE (*,9001) (w(i,r),r=1,region)
CD 3010 CONTINUE
CD 9001 FORMAT (3E13.5)

      RETURN
      END
