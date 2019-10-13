      SUBROUTINE seadj(p, t, z, nlat, nlong)
C     Perform an adiabatic sea level adjustment on arrays of pressure and
C       temperature fields.
C     Robert Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER nlat, nlong
      REAL p(nlong, nlat), t(nlong, nlat), z(nlong, nlat)

      REAL cp, rd, g
      PARAMETER (g  = 9.81)
      PARAMETER (rd = 287.06)
      PARAMETER (cp = rd*7./2.)

      INTEGER i, j

      DO 1000 j = 1, nlat
        DO 1010 i = 1, nlong
          p(i,j) = p(i,j)*(1.+g/cp/t(i,j)*z(i,j))**(+cp/rd)
 1010   CONTINUE
 1000 CONTINUE

      DO 2000 j = 1, nlat
        DO 2000 i = 1, nlong
          t(i,j) = t(i,j) + (g/cp)*z(i,j)
 2010   CONTINUE
 2000 CONTINUE

      RETURN
      END
