      SUBROUTINE filtsalt(sshal, sdeep, sml, smld, nx, ny, errunit)
C     Ensure that the salinity is non-negative
C     Robert Grumbine
C     24 July 2002

      IMPLICIT none
      INCLUDE "icegrid.inc"

      INTEGER i, j, nx, ny, errunit
      REAL sshal(nx, ny), sdeep(nx, ny)
      REAL sml(nx, ny)
      REAL smld(nx, ny)

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          sml(i,j)   = AMAX1(0., sml(i,j) )
          smld(i,j)  = AMAX1(0., smld(i,j) )
          sshal(i,j) = AMAX1(0., sshal(i,j) )
          sdeep(i,j) = AMAX1(0., sdeep(i,j) )
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END 
