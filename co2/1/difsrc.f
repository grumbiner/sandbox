      SUBROUTINE difsrc(x, kappa, nlayer, nchem, regions, qdif)
C     Compute the source due to lateral difsion between domains.
C     CO2 layered model.
C     BG 12/9/91

      INTEGER nlayer, nchem, regions
      DOUBLE PRECISION x(nlayer, nchem, regions), kappa(nlayer)
      DOUBLE PRECISION qdif(nlayer, nchem, regions)

      INTEGER i, j, k
      REAL lenna, lenso, area(3), rho(3)
      INCLUDE "arrho.inc"

      k = 1
      DO 1000 j = 1, nchem
        DO 1100 i =  1, nlayer
          qdif(i, j, k) = kappa(i)*(x(i,j,2)-x(i,j,1))/lenna
     1                  -kappa(i)*(x(i,j,1)-x(i,j,3))/lenso
 1100   CONTINUE
 1000 CONTINUE

      k = 2
      DO 2000 j = 1, nchem
        DO 2100 i =  1, nlayer
          qdif(i, j, k) = -kappa(i)*(x(i,j,2)-x(i,j,1))/lenna
 2100   CONTINUE
 2000 CONTINUE

      k = 3
      DO 3000 j = 1, nchem
        DO 3100 i =  1, nlayer
          qdif(i, j, k) =
     1                  kappa(i)*(x(i,j,1)-x(i,j,3))/lenso
 3100   CONTINUE
 3000 CONTINUE

      DO 4000 k = 1, 3
        DO 4100 j = 1, nchem
          DO 4200 i = 1, nlayer
            qdif(i,j,k) = qdif(i,j,k)*1.E6
 4200     CONTINUE
 4100   CONTINUE
 4000 CONTINUE

      RETURN
      END
