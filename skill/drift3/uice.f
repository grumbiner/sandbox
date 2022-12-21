      SUBROUTINE uice(ug, vg, ui, vi, nlong, nlat)
C     Compute the motion of the non-interacting floes.
C     Bob Grumbine 17 September 1993.
C     U10 wind rule implemented 15 March 2007 

      IMPLICIT none
CD      INCLUDE "sicedrft.inc"

C     Declare the arguments
      INTEGER nlat, nlong
      REAL ug(nlong, nlat), vg(nlong, nlat)
      REAL ui(nlong, nlat), vi(nlong, nlat)

C     Declare local utility variables
      INTEGER i, j

C     Declare the flow model.
      REAL pi, betar
      REAL alpha1, beta1
      REAL alpha2, beta2
C These are the geostrophic wind values, superceded 15 March 2007
COLD      PARAMETER (alpha1 = 0.008)
COLD      PARAMETER (beta1  = 8.0)
COLD      PARAMETER (alpha2 = 0.0303)
COLD      PARAMETER (beta2  = -23.4)
      PARAMETER (alpha1 = 0.01468)
      PARAMETER (beta1  = 28.0)
      PARAMETER (alpha2 = 0.01468)
      PARAMETER (beta2  = -28.0)

      PARAMETER (pi    = 3.141592654)

C     Operational Code.
C     Compute the gridded ice drift velocity field. Note j convention is
C       reversed from MRF standard, and runs south to north.
C     Northern Hemisphere
      betar = pi*beta1/180.
      DO 2000 j = nlat/2+1, nlat
        DO 2010 i = 1, nlong
          ui(i,j) = alpha1*(COS(betar)*ug(i,j)+SIN(betar)*vg(i,j))
          vi(i,j) = alpha1*(-SIN(betar)*ug(i,j)+COS(betar)*vg(i,j))
 2010   CONTINUE
 2000 CONTINUE
C     Southern Hemisphere
      betar = pi*beta2/180.
      DO 2100 j = 2, nlat/2
        DO 2110 i = 1, nlong
          ui(i,j) = alpha2*(COS(betar)*ug(i,j)+SIN(betar)*vg(i,j))
          vi(i,j) = alpha2*(-SIN(betar)*ug(i,j)+COS(betar)*vg(i,j))
 2110   CONTINUE
 2100 CONTINUE

      PRINT *,'debug max, min u, v ',MAXVAL(ui), MINVAL(ui), 
     1               MAXVAL(vi), MINVAL(vi)

      RETURN
      END
