C*************************************************----------++++++++++!!
      SUBROUTINE bwsrc(fna, faa, fiw, aamix, w, q, x,
     1                           nlayer, deltaz, pchem, k,
     2                                    chemu, region, m)
C     Compute the chemical source strength due to bottom water fluxes
C       into or out of the world ocean.  8/4/91 BG.
      IMPLICIT none

      INTEGER nlayer, region, m, chemu, k
      DOUBLE PRECISION fna(nlayer, region), faa(nlayer, region)
      DOUBLE PRECISION fiw(nlayer, region)
      DOUBLE PRECISION q(nlayer), x(nlayer, chemu, region)
      DOUBLE PRECISION w(nlayer, region)
      DOUBLE PRECISION aamix, deltaz
      INTEGER pchem

      INTEGER nr, nlev
      PARAMETER (nr = 3)
      PARAMETER (nlev = 76)
      DOUBLE PRECISION area(nr), rho(nr), sum

      INCLUDE "chmptr.inc"

      INTEGER i, j
      DOUBLE PRECISION cna(nlev,nr), caa(nlev,nr), ciw(nlev,nr)
      REAL aadept(nr), aasour(nr), iwdept(nr), iwsour(nr)
      REAL nadept(nr), nasour(nr)
      INTEGER ina(nr), iaa(nr), iiw(nr)
      INTEGER inas(nr), iaas(nr), iiws(nr)

C     ANSI requires SAVE before data.
      SAVE  area, rho, aadept, aasour, iwdept, iwsour, nadept, nasour

      INCLUDE "arrho.inc"

C     This is taken from AABW
      INCLUDE "bw.inc"

C     Having spent an age or two defining variables, now compute
C       the source strength due to bottom water and the associated
C       upwelling.
C     NOTE!! The AABW mixture being used is not strictly consistent
C       with the above table, having been derived from a different
C       source.  Apparent deviations are small.  BG 7/25/91.
C     NOTE!! 2.  The upper layer is at lower i than the lower
C       layer.  But because of the sign reversal of the coordinate
C       system, we need to swap signs.

C     Ensure that the forcing starts at zero.
      DO 1000 i = 1, nlayer
        q(i) = 0.0D0
 1000 CONTINUE

C     Ensure that the scalar concentrations start at zero.
C     Note: It might be better to set it to something ridiculous, so as
C       to ferret out improper flows.
      DO 1100 j = 1, nr
        DO 1200 i = 1, nlev
          cna(i,j) = 0.D0
          caa(i,j) = 0.D0
          ciw(i,j) = 0.D0
 1200   CONTINUE
 1100 CONTINUE

C     Now load up the vectors for the transfer of scalars.
      DO 2000 i = 1, nr
        iaa(i) = INT(aadept(i)/deltaz)
        ina(i) = INT(nadept(i)/deltaz)
        iiw(i) = INT(iwdept(i)/deltaz)
        iaas(i) = INT(aasour(i)/deltaz)
        inas(i) = INT(nasour(i)/deltaz)
        iiws(i) = INT(iwsour(i)/deltaz)
 2000 CONTINUE

      DO 2100 i = 0, 4
        cna(ina(1)-i, 1)  = x(ina (2)-i,k,2)
        cna(ina(2)-i, 2)  = x(ina (2)-i,k,2)
        cna(inas(1)-i, 1) = x(inas(1)-i,k,1)
        cna(inas(2)-i, 2) = x(inas(1)-i,k,1)
 2100 CONTINUE
 
      DO 2200 i = 0,1
        caa(iaa(1)-i, 1)  = x(iaa (3)-i,k,3)
        caa(iaa(3)-i, 3)  = x(iaa (3)-i,k,3)
        caa(iaas(1)-i, 1) = x(iaas(1)-i,k,1)
        caa(iaas(3)-i, 3) = x(iaas(1)-i,k,1)
 2200 CONTINUE
 
      DO 2300 i = 0,5
        ciw(iiw(1)-i, 1)  = x(iiw (3)-i,k,3)
        ciw(iiw(3)-i, 3)  = x(iiw (3)-i,k,3)
        ciw(iiws(1)-i, 1) = x(iiws(1)-i,k,1)
        ciw(iiws(3)-i, 3) = x(iiws(1)-i,k,1)
 2300 CONTINUE

C     Now compute the related flux
      DO 3000 i = 1, nlayer-1

        q(i) =
     1        +( rho(2)*cna(i,m)  *fna(i,m)
     2          +rho(3)*caa(i,m)  *faa(i,m)
     5          +rho(3)*ciw(i,m)  *fiw(i,m)
     6          +rho(m)*area(m)*x(i,k,m)*(-w(i+1,m)+w(i,m))
     7         )
     8         /area(m)/rho(m)

 3000 CONTINUE
 
CD      IF (m .EQ. 2) THEN
CD      DO 2500 i = inas(1)-5, inas(1)
CD        WRITE (*,9004) fna(i,m), w(i+1,m)-w(i,m), q(i),
CD     1 x(i,k,1), x(i,k,2)-x(i,k,1),
CD     2 cna(i,m)*fna(i,m)+x(i,k,m)*(w(i,m)-w(i+1,m))*area(m), m
CD 2500 CONTINUE
CD      ENDIF
 9004 FORMAT (6E11.4,I2)

CD      sum = 0.D0
CD      DO 4000 i = 1, nlayer
CD        sum = sum + q(i)
CD 4000 CONTINUE
CD      IF (m .EQ. 1) PRINT *,'bwsource',pchem, sum

 9001 FORMAT (5E10.2, 2E10.3, 3I3)
 9002 FORMAT (3I3, 4E12.4)

      RETURN
      END
