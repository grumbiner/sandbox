
C*************************************************----------++++++++++!!
      SUBROUTINE biosrc(pchem, x, q, nlayer, deltaz, deltat, produc)
C     Present version assumes that phosphate is only chemical.
C     BG 7/25/91.
      IMPLICIT none

      INTEGER pchem, nlayer
      DOUBLE PRECISION x(nlayer), q(nlayer)
      DOUBLE PRECISION deltaz, deltat, produc

      DOUBLE PRECISION decay
      PARAMETER (decay = 1.D0/577.D0)

      DOUBLE PRECISION comb, sum
      INTEGER i

C     Chemical pointers
      INCLUDE "chmptr.inc"

C     Redfield ratios              !Pedigree
      REAL redfld(nchem)
      DATA redfld(He)   /0.0/
      DATA redfld(Ne)   /0.0/
      DATA redfld(N2)   /0.0/
      DATA redfld(O2)   /-172.0/ !Takahashi et al., 1985 obs
      DATA redfld(Ar)   /0.0/
      DATA redfld(Kr)   /0.0/
      DATA redfld(Xe)   /0.0/
      DATA redfld(CO2)  /122.0/
      DATA redfld(N2O)  /0.0/
      DATA redfld(temp) /0.0/
      DATA redfld(salt) /0.0/
      DATA redfld(NO3)  / 16.0/  !Takahashi et al., 1985 obs
      DATA redfld(PO4)  /1.0/
      DATA redfld(SiO2) /0.0/
      DATA redfld(dc14) /0.0/
CD      DATA redfld(tco2) /122.0/  !Takahashi et al., 1985 obs
      DATA redfld(tco2) /134./  !Takahashi et al., 1985 obs
      DATA redfld(alk)  / 24.0/  !Takahashi et al., 1985 obs

C     Particle rain relation for chemicals.  Assumes assumes many
C       many other points, including total phosphate utilization in
C       the surface layer.
C     Now have the source function based on the upper level
C       productivity  (phosphate flux into the upper level, with
C       immediate rain out)

      comb = produc*redfld(pchem)*decay*deltaz/
     1      (1.-EXP(-decay*deltaz*(DBLE(nlayer-1)-2.5)) )
     2     *EXP(decay*deltaz*2.5D0)
      DO 1000 i = 3, nlayer
        q(i) = EXP(-decay*deltaz*DBLE(i))*comb
 1000 CONTINUE
      q(1) = 0.D0
      q(2) = -produc*redfld(pchem)

      sum = 0.D0
      DO 2000 i = 1, nlayer
        sum = sum+q(i)
 2000 CONTINUE
      sum = -sum/(nlayer-2)
      DO 2010 i = 3, nlayer
CD        IF (sum .GT. 1.E-3*q(i)) PRINT *,sum, q(i), i
        q(i)  = q(i) + sum
 2010 CONTINUE

CD      PRINT *,'biosum',pchem,sum

      RETURN
      END