C*************************************************----------++++++++++!!
      SUBROUTINE atmos(x, xnm1, mtemp, mwind, region, 
     1                      mtime, step, pchem, chemu, deltat, deltaz,
     2                      forced, pref, xocean, nlayer, rsalt)
C     Compute the evolution of the atmosphere.  BG 8/5/91.

      IMPLICIT none

      REAL revell
      COMMON /carbon/ revell
C     Declare Arguments      
      INTEGER region, chemu, step, nlayer
      DOUBLE PRECISION x(chemu), xnm1(chemu)
      DOUBLE PRECISION mtemp(region), mwind(region)
      DOUBLE PRECISION deltat, deltaz
      REAL mtime, rsalt
      INTEGER pchem(chemu)
      LOGICAL forced
      DOUBLE PRECISION xocean(nlayer, chemu, region)
      
C     Declare local data
      INTEGER nregn
      PARAMETER (nregn = 3)
      DOUBLE PRECISION area(nregn), rho(nregn)
      DOUBLE PRECISION matm, molair
      PARAMETER (matm   = 5.3E18)
      PARAMETER (molair = 29.87E-3)
      
      INTEGER ntimes
      PARAMETER (ntimes = 9)
      DOUBLE PRECISION atemp(ntimes, nregn), wind(ntimes, nregn)
      DOUBLE PRECISION time(ntimes)
      
      INCLUDE "chmptr.inc"
  
C     Declare locally-used variables
      INTEGER i, j, itime, n
      REAL qatm(nchem), fract
      DOUBLE PRECISION gasses, tempor(nchem)
      DOUBLE PRECISION pref(nchem)
      DOUBLE PRECISION temp1, talk, cco2
  
C     Define Molecular weights kg/mole:
      DOUBLE PRECISION molkg(nchem)

C     Save the data variables for the next call.
      SAVE atemp, wind, time, area, rho

C     Define Molecular weights kg/mole:      
      DATA molkg(He  ) /  4.E-3/
      DATA molkg(Ne  ) / 20.E-3/
      DATA molkg(N2  ) / 28.E-3/
      DATA molkg(O2  ) / 32.E-3/
      DATA molkg(Ar  ) / 40.E-3/
      DATA molkg(Kr  ) / 84.E-3/
      DATA molkg(Xe  ) /131.E-3/
      DATA molkg(CO2 ) / 44.E-3/
      DATA molkg(N2O ) / 44.E-3/
      DATA molkg(temp) /  1.E-3/
      DATA molkg(salt) /  1.E-3/
      DATA molkg(NO3 ) / 60.E-3/
      DATA molkg(PO4 ) / 97.E-3/
      DATA molkg(SiO2) / 64.E-3/
      DATA molkg(dc14) / 14.E-3/
      DATA molkg(tco2) / 44.E-3/
      DATA molkg(alk ) /  1.E-3/

C     Define time histories
      DATA atemp(1,1) /18.4/
      DATA atemp(2,1) /19.9/
      DATA atemp(3,1) /20.4/
      DATA atemp(4,1) /21.4/
      DATA atemp(5,1) /21.4/
      DATA atemp(6,1) /21.4/
      DATA atemp(7,1) /21.4/
      DATA atemp(8,1) /21.4/
      DATA atemp(9,1) /21.4/
      DATA atemp(1,2) / 0.5/
      DATA atemp(2,2) / 0.8/
      DATA atemp(3,2) / 1.3/
      DATA atemp(4,2) / 2.3/
      DATA atemp(5,2) / 2.3/
      DATA atemp(6,2) / 2.3/
      DATA atemp(7,2) / 2.3/
      DATA atemp(8,2) / 2.3/
      DATA atemp(9,2) / 2.3/
      DATA atemp(1,3) / 0.5/
      DATA atemp(2,3) / 0.5/
      DATA atemp(3,3) / 0.5/
      DATA atemp(4,3) / 1.5/
      DATA atemp(5,3) / 1.5/
      DATA atemp(6,3) / 1.5/
      DATA atemp(7,3) / 1.5/
      DATA atemp(8,3) / 1.5/
      DATA atemp(9,3) / 1.5/
      
      DATA wind(1,1) /6.0/
      DATA wind(2,1) /6.0/
      DATA wind(3,1) /6.0/
      DATA wind(4,1) /6.0/
      DATA wind(5,1) /6.0/
      DATA wind(6,1) /6.0/
      DATA wind(7,1) /6.0/
      DATA wind(8,1) /6.0/
      DATA wind(9,1) /6.0/
      DATA wind(1,2) /10.0/
      DATA wind(2,2) /10.0/
      DATA wind(3,2) /6.0/
      DATA wind(4,2) /6.0/
      DATA wind(5,2) /6.0/
      DATA wind(6,2) /6.0/
      DATA wind(7,2) /6.0/
      DATA wind(8,2) /6.0/
      DATA wind(9,2) /6.0/
      DATA wind(1,3) /10.0/
      DATA wind(2,3) /10.0/
      DATA wind(3,3) /6.0/
      DATA wind(4,3) /6.0/
      DATA wind(5,3) /6.0/
      DATA wind(6,3) /6.0/
      DATA wind(7,3) /6.0/
      DATA wind(8,3) /6.0/
      DATA wind(9,3) /6.0/

C     These data are from the time scales in Barnola et al., 1987
CD      DATA time(1) /-20090./  
CD      DATA time(2) /-16250./
CD      DATA time(3) /-12930./
CD      DATA time(4) /- 9140./
CD      DATA time(5) /- 7500./
CD      DATA time(6) /- 6800./
CD      DATA time(7) /- 3530./
CD      DATA time(8) /- 1700./
CD      DATA time(9) /- 0000./
      DATA time(1) /-20000./
      DATA time(2) /-16000./
      DATA time(3) /-14000./
      DATA time(4) /-10000./
      DATA time(5) /- 8000./
      DATA time(6) /- 6000./
      DATA time(7) /- 4000./
      DATA time(8) /- 2000./
      DATA time(9) /     0./
      
      INCLUDE "arrho.inc"
      
C     Now get down to work

C     Compute the temperature and wind speed for each region 
C       at the given time.
C     NOTE The following code is in common with AABW, probable sbr/function.
CD      PRINT *,'Entered atmos'
      IF (forced) THEN
        i = 1
        IF (mtime .LT. time(i)) THEN
          PRINT *,'Time is too old.  Atmos'
          DO 110 j = 1, region
            mtemp(j) = atemp(1,j)
            mwind(j) =  wind(1,j)
  110     CONTINUE
          GO TO 1000
        ENDIF
  100   i = i + 1
        IF (mtime .GT. time(i) .AND. i .LT. ntimes) GO TO 100
C         The tabled time is less than (older) the chosen time.
C         Having reached this point, we've bracketed the time.
C         Compute the fraction of the two times to use for linearly
C         interpolating between table entries.
        itime = i

C       Fraction of the time from time(i-1) to time(i)
        fract = (mtime - time(itime-1) )/
     1          (time(itime)-time(itime-1))
     
        DO 200 j = 1, region
          mtemp(j) = fract *atemp(itime,j) + 
     1                   (1.-fract)*atemp(itime-1,j)
          mwind(j) = fract * wind(itime,j) + 
     1                   (1.-fract)* wind(itime-1,j)
  200   CONTINUE
       ELSE
        STOP 'Free solution not implemented yet'
      ENDIF
      
 1000 CONTINUE

CD      PRINT *,'temperature and wind speed:',mtemp(1), mwind(1)
     
C     Compute the evolution of the mixing ratios
      DO 2000 i = 1, chemu
        qatm(i) = 0.0
        DO 2100 j = 1, region
        temp1 = xocean(2, i, j)
        IF (pchem(i) .EQ. tco2) THEN
          talk = 2300.D-6   !Geosecs Surface value
          DO 2220 n = 1, chemu
            IF (pchem(n) .EQ. alk) THEN
              talk = xocean(2,n,j)
            ENDIF
 2220     CONTINUE
          temp1 = cco2(xocean(2,i,j), talk, mtemp(j), rsalt, 0.D0)
        ENDIF
        IF (pchem(i) .NE. CO2 .AND. pchem(i) .NE. tco2) THEN
          qatm(i) = qatm(i) - molkg(pchem(i))/matm/deltaz
     1           *rho(j)*area(j)*
     2            gasses(pchem(i), temp1, 
     3                    xnm1(i), mtemp(j), rsalt, mwind(j) )
         ELSE
          qatm(i) = qatm(i) - molkg(pchem(i))/matm/deltaz
     1           *rho(j)*area(j)*revell
CD     1           *rho(j)*area(j)
     2           *gasses(pchem(i), temp1, 
     3                    xnm1(i), mtemp(j), rsalt, mwind(j) )
        ENDIF
 2100   CONTINUE
 2000 CONTINUE
 
C     Now carry out the extrapolation:
      DO 3000 i = 1, chemu
        tempor(i) = x(i)
 3000 CONTINUE
 
      IF (step .EQ. 1) THEN
        DO 3100 i = 1, chemu
          x(i) = xnm1(i) + qatm(i)*deltat
 3100   CONTINUE
       ELSE
        DO 3200 i = 1, chemu
          x(i) = xnm1(i) + qatm(i)*deltat*2.
 3200   CONTINUE
      ENDIF

      DO 3300 i = 1, chemu
        xnm1(i) = tempor(i)
 3300 CONTINUE

CD      DO 3400 i = 1, chemu
CD      PRINT *,'x, xnm1, qatm, deltat', x(i), xnm1(i), qatm(i), deltat
CD 3400 CONTINUE

C     Now Have the evaluated mixing ratio passed along to the 
C       general description of the atmosphere.  Assume that the
C       changes implied do not affect bulk composition.

      DO 4000 i = 1, chemu
        pref(pchem(i)) = x(i)
 4000 CONTINUE
 
      RETURN
      END
