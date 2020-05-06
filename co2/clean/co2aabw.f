C*************************************************----------++++++++++!!
      SUBROUTINE co2aabw(temp, wind, csdept, ishelf, time,
     1                      faa, aamix, fna, fiw,
     2                      forced, deltaz, nlayer, region)
      IMPLICIT none

      INTEGER nlayer, region
      DOUBLE PRECISION temp(region), wind(region)
      REAL csdept, ishelf, time
      DOUBLE PRECISION faa(nlayer, region), aamix
      DOUBLE PRECISION fna(nlayer, region)
      DOUBLE PRECISION fiw(nlayer, region)
      LOGICAL forced
      DOUBLE PRECISION deltaz

C     Start with the Assumption that the entry depth, and source
C       depth do not change in time.
      INTEGER nr
      PARAMETER (nr = 3)
      REAL aadept(nr), nadept(nr), aasour(nr)
      REAL nasour(nr), iwdept(nr), iwsour(nr)

      INTEGER ntime, nfactr
      PARAMETER (ntime = 20)
      PARAMETER (nfactr = 5)
      INTEGER ptime, pfna, pfaa, pmix, piw
      PARAMETER (ptime = 1)
      PARAMETER (pfna  = 2)
      PARAMETER (pfaa  = 3)
      PARAMETER (pmix  = 4)
      PARAMETER (piw   = 5)
      DOUBLE PRECISION table(ntime, nfactr)

      INTEGER i, m, itime
      INTEGER iaa, ina, iiw
      INTEGER iaas, inas, iiws
      REAL fract
      DOUBLE PRECISION aaiwfl, aabwfl, nadwfl

      SAVE aadept, nadept, iwdept, aasour, iwsour

      INCLUDE "bw.inc"

C     Chronology
C     NADW Flux through time, in m3/s
C     AAIW Flux through time, in m3/s
C     AABW Flux through time, in m3/s
C     AABW Mixing fraction through time.
C     Above moved to include for bottom water, and a function to
C       Interpolate.

C     Zero the flux fields
CD      PRINT *,'zeroing the flux fields'
      DO 300 m = 1, region
        DO 400 i = 1, nlayer
          fna(i,m) = 0.0
          fiw(i,m) = 0.0
          faa(i,m) = 0.0
  400   CONTINUE
  300 CONTINUE

C     Now derive the forcings:
CD      PRINT *,'computing the depths'
      iaa = INT(aadept(1)/deltaz)
      ina = INT(nadept(1)/deltaz)
      iiw = INT(iwdept(1)/deltaz)
      iaas = INT(aasour(1)/deltaz)
      inas = INT(nasour(1)/deltaz)
      iiws = INT(iwsour(1)/deltaz)
      i = 0
      IF (forced) THEN
CD        PRINT *,'computing the regular fields'
        itime = i + 1
C       Polar to World Ocean Flux
        aabwfl = table(itime  ,pfaa) 
        nadwfl = table(itime  ,pfna) 
        aaiwfl = table(itime  ,piw ) 
        aamix  = table(itime  ,pmix) 

        faa(iaa  , 1) = aabwfl/2.
        faa(iaa-1, 1) = aabwfl/2.
        fna(ina, 1) = nadwfl/5.
        fna(ina-1, 1) = nadwfl/5.
        fna(ina-2, 1) = nadwfl/5.
        fna(ina-3, 1) = nadwfl/5.
        fna(ina-4, 1) = nadwfl/5.
        fiw(iiw, 1) = aaiwfl/6.
        fiw(iiw-1, 1) = aaiwfl/6.
        fiw(iiw-2, 1) = aaiwfl/6.
        fiw(iiw-3, 1) = aaiwfl/6.
        fiw(iiw-4, 1) = aaiwfl/6.
        fiw(iiw-5, 1) = aaiwfl/6.

C       World ocean to Polar ocean flux
        faa(iaas, 1) = -faa(iaa, 1)
        faa(iaas-1, 1) = -faa(iaa-1, 1)
        fna(inas, 1) = -fna(ina, 1)
        fna(inas-1, 1) = -fna(ina-1, 1)
        fna(inas-2, 1) = -fna(ina-2, 1)
        fna(inas-3, 1) = -fna(ina-3, 1)
        fna(inas-4, 1) = -fna(ina-4, 1)
        fiw(iiws, 1) = -fiw(iiw, 1)
        fiw(iiws-1, 1) = -fiw(iiw-1, 1)
        fiw(iiws-2, 1) = -fiw(iiw-2, 1)
        fiw(iiws-3, 1) = -fiw(iiw-3, 1)
        fiw(iiws-4, 1) = -fiw(iiw-4, 1)
        fiw(iiws-5, 1) = -fiw(iiw-5, 1)

        GO TO 1000

       ELSE
C       Free solution, Faa, fna = fns of T, U, H, IS, ...
        STOP 'Free solution not implemented yet'

      ENDIF

 1000 CONTINUE

C     Now assign the regional fluxes, assumed that 2=NA, 3=SO
C     Warning! BWSRC assumes that fluxes are positive if directed
C       towards the box of interest.
      IF (region .LT. 2) GO TO 9999
      iaa = INT(aadept(2)/deltaz)
      ina = INT(nadept(2)/deltaz)
      iiw = INT(iwdept(2)/deltaz)
      iaas = INT(aasour(2)/deltaz)
      inas = INT(nasour(2)/deltaz)
      iiws = INT(iwsour(2)/deltaz)
      faa(iaa  , 2) = 0.
      faa(iaa-1, 2) = 0.
      fna(ina, 2) = -nadwfl/5.
      fna(ina-1, 2) = -nadwfl/5.
      fna(ina-2, 2) = -nadwfl/5.
      fna(ina-3, 2) = -nadwfl/5.
      fna(ina-4, 2) = -nadwfl/5.
      fiw(iiw, 2) = 0.
      fiw(iiw-1, 2) = 0.
      fiw(iiw-2, 2) = 0.
      fiw(iiw-3, 2) = 0.
      fiw(iiw-4, 2) = 0.
      fiw(iiw-5, 2) = 0.
C     World ocean to Polar ocean flux
      faa(iaas, 2) = 0.0
      faa(iaas-1, 2) = 0.0
      fna(inas, 2) = nadwfl/5.
      fna(inas-1, 2) = nadwfl/5.
      fna(inas-2, 2) = nadwfl/5.
      fna(inas-3, 2) = nadwfl/5.
      fna(inas-4, 2) = nadwfl/5.
      fiw(iiws, 2) = 0.0
      fiw(iiws-1, 2) = 0.0
      fiw(iiws-2, 2) = 0.0
      fiw(iiws-3, 2) = 0.0
      fiw(iiws-4, 2) = 0.0
      fiw(iiws-5, 2) = 0.0

C     Antarctic
      IF (region .LT. 3) GO TO 9999
      iaa = INT(aadept(3)/deltaz)
      ina = INT(nadept(3)/deltaz)
      iiw = INT(iwdept(3)/deltaz)
      iaas = INT(aasour(3)/deltaz)
      inas = INT(nasour(3)/deltaz)
      iiws = INT(iwsour(3)/deltaz)
      faa(iaa  , 3) = -aabwfl/2.
      faa(iaa-1, 3) = -aabwfl/2.
      fna(ina, 3) = 0.
      fna(ina-1, 3) = 0.
      fna(ina-2, 3) = 0.
      fna(ina-3, 3) = 0.
      fna(ina-4, 3) = 0.
      fiw(iiw, 3) = -aaiwfl/6.
      fiw(iiw-1, 3) = -aaiwfl/6.
      fiw(iiw-2, 3) = -aaiwfl/6.
      fiw(iiw-3, 3) = -aaiwfl/6.
      fiw(iiw-4, 3) = -aaiwfl/6.
      fiw(iiw-5, 3) = -aaiwfl/6.
C     World ocean to Polar ocean flux
      faa(iaas, 3) = aabwfl/2.
      faa(iaas-1, 3) = aabwfl/2.
      fna(inas, 3) = 0.0
      fna(inas-1, 3) = 0.0
      fna(inas-2, 3) = 0.0
      fna(inas-3, 3) = 0.0
      fna(inas-4, 3) = 0.0
      fiw(iiws, 3) = aaiwfl/6.
      fiw(iiws-1, 3) = aaiwfl/6.
      fiw(iiws-2, 3) = aaiwfl/6.
      fiw(iiws-3, 3) = aaiwfl/6.
      fiw(iiws-4, 3) = aaiwfl/6.
      fiw(iiws-5, 3) = aaiwfl/6.

 9999 CONTINUE
      RETURN
      END
