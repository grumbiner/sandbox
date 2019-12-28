C*************************************************----------++++++++++!!
      SUBROUTINE aabw(temp, wind, csdept, ishelf, time,
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

      SAVE table, aadept, nadept, iwdept, aasour, iwsour

      INCLUDE "bw.inc"

C     Chronology
      table( 1,ptime) = -19000.0
      table( 2,ptime) = -18000.0
      table( 3,ptime) = -17000.0
      table( 4,ptime) = -16000.0
      table( 5,ptime) = -15000.0
      table( 6,ptime) = -14000.0
      table( 7,ptime) = -13000.0
      table( 8,ptime) = -12000.0
      table( 9,ptime) = -11000.0
      table(10,ptime) = -10000.0
      table(11,ptime) = - 9000.0
      table(12,ptime) = - 8000.0
      table(13,ptime) = - 7000.0
      table(14,ptime) = - 6000.0
      table(15,ptime) = - 5000.0
      table(16,ptime) = - 4000.0
      table(17,ptime) = - 3000.0
      table(18,ptime) = - 2000.0
      table(19,ptime) = - 1000.0
      table(20,ptime) = - 0000.0

C     NADW Flux through time, in m3/s
      table( 1,pfna ) =  3.D6 ! values from 17 to 0 are derived
      table( 2,pfna ) =  3.D6 !   from Keir 1990, combined with
      table( 3,pfna ) = 10.D6 !   Grumbine 1991 in prep expts.
      table( 4,pfna ) = 10.D6
      table( 5,pfna ) = 10.D6
      table( 6,pfna ) = 10.D6
      table( 7,pfna ) = 10.D6
      table( 8,pfna ) = 11.D6
      table( 9,pfna ) = 12.D6
      table(10,pfna ) = 13.D6
      table(11,pfna ) = 14.D6
      table(12,pfna ) = 15.D6
      table(13,pfna ) = 16.D6
      table(14,pfna ) = 17.D6
      table(15,pfna ) = 18.D6
      table(16,pfna ) = 18.D6
      table(17,pfna ) = 18.D6
      table(18,pfna ) = 18.D6
      table(19,pfna ) = 18.D6
      table(20,pfna ) = 18.D6
      
C     AAIW Flux through time, in m3/s
      table( 1,piw  ) = 10.D6
      table( 2,piw  ) = 10.D6
      table( 3,piw  ) = 40.D6
      table( 4,piw  ) = 40.D6
      table( 5,piw  ) = 40.D6
      table( 6,piw  ) = 40.D6
      table( 7,piw  ) = 40.D6
      table( 8,piw  ) = 40.D6
      table( 9,piw  ) = 40.D6
      table(10,piw  ) = 40.D6
      table(11,piw  ) = 40.D6
      table(12,piw  ) = 40.D6
      table(13,piw  ) = 40.D6
      table(14,piw  ) = 40.D6
      table(15,piw  ) = 40.D6
      table(16,piw  ) = 40.D6
      table(17,piw  ) = 25.35D6
      table(18,piw  ) = 25.35D6
      table(19,piw  ) = 25.35D6
      table(20,piw  ) = 25.35D6

C     AABW Flux through time, in m3/s
      table( 1,pfaa ) = 3.D6
      table( 2,pfaa ) = 3.D6
      table( 3,pfaa ) = 20.7D6  !expt
      table( 4,pfaa ) = 21.32D6
      table( 5,pfaa ) = 21.93D6 !expt
      table( 6,pfaa ) = 17.015D6
      table( 7,pfaa ) = 12.1D6  !expt
      table( 8,pfaa ) = 10.82D6
      table( 9,pfaa ) = 9.55D6
      table(10,pfaa ) = 8.27D6  !expt
      table(11,pfaa ) = 7.69D6
      table(12,pfaa ) = 7.11D6
      table(13,pfaa ) = 6.525D6 !expt
      table(14,pfaa ) = 6.54D6
      table(15,pfaa ) = 6.56D6
      table(16,pfaa ) = 6.58D6
      table(17,pfaa ) = 6.60D6
      table(18,pfaa ) = 6.61D6
      table(19,pfaa ) = 6.63D6
      table(20,pfaa ) = 6.65D6  !expt

C     AABW Mixing fraction through time.
      table( 1,pmix ) = 0.D6
      table( 2,pmix ) = 0.D6
      table( 3,pmix ) = 0.056  !expt
      table( 4,pmix ) = 0.065
      table( 5,pmix ) = 0.074  !expt
      table( 6,pmix ) = 0.081
      table( 7,pmix ) = 0.088  !expt
      table( 8,pmix ) = 0.098
      table( 9,pmix ) = 0.108
      table(10,pmix ) = 0.118  !expt
      table(11,pmix ) = 0.127
      table(12,pmix ) = 0.136
      table(13,pmix ) = 0.144  !expt
      table(14,pmix ) = 0.144
      table(15,pmix ) = 0.144
      table(16,pmix ) = 0.145
      table(17,pmix ) = 0.145
      table(18,pmix ) = 0.145
      table(19,pmix ) = 0.146
      table(20,pmix ) = 0.146  !expt

C     Zero the flux fields
      DO 300 m = 1, region
        DO 400 i = 1, nlayer
          fna(i,m) = 0.0
          fiw(i,m) = 0.0
          faa(i,m) = 0.0
  400   CONTINUE
  300 CONTINUE

C     Now derive the forcings:
      iaa = INT(aadept(1)/deltaz)
      ina = INT(nadept(1)/deltaz)
      iiw = INT(iwdept(1)/deltaz)
      iaas = INT(aasour(1)/deltaz)
      inas = INT(nasour(1)/deltaz)
      iiws = INT(iwsour(1)/deltaz)
      IF (forced) THEN
        i = 1
        IF (time .LT. table(1,ptime)) THEN
          PRINT *,'Time is too old.  AABW'
          aabwfl = table(1,pfaa)
          nadwfl = table(1,pfna)
          aaiwfl = table(1,piw)
          aamix         = table(1,pmix)
          faa(iaa, 1)   = aabwfl/2.
          faa(iaa-1, 1) = aabwfl/2.
          fna(ina, 1)   = nadwfl/5.
          fna(ina-1, 1) = nadwfl/5.
          fna(ina-2, 1) = nadwfl/5.
          fna(ina-3, 1) = nadwfl/5.
          fna(ina-4, 1) = nadwfl/5.
          fiw(iiw, 1)   = aaiwfl/6.
          fiw(iiw-1, 1) = aaiwfl/6.
          fiw(iiw-2, 1) = aaiwfl/6.
          fiw(iiw-3, 1) = aaiwfl/6.
          fiw(iiw-4, 1) = aaiwfl/6.
          fiw(iiw-5, 1) = aaiwfl/6.
          faa(iaas, 1)   = -aabwfl/2.
          faa(iaas-1, 1) = -aabwfl/2.
          fna(inas, 1)   = -nadwfl/5.
          fna(inas-1, 1) = -nadwfl/5.
          fna(inas-2, 1) = -nadwfl/5.
          fna(inas-3, 1) = -nadwfl/5.
          fna(inas-4, 1) = -nadwfl/5.
          fiw(iiws, 1)   = -aaiwfl/6.
          fiw(iiws-1, 1) = -aaiwfl/6.
          fiw(iiws-2, 1) = -aaiwfl/6.
          fiw(iiws-3, 1) = -aaiwfl/6.
          fiw(iiws-4, 1) = -aaiwfl/6.
          fiw(iiws-5, 1) = -aaiwfl/6.
          GO TO 1000
        ENDIF
 100    i = i + 1
        IF (time .GT. table(i,ptime) .AND. i .LT. ntime) GO TO 100
C         the tabled time is less than (older) the chosen time.
C       Having reached this point, we've bracketed the time.
C       Compute the fraction of the two times to use for linearly
C         interpolating between table entries.
        itime = i

C       Polar to World Ocean Flux
        fract = (time - table(itime-1,ptime) )/
     1          (table(itime,ptime)-table(itime-1,ptime))
          
        aabwfl = fract *table(itime  ,pfaa) +
     1                   (1.-fract)*table(itime-1,pfaa)
        nadwfl = fract *table(itime  ,pfna) +
     1                   (1.-fract)*table(itime-1,pfna)
CD        PRINT *,'nadw flux',nadwfl
        aaiwfl = fract *table(itime  ,piw ) +
     1                   (1.-fract)*table(itime-1,piw )
        aamix  = fract *table(itime  ,pmix) +
     1                   (1.-fract)*table(itime-1,pmix)

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

CD      DO 4000 i = 1, nlayer
CD        WRITE (*,9001) (fna(i,m), faa(i,m), fiw(i,m), m=1,region)
CD 4000 CONTINUE
CD 9001 FORMAT (9E9.2)

 9999 CONTINUE
      RETURN
      END
