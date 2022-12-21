C*************************************************----------++++++++++!!
	SUBROUTINE oestar(x, xnm1, kappa, w, q, faa, fna, fiw, nlayer,
     1                   atemp, wind, csdept, ishelf, stime, forced,
     2                   lenrun, outoft, deltaz, deltat,
     3                   pref, xatm, xanm1, pchem, chemu, region)

	IMPLICIT none

	INTEGER nlayer, chemu, region

	DOUBLE PRECISION x(nlayer, chemu, region)
	DOUBLE PRECISION xnm1(nlayer, chemu, region)
	DOUBLE PRECISION faa(nlayer, region), fna(nlayer, region)
	DOUBLE PRECISION fiw(nlayer, region)
	DOUBLE PRECISION kappa(nlayer, region), w(nlayer, region)
	DOUBLE PRECISION q(nlayer, chemu, region)
	INTEGER lenrun, outoft, peryr, pchem(chemu)
	DOUBLE PRECISION deltaz, deltat
	DOUBLE PRECISION atemp(region), wind(region)
	REAL csdept, ishelf, stime
	DOUBLE PRECISION xatm(chemu), xanm1(chemu)
	LOGICAL forced

	DOUBLE PRECISION wm, kappam, dt1, dt2, aamix
	DOUBLE PRECISION tempor, length
	CHARACTER*60 fname, ques
	INTEGER i, j, l, m, step, iref

	DOUBLE PRECISION secpyr
	PARAMETER (secpyr = 8.64D4*365.2422)

C     Chemistry pointers
	INCLUDE "chmptr.inc"

C     Initial conditions for the assorted tracers:
	DOUBLE PRECISION ic(nchem), pref(nchem)

C                                  Pedigree
	DATA ic(He)   /1.768D-9 / !B+P, 1982 saturation 0 !C
	DATA ic(Ne)   /7.920D-9 / !B+P, 1982 saturation 0 !C
	DATA ic(N2)   /6.248D-4 / !B+P, 1982 saturation 0 !C
	DATA ic(O2)   /2.000D-0 / !Approx Geosecs Global Mean
	DATA ic(Ar)   /1.702D-5 / !B+P, 1982 saturation 0 !C
	DATA ic(Kr)   /4.180D-9 / !B+P, 1982 saturation 0 !C
	DATA ic(Xe)   /7.224D-10/ !B+P, 1982 saturation 0 !C
	DATA ic(CO2)  /17.64D-6 / !B+P, 1982 saturation 0 !C
	DATA ic(N2O)  /1.410D-8 / !B+P, 1982 saturation 0 !C
	DATA ic(temp) /3.62D0   / !Levitus
	DATA ic(salt) /34.73D0  / !Levitus
	DATA ic(NO3)  /33.0D-6  /
	DATA ic(PO4)  /2.116 D-6 /!BG analysis of GEOSECS data, SCOPE16
	DATA ic(SiO2) /150.D-6  /
	DATA ic(dc14) /-160.D0  /
	DATA ic(tco2) /2300.D-6 / !Approx Geosecs global mean
	DATA ic(alk ) /2400.D-6 / !Approx Geosecs global mean

C     Equilibrium atmospheric mixing ratios.  (Not pressures)
C       Note that pref(CO2) is fixed at the pre-industrial value.
	pref(He)  = 5.2E-6
	pref(Ne)  = 1.8E-5
	pref(N2)  = 0.781
	pref(O2)  = 0.209
	pref(Ar)  = 9.3E-3
	pref(Kr)  = 1.1E-6
	pref(Xe)  = 8.6E-8
	PRINT *,'What is the initial atmospheric CO2?'
	READ (*,9004) pref(CO2)
CD      pref(CO2) = 254.E-6
	pref(N2O) = 3.0E-7
	pref(temp) = 3.5
	pref(salt) = 34.6
	pref(NO3)  = 0.0
	pref(PO4)  = 0.0
	pref(SiO2) = 0.0
	pref(dc14) = -100.0D-6
	pref(tco2) = pref(CO2)
	pref(alk ) = 0.0

C     Reading in 'chemical' variable
	DO 10 i = 1, chemu
	  PRINT *, 'Enter tracer number'
	  READ (*,9003) pchem(i)
   10 CONTINUE

C     Initialize the tracer concentrations.
C       Concentrations are in Moles per cubic kg.
C     Attempting to read in files
	PRINT *, 'Are you reading from a file (y or n)'
	READ (*,9001) ques

	IF (ques .EQ. 'n') THEN
	  DO 120 m = 1, region
	    DO 110 j = 1, chemu
		DO 100 i = 1, nlayer
		  x(i, j, m)    = ic(pchem(j))
		  xnm1(i, j, m) = ic(pchem(j))
  100       CONTINUE
  110     CONTINUE
  120   CONTINUE

	 ELSE
	  PRINT *, 'What file do you want to read from?'
	  READ (*,9001) fname
	  PRINT *, 'What step do you want to start at?'
	  READ (*,9003) step
	  OPEN (UNIT=30, FILE=fname, FORM='FORMATTED', STATUS='OLD')
	  STOP 'Not yet implemented for multiple chemicals.'
C       Reading in from file here
	  DO 200 i = 1, step
	    DO 220 j = 1, region
		DO 210 m = 1, nlayer
		  READ (12, 9010) l,x(m,1, j)
		  xnm1(m,1, j) = x(m,1, j)
  210       CONTINUE
  220     CONTINUE
  200   CONTINUE

	ENDIF

C     Initialize the forcing and bw formation vectors.
	DO 422 m = 1, region
	  DO 410 j = 1, chemu
	    DO 411 i = 1, nlayer
		q(i,j,m) = 0.D0
  411     CONTINUE
  410   CONTINUE
	  DO 420 i = 1, nlayer
	    faa(i,m) = 0.D0
	    fna(i,m) = 0.D0
	    fiw(i,m) = 0.D0
  420   CONTINUE
  422 CONTINUE

C     Initialize the Atmospheric and geographic parameters.
	DO 430 i = 1, region
	  atemp(i)  = 20.0
	  wind(i)   =  6.0
  430 CONTINUE
	DO 440 i = 1, chemu
	  xatm(i) = pref(pchem(i))
	  xanm1(i) = xatm(i)
  440 CONTINUE

	csdept = 500.0
	ishelf = 340.E3
	PRINT *,'What start time would you like?'
	READ (*,9004) stime
CD      stime  = -7000.0
	forced = .TRUE.

C     Open the output files.
	PRINT *,'What is the output file base name?'
	READ (*,9001) fname
	iref = INDEX(fname, ' ')
	DO 455 m = 1, region
	  DO 450 i = 1, chemu
	    fname(iref  :iref  ) = CHAR(64+MOD(m,26))
	    fname(iref+1:iref+1) = CHAR(64+MOD(i,26))
	    OPEN (9*m+i, FILE=fname, FORM='FORMATTED', STATUS='NEW')
  450   CONTINUE
  455 CONTINUE
	fname(iref : iref+2) = 'air'
	OPEN (1, FILE=fname, FORM='FORMATTED', STATUS='NEW')

C     Determine the length of the run:
	PRINT *,'What is deltaz?'
	READ (*,9004) deltaz
	PRINT *,'How long a run (approx 50 years per minute)'
	READ (*,9003) lenrun
	PRINT *,'How often (years) do you want the output?'
	READ (*,9003) outoft
	PRINT *,'How many steps per year do you want (integer)?'
	READ (*,9003) peryr

	deltat = secpyr/DBLE(peryr)
CD      PRINT *,'deltat, lenrun, outoft', deltat, lenrun, outoft

C     Initialize the vertical upwelling (meters per second)
	CALL aabw(atemp, wind, csdept, ishelf, stime,
     1          faa, aamix, fna, fiw, forced, deltaz,
     2          nlayer, region)
	CALL upwell(fna, faa, fiw, deltaz, nlayer, w, region)

C     Initialize the diffusivity (meters squared per second)
C     Reading in input here
	PRINT *,'What is the mixing length?'
	READ (*,9004) length
CD      WRITE (*,9004) length
CD      length = 2.E3
	PRINT *,'What is the reference diffusivity?'
	READ (*,9004) tempor
CD      WRITE (*,9004) tempor
CD      tempor = 1.6E-4
	DO 300 i = 1, nlayer
	  IF (ABS(w(i, 1)) .LT. 1.D-8) THEN
	    kappa(i,1) = tempor
	   ELSE
	    kappa(i,1) = length*ABS(w(i,1))
	  ENDIF
	  DO 310 m = 2, region
	    kappa(i,m) = kappa(i,1)
  310   CONTINUE
CD        PRINT *,'diffusion', i, kappa(i,1),kappa(i,2),kappa(i,3)
  300 CONTINUE

C     Check deltat for numerical stability.
	wm = 1.D-6
	kappam = 0.D0
	DO 510 m = 1, region
	  DO 500 i = 1, nlayer
	    wm     = DMAX1(wm, DABS(w(i, m)))
	    kappam = DMAX1(kappam, DABS(kappa(i, m)))
  500   CONTINUE
  510 CONTINUE
	dt1 = deltaz/wm
	dt2 = deltaz*deltaz/4.D0/kappam
	IF (deltat .GT. dt1 .OR. deltat .GT. dt2) THEN
	  PRINT *,'Deltat chosen is numerically unstable'
	  IF (dt1 .LT. dt2) THEN
	    PRINT *,'The constraint is imposed by the CFL condition',dt1
	   ELSE
	    PRINT *,'The constraint is imposed by the diffusion',dt2
	  ENDIF
	  deltat = secpyr/INT(1+secpyr/DMAX1(dt1,dt2))
	  PRINT *,'Number of steps per year is now',
     1           INT(1+secpyr/DMIN1(dt1,dt2))
	ENDIF

CD      PRINT *,'leaving oestart'

 9001 FORMAT (A60)

 9003 FORMAT (BN, I5)

 9004 FORMAT (E13.6)

 9010 FORMAT (I2,6X,D13.6)

	RETURN
	END
