C*************************************************----------++++++++++!!
	PROGRAM oned

	IMPLICIT none

	DOUBLE PRECISION secpyr
	PARAMETER (secpyr = 8.64D4*365.2422)
	INTEGER nlayer, chemu, region, nchem, pchem
	PARAMETER (nlayer = 76)
        PARAMETER (region =  3)
        PARAMETER (chemu  =  4)
        PARAMETER (nchem  = 17)
        PARAMETER (pchem  =  5)

	DOUBLE PRECISION deltaz, deltat
	INTEGER LONG

C     Chemical Profiles

C     Advection/Diffusion
        DOUBLE PRECISION x(nlayer, chemu, region)
        DOUBLE PRECISION xnm1(nlayer, chemu, region)
	DOUBLE PRECISION w(nlayer, region), kappa(nlayer, region)
	DOUBLE PRECISION q(nlayer, chemu, region)

C     BW formation
	DOUBLE PRECISION faa(nlayer, region), fna(nlayer, region)
	DOUBLE PRECISION fiw(nlayer, region)
	REAL ishelf, csdept, rsalt
	DOUBLE PRECISION aamix

C     Atmosphere
	DOUBLE PRECISION xatm(chemu), xanm1(chemu), pref(nchem)
	DOUBLE PRECISION temp(region), wind(region)

C     Run parameters
	INTEGER lenrun, outoft
	INTEGER i, j, k, r
	REAL  time, stime
	LOGICAL forced

C     Set up parameters and files
CD      PRINT *,'Calling oestart'
	CALL oestar(x, xnm1, kappa, w, q, faa, fna, fiw, nlayer,
     1                   temp, wind, csdept, ishelf, stime, forced,
     2                   lenrun, outoft, deltaz, deltat,
     3                   pref, xatm, xanm1, pchem, chemu, region)

C     Now ready to compute the evolution of the chemical
C       distributions.
CD      PRINT *,'time at start', LONG(362)

	DO 1000 j = 1, lenrun*INT(secpyr/deltat)+1

	  time = stime + FLOAT(j-1)*SNGL(deltat/secpyr)

	  DO 1200 r = 1, region
	    DO 1100 k = 1, chemu
		CALL advdif(deltaz, deltat, x(1,k,r), xnm1(1,k,r),
     1                           kappa(1,r), w(1,r), q(1,k,r), j)
 1100     CONTINUE
 1200   CONTINUE

CD        PRINT *,'Calling oeout',j
	  CALL oeout(x, xnm1,  nlayer, chemu,
     1             j, outoft, secpyr, deltat, deltaz,
     2                 xatm, temp, wind, region, time)

 1000 CONTINUE

	END
