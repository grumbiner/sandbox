C*************************************************----------++++++++++!!
      PROGRAM oeschger
C     Attempt to simulate the results of the Oeschger model
C        as presented in Broecker and Peng, 1982.
C     Generalize to have plumes of  water entering and exiting at 
C       variable depths.  8/4/91. BG
C     Model the Phosphate profile 8/4/91. BG
C     Generalize to have multiple chemicals, started 8/4/91 BG.

      IMPLICIT none
      
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)
      INTEGER nlayer, chemu, region, nchem
      PARAMETER (nlayer = 76)
      PARAMETER (chemu  =  4)
      PARAMETER (region =  3)
      PARAMETER (nchem  = 17)

      DOUBLE PRECISION deltaz, deltat
      INTEGER LONG

C     Chemical Profiles 
      DOUBLE PRECISION x(nlayer, chemu, region)
      DOUBLE PRECISION xnm1(nlayer, chemu, region)
      INTEGER pchem(chemu)

C     Advection/Diffusion
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

        IF (MOD(j-1,INT(secpyr/deltat)) .EQ. 0) THEN
C         Asynchronously Update geologically-changing parameters.
CD          PRINT *,'Calling geolog',j
          CALL geolog(x, pchem, nlayer, chemu, region, 
     1                   csdept, ishelf, rsalt, deltaz, 
     2                   time)
        ENDIF
  
CD        PRINT *,'Calling aabw',j
        CALL aabw(temp, wind, csdept, ishelf, time,
     1            faa, aamix, fna, fiw, forced, deltaz, 
     2            nlayer, region)

CD        PRINT *,'Calling upwell',j
        CALL upwell(fna, faa, fiw, deltaz, nlayer, w, region)

CD        PRINT *,'Calling atmos',j
        CALL atmos(xatm, xanm1, temp, wind, region, 
     1                      time, j, pchem, chemu, deltat, deltaz,
     2                      forced, pref, xnm1, nlayer, rsalt)
     
CD        PRINT *,'Calling ssink',j
        CALL ssink(pchem, chemu, deltaz, deltat, 
     1             xnm1, w, q, fna, faa, fiw, aamix, 
     2             pref, temp, wind, region, rsalt)

        DO 1200 r = 1, region
          DO 1100 k = 1, chemu 
CD            PRINT *,'Calling advdif, r, k', r, k ,j   
            CALL advdif(deltaz, deltat, x(1,k,r), xnm1(1,k,r), 
     1                           kappa(1,r), w(1,r), q(1,k,r), j)
 1100     CONTINUE
 1200   CONTINUE

CD        PRINT *,'Calling oeout',j
        CALL oeout(x, xnm1,  nlayer, chemu, 
     1             j, outoft, secpyr, deltat, deltaz,
     2                 xatm, temp, wind, region, time)

 1000 CONTINUE

CD      PRINT *,'time at end', LONG(362)
      CLOSE(10, STATUS='KEEP')
      CLOSE(11, STATUS='KEEP')
      CLOSE(12, STATUS='KEEP')
      CLOSE(13, STATUS='KEEP')
      CLOSE(20, STATUS='KEEP')
      CLOSE(21, STATUS='KEEP')
      CLOSE(22, STATUS='KEEP')
      CLOSE(23, STATUS='KEEP')
      CLOSE(30, STATUS='KEEP')
      CLOSE(31, STATUS='KEEP')
      CLOSE(32, STATUS='KEEP')
      CLOSE(33, STATUS='KEEP')
      CLOSE(1, STATUS='KEEP')
CD      PAUSE

      END
