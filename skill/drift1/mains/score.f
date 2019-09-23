      PROGRAM score
C     Score the forecasts versus the observed buoy drifts.
C     Use all valid time observations equally.  (May bias
C       as some buoys report more often.)
      IMPLICIT none

      INTEGER nmax
      PARAMETER (nmax = 1000)
      CHARACTER*7 id(nmax)
      INTEGER date(nmax)
      REAL blong(nmax), blat(nmax), dlong(nmax), dlat(nmax)
      REAL dist(nmax), dir(nmax)
      REAL long(nmax), lat(nmax)
      INTEGER npt(nmax)
      REAL r2, flong(nmax), flat(nmax), fdist(nmax)
      REAL arcdis


      INTEGER i, k, ntot
      CHARACTER*60 fname


C     Read in the initial locations.
      PRINT *,'What is the name of the verified file?'
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      READ (10,*) ntot
      PRINT *,'There were ',ntot,' buoy-point pairs'
      DO 1000 i = 1, ntot
        READ (10,9002) npt(i), lat(i), long(i), 
     1                 id(i), date(i), blat(i), blong(i)
 1000 CONTINUE
 9002 FORMAT (I3, 2F7.1, A7, I10, 2F7.1)
      

C     Have original positions, now look at forecasts:
      DO 2000 k = 1, 6
        DO 2100 i = 1, ntot
          READ (10,9003) npt, dir(i), dist(i), 
     1                   blat(i), blong(i), flat(i), flong(i)
          fdist(i) = arcdis(blat(i), blong(i), flat(i), flong(i))
 2100   CONTINUE
        CALL correl(dist, fdist, ntot, r2)
        PRINT *,'Day ',k,' score ',r2
 2000 CONTINUE

 9003 FORMAT (I3, 14x, 2F5.1, 7X, 4F7.3)

      STOP
      END
