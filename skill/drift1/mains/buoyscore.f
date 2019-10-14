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
      REAL dx(nmax), dy(nmax), fx(nmax), fy(nmax)
      REAL theta
      REAL arcdis, xbar, ybar, sigx, sigy, rbar, ia, v2, dtrad

      INTEGER m, i, j, k, ntot, ldum
      CHARACTER*60 fname, dummy

C     Read in the initial locations.
      PRINT *,'What is the name of the verified file?'
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      READ (10, 9001) dummy
      READ (10, 9001) dummy
      READ (10, 9001) dummy
      READ (10, 9001) dummy
      READ (10, 9001) dummy

      READ (10,9009) ntot
 9009 FORMAT (11x,I6)
      PRINT *,'There were ',ntot,' buoy-point pairs'
      DO 1000 i = 1, ntot
        READ (10,9002) npt(i), lat(i), long(i), 
     1                 id(i), date(i), blat(i), blong(i)
 1000 CONTINUE
 9002 FORMAT (I3, 2F9.3, A7, I10, 2F9.3)
      

C     Have original positions, now look at forecasts:
      READ (10, 9001) dummy
      DO 2000 k = 1, 6
        READ (10, 9001) dummy
        j = 1
        DO 2100 i = 1, ntot
          READ (10,9003) npt(j), dir(j), dist(j), 
     1                   flat(j), flong(j)
          IF (npt(j) .GT. 207) GO TO 2100
          fdist(j) = arcdis(blong(i), blat(i), flong(j), flat(j) )
          fx(j)    = arcdis(blong(i), blat(i), flong(j), blat(j) )
          fy(j)    = arcdis(blong(i), blat(i), blong(j), flat(j) )
          IF (flat(j) .NE. 0. .AND. fdist(j)/k .LT. 20.) j = j + 1
 2100   CONTINUE
        j = j - 1
        IF (j .GT. 1) THEN 
          CALL correl(dist, fdist, j, r2, xbar, ybar, sigx, sigy)
          CALL agree(dist, rbar, fdist, j, ia)
          dtrad = ATAN(1.)*4./180.
          DO 2010 m = 1, j
            theta = - (dir(m) + 90.)*dtrad
            dx(m) = dist(m)*COS(theta)
            dy(m) = dist(m)*SIN(theta)
 2010     CONTINUE
          CALL VCC(dx, dy, fx, fy, j, v2)

          WRITE (*,9004) k, r2, ia, v2, j, xbar, ybar, sigx, sigy
        ENDIF
 2000 CONTINUE

 9003 FORMAT (I3, 18x, 2F9.3, 7X, 2F9.3)

 9004 FORMAT ('Day ',I2,' Correl ',F6.3,' IA ',F6.3,' VC ',F6.3,
     1          ' Nverf ',
     1           I3,2X,4F9.3)

 9005 FORMAT (2F8.3)


      STOP
      END
