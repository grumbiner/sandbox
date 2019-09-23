      SUBROUTINE matscor(matunit, scorunit)
C     Score forecasts against observations, working from a pre-created
C     match-up file.
C     Bob Grumbine 10 April 1995

      IMPLICIT none
      INTEGER matunit, scorunit

      INTEGER nlim
      PARAMETER (nlim = 90000)

C     Variables read in to monster array
      CHARACTER*7 name(nlim)
      INTEGER skref(nlim), date(nlim), fday(nlim)
      REAL lat(nlim), lon(nlim), odist(nlim), odir(nlim)
      REAL dist1(nlim), dist2(nlim), dir1(nlim), dir2(nlim)

C     Local selection scoring variables
      REAL sdist(nlim), sdir(nlim)
      REAL f1dist(nlim), f2dist(nlim), f1dir(nlim), f2dir(nlim) 

C     Local variables
      INTEGER i, j, nday, npts, tdate
      INTEGER yy, mm, sk
      REAL ia, r2, vcor
      REAL obar, err1bar, err2bar, delbar, delsd, tstat

 9001 FORMAT (A7, I4, I7, I3, F7.2, F8.2, 3x, 3(F6.1, F7.2) )
      i = 0
 1000 CONTINUE
        i = i + 1
        READ (matunit, 9001, END=2000, ERR=2000) 
     1    name(i), skref(i), date(i), fday(i), 
     2    lat(i), lon(i), odist(i), odir(i), dist1(i), dir1(i),
     3    dist2(i), dir2(i)
        GO TO 1000

 2000 CONTINUE

      npts = i-1

C     Now begin to make scorings.
 9002 FORMAT ('Global scoring of skiles ', I1, I6, 7F7.3)
      CALL drift3(odist, odir, dist1, dir1, dist2, dir2, npts,
     1              obar, delbar, delsd, err1bar, err2bar, tstat)
      WRITE (scorunit, 9002) 0, npts, obar, delbar, delsd, 
     1              err1bar, err2bar, tstat
      PRINT *,'dist score ',npts, obar, delbar, delsd, 
     1              err1bar, err2bar, tstat
      CALL ssanaly(odist, odir, dist1, dir1, npts, ia, r2, vcor)
      WRITE (scorunit, 9002) 1, npts, ia, r2, vcor
      CALL ssanaly(odist, odir, dist2, dir2, npts, ia, r2, vcor)
      WRITE (scorunit, 9002) 2, npts, ia, r2, vcor
      

C     Summarize by fday
      DO 3000 nday = 1, 6
        i = 0
        DO 3100 j = 1, npts
          IF (fday(j) .EQ. nday) THEN
            i = i + 1
            sdist(i)  = odist(j)
            sdir(i)   = odir(j)
            f1dist(i) = dist1(j)
            f2dist(i) = dist2(j)
            f1dir(i)  = dir1(j)
            f2dir(i)  = dir2(j)
          ENDIF
 3100   CONTINUE
 9004   FORMAT ('model ',I1,' forecast day ',I3, I6, 7F7.3)
        CALL drift3(sdist, sdir, f1dist, f1dir, f2dist, f2dir, i,
     1              obar, delbar, delsd, err1bar, err2bar, tstat)
        WRITE (scorunit, 9004) 0, nday, i, obar, delbar, delsd, 
     1              err1bar, err2bar, tstat
        CALL ssanaly(sdist, sdir, f1dist, f1dir, i, ia, r2, vcor)
        WRITE (scorunit, 9004) 1, nday, i, ia, r2, vcor
        CALL ssanaly(sdist, sdir, f2dist, f2dir, i, ia, r2, vcor)
        WRITE (scorunit, 9004) 2, nday, i, ia, r2, vcor
 3000 CONTINUE

C     Summarize by month and fday
      DO 4000 yy = 93, 95
      DO 4000 mm = 1, 12
        tdate = 100*(mm + 100*yy)
        WRITE (scorunit, 9005) tdate
 9005   FORMAT (//,'Forecast month and day separation ', I6)
        DO 4100 nday = 1, 6
          i = 0
          DO 4200 j = 1, npts
            IF (tdate/100 .EQ. date(j)/100 .AND. nday .EQ. fday(j)) THEN
              i = i + 1
              sdist(i)  = odist(j)
              sdir(i)   = odir(j)
              f1dist(i) = dist1(j)
              f2dist(i) = dist2(j)
              f1dir(i)  = dir1(j)
              f2dir(i)  = dir2(j)
            ENDIF
 4200     CONTINUE

          IF (i .LT. 10) GO TO 4100
          CALL drift3(sdist, sdir, f1dist, f1dir, f2dist, f2dir, i,
     1                  obar, delbar, delsd, err1bar, err2bar, tstat)
          WRITE (scorunit, 9006) 0, nday, i, obar, delbar, delsd, 
     1              err1bar, err2bar, tstat
          CALL ssanaly(sdist, sdir, f1dist, f1dir, i, ia, r2, vcor)
          WRITE (scorunit, 9006) 1, nday, i, ia, r2, vcor
          CALL ssanaly(sdist, sdir, f2dist, f2dir, i, ia, r2, vcor)
          WRITE (scorunit, 9006) 2, nday, i, ia, r2, vcor
 9006     FORMAT ('model ',I1,' forecast day ',I3, ' pts = ',I6,
     1     7F7.3)
 4100   CONTINUE

 4000 CONTINUE


C     Summarize by skiles points, ignore fday for the moment
      DO 5000 sk = 1, 207
        WRITE (scorunit, 9007) sk
 9007   FORMAT ('point separation ', I6)
        i = 0
        DO 5100 j = 1, npts
          IF (skref(j) .EQ. sk ) THEN
              i = i + 1
              sdist(i)  = odist(j)
              sdir(i)   = odir(j)
              f1dist(i) = dist1(j)
              f2dist(i) = dist2(j)
              f1dir(i)  = dir1(j)
              f2dir(i)  = dir2(j)
          ENDIF
 5100   CONTINUE 
        IF (i .LT. 10) GO TO 5000
        CALL drift3(sdist, sdir, f1dist, f1dir, f2dist, f2dir, i,
     1                obar, delbar, delsd, err1bar, err2bar, tstat)
        WRITE (scorunit, 9008) 0, sk, i, obar, delbar, delsd, 
     1              err1bar, err2bar, tstat
        CALL ssanaly(sdist, sdir, f1dist, f1dir, i, ia, r2, vcor)
        WRITE (scorunit, 9008) 1, sk, i, ia, r2, vcor
        CALL ssanaly(sdist, sdir, f2dist, f2dir, i, ia, r2, vcor)
        WRITE (scorunit, 9008) 2, sk, i, ia, r2, vcor
 9008   FORMAT ('Model, point, npts ', I1, I4, I8, 7F7.3)
 5000 CONTINUE

C     Stratify by distance (obsd) drifted:
      DO 6000 sk = 1, 20
        WRITE (scorunit, 9007) sk*10
 9009   FORMAT ('Max drift distance ', I6)
        i = 0
        DO 6100 j = 1, npts
          IF (odist(j) .LE. sk*10 .AND. odist(j) .GT. 10*(sk-1) ) THEN
              i = i + 1
              sdist(i)  = odist(j)
              sdir(i)   = odir(j)
              f1dist(i) = dist1(j)
              f2dist(i) = dist2(j)
              f1dir(i)  = dir1(j)
              f2dir(i)  = dir2(j)
          ENDIF
 6100   CONTINUE 
        IF (i .LT. 10) GO TO 6000
        CALL drift3(sdist, sdir, f1dist, f1dir, f2dist, f2dir, i,
     1                obar, delbar, delsd, err1bar, err2bar, tstat)
        WRITE (scorunit, 9010) 0, sk*10, i, obar, delbar, delsd, 
     1              err1bar, err2bar, tstat
        CALL ssanaly(sdist, sdir, f1dist, f1dir, i, ia, r2, vcor)
        WRITE (scorunit, 9010) 1, sk*10, i, ia, r2, vcor
        CALL ssanaly(sdist, sdir, f2dist, f2dir, i, ia, r2, vcor)
        WRITE (scorunit, 9010) 2, sk*10, i, ia, r2, vcor
 9010   FORMAT ('Model, dist, npts ', I1, I4, I8, 7F7.3)
 6000 CONTINUE

      RETURN
      END
