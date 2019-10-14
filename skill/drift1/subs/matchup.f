      SUBROUTINE matchup(sk1tmp, sk2tmp, trimfile, matchfile, 
     1   skpoints)

C     Match points from the buoy file (trimfile) to the skiles points
C      __ write out with the related forecast.
C     Bob Grumbine 31 March 1995
      IMPLICIT none

      INTEGER sk1tmp, sk2tmp, trimfile, matchfile, skpoints
      INTEGER ndays, nskile 
      PARAMETER (nskile = 207)
      PARAMETER (ndays  =   6)
      
!      INCLUDE "buoy.inc"
      REAL rlat, rlong

      REAL dist1(nskile, ndays), dir1(nskile, ndays)
      REAL dist2(nskile, ndays), dir2(nskile, ndays)
      REAL sklat(nskile), sklon(nskile)

      REAL blat(2*ndays+1), blon(2*ndays+1)
      INTEGER bdate(2*ndays+1), adate
      CHARACTER*7 bname(2*ndays+1) 
      CHARACTER*80 tmp

      LOGICAL nearany

      INTEGER iret, i, j, sk, which, skref, fday, nobs
      INTEGER yy, mm, dd, hh
      INTEGER delay1, datedh
      REAL dx, dy, odist, odir

      DO 100 i = 1, nskile
        READ(skpoints, *) j, sklat(i), sklon(i)
CD        WRITE(*, *) j, sklat(i), sklon(i)
 100  CONTINUE

C     Getset loop -- Get a set of buoy locations, starting with an ob and
C       ending up to 6 days ago.
 1000 CONTINUE
        CALL getboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1               ddir, dir, dsp, sp, dsst, sst, trimfile, iret)
        IF (iret .EQ. RERROR) GO TO 1000
        IF (iret .EQ. ENDDAT) GO TO 9999
        IF (.NOT. nearany(sklat, sklon, nskile, toler, 
     1                    rlat, rlong, which) ) THEN
           GO TO 1000
        ENDIF
        nobs = 1
        skref = which
        blat(nobs)  = rlat
        blon(nobs)  = rlong
        bname(nobs) = id
CD        PRINT *,'bname, id = ',bname(1), id
        WRITE (tmp, 9002) date
        READ (tmp, 9003) yy, mm, dd, hh
        IF (hh .GT. 20) date = datedh(date, 24 - hh)
        IF (hh .GT. 0 .AND. hh .LT. 20) date = datedh(date, - hh)
CD        PRINT *,'date = ',date
        bdate(nobs) = date/100
        adate       = date/100

C       Get rest of obs w.in 6 days of first.
 1100   CONTINUE
        CALL getboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1               ddir, dir, dsp, sp, dsst, sst, trimfile, iret)
        IF (iret .EQ. RERROR) GO TO 1100
        IF (iret .EQ. ENDDAT) GO TO 1200
        IF (id .NE. bname(1)) GO TO 1200
        WRITE (tmp, 9002) date
        READ (tmp, 9003) yy, mm, dd, hh
        IF (hh .GT. 20) date = datedh(date, 24 - hh)
        IF (hh .GT. 0 .AND. hh .LT. 20) date = datedh(date, - hh)
        IF (delay1(date, adate*100) .LE. ndays*24 + 3 .AND. 
     1      delay1(date, adate*100) .GT. 12) THEN
          nobs = nobs + 1
          blat(nobs) = rlat
          blon(nobs) = rlong
          bname(nobs)  = id
          bdate(nobs) = date/100
CD          PRINT *,'date = ',date
CD          PRINT *,'bname, id = ',bname(nobs), id, blat(nobs), 
CD     1                blon(nobs), bdate(nobs)
          IF (nobs .GE. ndays+1) GO TO 1200
          GO TO 1100 
        ENDIF

 1200   CONTINUE

      IF (nobs .LE. 1) GO TO 1000

C     Now have a set of buoy observations out to ndays from a given date.
      CALL getfcst(adate, sk1tmp, sk2tmp, dir1, dist1, 
     1             dir2, dist2, iret)  
      IF (iret .NE. 0) GO TO 1000
      DO 2000 i = 2, nobs
CD        PRINT *,i, blat(1), blon(1), blat(i), blon(i), 
CD     1             bdate(1), bdate(i), bname(1), bname(i), 
CD     2             delay1(100*bdate(i), 100*bdate(1))
        CALL drift(blat(1), blon(1), blat(i), blon(i), 
     1        dx, dy, odist, odir)
        fday = ( delay1(100*bdate(i), 100*bdate(1) ) + 12 ) / 24
        IF (fday .LT. 1 .OR. fday .GT. ndays) GO TO 2000
        WRITE (matchfile, 9008) bname(1), skref, bdate(1), fday, 
     1      blat(1), blon(1), odist/kmtonm, odir, dist1(skref, fday),
     2      dir1(skref, fday), dist2(skref, fday), dir2(skref, fday)
 2000 CONTINUE

C     Now shuffle down the data
      DO 2100 i = 2, nobs
        bname(i-1) = bname(i)
        blat(i-1)  = blat(i)
        blon(i-1)  = blon(i)
        bdate(i-1) = bdate(i)
 2100 CONTINUE
      adate = bdate(1)
      nobs = nobs - 1
C     Go back to fill in the rest of the days for the new start date
      GO TO 1100

C     End of data condition.
 9999 CONTINUE
      IF (nobs .GT. 1) THEN
C       Now have a set of buoy observations out to ndays from a given date.
        CALL getfcst(adate, sk1tmp, sk2tmp, dir1, dist1, 
     1               dir2, dist2, iret)  
        IF (iret .NE. 0) GO TO 9998
        DO 3000 i = 2, nobs
          CALL drift(blat(1), blon(1), blat(i), blon(i), 
     1        dx, dy, odist, odir)
          fday = ( delay1(100*bdate(i), 100*bdate(1) ) + 12 ) / 24
          IF (fday .LT. 1 .OR. fday .GT. ndays) GO TO 3000
          WRITE (matchfile, 9008) bname(1), skref, bdate(1), fday, 
     1        blat(1), blon(1), odist, odir, dist1(skref, fday), 
     2        dir1(skref, fday), dist2(skref, fday), dir2(skref, fday)
 3000   CONTINUE
      ENDIF
      DO 3100 i = 2, nobs
        bname(i-1) = bname(i)
        blat(i-1)  = blat(i)
        blon(i-1)  = blon(i)
        bdate(i-1) = bdate(i)
 3100 CONTINUE
      adate = bdate(1)
      nobs = nobs - 1
      IF (nobs .GT. 1) GO TO 9999


 9008 FORMAT (A7, I4, I7, I3, F7.2, F8.2, 3x, 3(F6.1, F7.1) )
 9002 FORMAT (I8)
 9003 FORMAT (4I2)

 9998 CONTINUE
 
      REWIND (matchfile)

      RETURN
      END
