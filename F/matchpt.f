C================================================================         
      SUBROUTINE matchpt(lat, long, ntot, mm, dd, yy, pmatch, nmatch, 
     1      id, date, mlat, mlong, toler, totmatch)

      IMPLICIT none

      INTEGER mm, dd, yy
      INTEGER ntot, pmatch(ntot), nmatch(ntot)
      REAL lat(ntot), long(ntot)
      CHARACTER*7 id(3*ntot)
      INTEGER date(3*ntot)
      REAL toler, mlat(3*ntot), mlong(3*ntot)
C     Find matched to the points passed in.  Match == point within toler of
C      The original, on the date of the original.  Since working with a file,
C      must do a sequential read to locate.
      INTEGER i
      INTEGER tmm, tdd, tyy
      CHARACTER*7 tid, oid
      INTEGER tdate, tlat, tlong, odate, pdate
      INTEGER totmatch

      INTEGER days(12)
      REAL arcdis, rlat, rlong

      DATA days /31,28,31,30,31,30,31,31,30,31,30,31/
     
      
      pdate = ((yy*100+mm)*100+dd)*100
      odate = ((yy*100+mm)*100+dd)*100
CD      PRINT *,'pdate = ',pdate
      i = 0
 9001 FORMAT (A7, 5x, I5, I6, I9)
      REWIND (11)

 1000 CONTINUE
        READ (11, 9001, END=1100, ERR=1100) tid, 
     1                                      tlat, tlong, tdate
        IF (pdate - tdate .LE. 79) THEN
C         have a match
          GO TO 1200
         ELSE
          GO TO 1000
        ENDIF

 1100 CONTINUE
      PRINT *,'Error trying to match the dates, none found'
      STOP

 1200 CONTINUE
CD      PRINT *,'Found a date match, now writing out file 13, those'
CD      PRINT *,'  obs from time zero to d+6+3hours'
      OPEN (13, FILE='tempor', FORM='FORMATTED')

      tdd = dd
      tmm = mm
      tyy = yy
      IF (tdd + 6 .GT. days(tmm)) THEN
        IF (tmm .EQ. 2) THEN
          PRINT *,'Must write leap year code'
         ELSE
          tdd = tdd + 6 - days(tmm)
          tmm = tmm + 1
          IF (tmm .GT. 12) THEN
           tmm = 1
           tyy = tyy + 1
          ENDIF
        ENDIF
       ELSE
        tdd = tdd + 6
      ENDIF

      pdate = ((tyy*100+tmm)*100+tdd)*100
      PRINT *,'Last date to match is ',pdate
      BACKSPACE 11
 
 2000 CONTINUE
        READ (11, 9001, END=2100, ERR=2100) tid, tlat, tlong, tdate
CD        PRINT *,pdate, tdate
        IF (pdate - tdate .GE. -3) THEN
          IF (MOD(tdate, 100) .GE. 21 .OR. MOD(tdate, 100) .LE. 3) 
     1          WRITE (13, 9001) tid, tlat, -tlong, tdate
          GO TO 2000
        ENDIF
C     Fallen out of loop
 2100 CONTINUE
      
C     Now ready to search for location matches:
      pdate = odate
      PRINT *,'Starting to look for location matches'
      totmatch = 0
      oid      = ' '
      DO 3000 i = 1, ntot
        pmatch(i) = 0
        nmatch(i) = 0
        REWIND(13)
 3100   CONTINUE
          READ (13, 9001, END=3200, ERR=3200) tid, 
     1                                        tlat, tlong, tdate
          IF (pdate - tdate .GT. 79) GO TO 3100 
          IF (pdate - tdate .LT. -3) GO TO 3200
C         Only take the first occurrence of a buoy id.
          IF (oid .EQ. tid) GO TO 3100
          oid = tid
          rlat = tlat / 100.
          rlong = tlong / 100.
          IF (arcdis(long(i), lat(i), rlong, rlat) .LE. toler) THEN
            totmatch = totmatch + 1
            nmatch(i) = nmatch(i) + 1
            IF (nmatch(i) .EQ. 1) THEN
              pmatch(i) = totmatch
            ENDIF
            id(totmatch)    = tid
            mlat(totmatch)  = rlat
            mlong(totmatch) = rlong
            date(totmatch)  = tdate
          ENDIF
        GO TO 3100
 3200 CONTINUE
 3000 CONTINUE
C     Should now have all the matches which are within a 3 hour 
C       window of 00 Z.

      RETURN
      END
