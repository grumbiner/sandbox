      PROGRAM buoydates
C     Split buoy file(s) into files by date.  Assume that
C       the file has already been sorted by date.
      IMPLICIT none

      CHARACTER*60 fname

      CHARACTER*7 id
      REAL rlat, rlong
      INTEGER code, dp, p, dt, t, ddir, dir, dsp, sp, dsst, sst
      INTEGER date, iunit, ounit, iret

      INTEGER tdate, odate

 9001 FORMAT (A60)
 9002 FORMAT ('buoy.',I6)
      PRINT *,'What is the buoy file name?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')

      odate = 0
      iunit = 10
      ounit = 11

C     Read in first buoy and open first file.
C     Establish 'old' values.
      CALL getboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1            ddir, dir, dsp, sp, dsst, sst, iunit, iret)
      IF (iret .NE. 0) THEN
        PRINT *,'get buoy return error ',iret
        STOP
      ENDIF
      tdate = date / 100
      odate = tdate
      WRITE (fname, 9002) tdate
      OPEN (ounit, FILE=fname, FORM='FORMATTED', STATUS='UNKNOWN')
      CALL putboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1            ddir, dir, dsp, sp, dsst, sst, ounit, iret)
       
C     Now repeat until we run out of data
 1000 CONTINUE
      CALL getboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1            ddir, dir, dsp, sp, dsst, sst, iunit, iret)
      IF (iret .EQ. 2) THEN
        PRINT *,'getboy read error, stopping'
        STOP
      ENDIF
      IF (iret .EQ. 3) THEN
        PRINT *,'Reached end of file, exiting'
        CLOSE (ounit, STATUS='KEEP')
        GO TO 2000
      ENDIF
    
      tdate = date/100
      IF (tdate .EQ. odate) THEN
        CALL putboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1            ddir, dir, dsp, sp, dsst, sst, ounit, iret)
       ELSE
        CLOSE (ounit, STATUS='KEEP')
        odate = tdate
        WRITE (fname, 9002) tdate
        OPEN (ounit, FILE=fname, FORM='FORMATTED', STATUS='UNKNOWN')
        CALL putboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1            ddir, dir, dsp, sp, dsst, sst, ounit, iret)
      ENDIF

      GO TO 1000


 2000 CONTINUE

      STOP
      END
