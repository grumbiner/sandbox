      PROGRAM survey
C     'Survey' a buoy file.
C     -- Check for good data records
C     -- Check for polar buoys
C     -- Check that obs are within 3 hours of 00 UTC
C     -- Trim the results of the above by averaging multiple
C          observations within the window into a single observation.
C     Bob Grumbine 30 March 1995.


      IMPLICIT none
      INCLUDE "buoy.inc"
      REAL rlat, rlong
      INTEGER iret

      CHARACTER*60 fname
      INTEGER inunit, ckunit, tunit
      PARAMETER (inunit = 10)
      PARAMETER (ckunit = 11)
      PARAMETER (tunit = 12)
      LOGICAL latok, lonok, pok, tok, dirok, spok, sstok
      LOGICAL timer, polardb


      PRINT *,'Name of full file?'
      READ (*, 9001) fname
      OPEN (inunit, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      PRINT *,'Name of checked file?'
      READ (*, 9001) fname
      OPEN (ckunit, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      PRINT *,'Name of trimmed file?'
      READ (*, 9001) fname
      OPEN (tunit, FILE=fname, FORM='FORMATTED', STATUS='NEW')
 9001 FORMAT (A60)


C     First trim the data file

 1000 CONTINUE
        CALL getboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1               ddir, dir, dsp, sp, dsst, sst, inunit, iret)
        IF (iret .EQ. RERROR) GO TO 1000
        IF (iret .EQ. ENDDAT) GO TO 2000
        CALL buoyck(id, code, rlat, rlong, date, dp, p, dt, t, 
     1               ddir, dir, dsp, sp, dsst, sst, 
     2               latok, lonok, pok, tok, dirok, spok, sstok)
        IF ( .NOT. (latok .AND. lonok) ) THEN
          GO TO 1000
         ELSE
          IF ( .NOT. polardb(id, code, rlat, rlong, date, dp, p, 
     1           dt, t, ddir, dir, dsp, sp, dsst, sst) ) THEN
            GO TO 1000
          ELSE
           IF ( timer(id, code, rlat, rlong, date, dp, p,
     1           dt, t, ddir, dir, dsp, sp, dsst, sst, 00, 3) ) THEN
             CALL putboy(id, code, rlat, rlong, date, dp, p, 
     1          dt, t, ddir, dir, dsp, sp, dsst, sst, ckunit, iret)
           ENDIF
          ENDIF
        ENDIF
        GO TO 1000

 2000 CONTINUE

      PRINT *,'finished trimming'

      REWIND(ckunit)
      CALL timtrim(6, ckunit, tunit)

      STOP
      END
