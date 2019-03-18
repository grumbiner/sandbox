      SUBROUTINE remdup(unit, idlist)
C     Remove duplicate entries by averaging the location sightings
      IMPLICIT none

      INTEGER unit, maxdup
      PARAMETER (maxdup = 5000)

C     Return variables
      CHARACTER*7 idlist(maxdup)

C     Temporary variables
      INTEGER tj(maxdup), tbdate(maxdup)
      REAL tsklon(maxdup), tsklat(maxdup), trlong(maxdup), trlat(maxdup)
      CHARACTER*7 tid(maxdup)

C     Final variables
      INTEGER fj(maxdup), fbdate(maxdup)
      REAL fsklon(maxdup), fsklat(maxdup)
      REAL frlong(maxdup), frlat(maxdup)
      CHARACTER*7 fid(maxdup)

C     Temporary internal file for write-out
      CHARACTER*56 tfile(maxdup)
      INTEGER nbuoys, ndup, index, nindep
      INTEGER dater, hr0, hr1, hrout, i

C     Begin execution
C     Read the buoy data into memory (t___(i) arrays)
      REWIND (unit)

 9009 FORMAT (I4, 2F9.4, 2F9.4, I9, A7)
      i = 0
 1000 CONTINUE
      i = i + 1
      READ (unit, 9009, END=2000) tj(i), tsklon(i), tsklat(i), 
     1    trlong(i), trlat(i), tbdate(i), tid(i)
      GO TO 1000

 2000 CONTINUE
      nbuoys = i - 1 
CD      PRINT *,'Starting with ',nbuoys,' buoys'


C     Here is the loop to cycle over all the possible buoys to
C       check for duplicates.

C     Write out the first buoy
      WRITE (tfile(1), 9009) 
     1  tj(1), tsklon(1), tsklat(1), 
     1    trlong(1), trlat(1), tbdate(1), tid(1)

      ndup = 1
      DO 3000 i = 2, nbuoys
        IF (tid(i) .EQ. tid(1)) THEN
          ndup = ndup+1
          WRITE (tfile(ndup), 9009) 
     1       tj(i), tsklon(i), tsklat(i), 
     1       trlong(i), trlat(i), tbdate(i), tid(i)
        ENDIF
 3000 CONTINUE

      IF (ndup .GT. 1) THEN
        PRINT *,'found ', ndup, ' duplicates '
C       Must average the observed trlat, trlong, tdate.  Note
C         that hours in tdates must be referenced to 24, 0.
        DO 3100 i = 1, ndup
          READ (tfile(i), 9009) 
     1      fj(i), fsklon(i), fsklat(i), 
     1        frlong(i), frlat(i), fbdate(i), fid(i)
          WRITE (*, 9009) 
     1      fj(i), fsklon(i), fsklat(i), 
     1        frlong(i), frlat(i), fbdate(i), fid(i)
 3100   CONTINUE

        hr0 = MOD(fbdate(1), 100)
        IF (hr0 .GT. 12) hr0 = hr0 - 24
CD        PRINT *,hr0
        DO 3110 i = 2, ndup
C         Note that this does not handle meridian crossings
          frlong(1) = frlong(1)+frlong(i)
          frlat(1) = frlat(1) + frlat(i)
          hr1 = MOD(fbdate(i), 100)
          IF (hr1 .GT. 12) hr1 = hr1 - 24
CD          PRINT *,hr1
          hr0 = hr0 + hr1
 3110   CONTINUE
        frlong(1) = frlong(1) / ndup
        frlat(1) = frlat(1) / ndup
        PRINT *,'hr0 summed is ',hr0, (FLOAT(hr0)/FLOAT(ndup))
        hrout = INT ( (FLOAT(hr0) / FLOAT(ndup) )  + 0.5 )
        hr0 = MOD(fbdate(1), 100)
        IF (hr0 .GT. 12) hr0 = hr0 - 24
        IF ((hr0 .LT. 0 .AND. hrout .LT. 0) .OR.
     1      (hr0 .GT. 0 .AND. hrout .GT. 0)      ) THEN
          IF (hrout .LT. 0 ) hrout = 24 + hrout
          fbdate(1) = 100*(fbdate(1)/100) + hrout
         ELSE IF (hr0 .LT. 0.) THEN
C         hrout gt 0 > add a day
          fbdate(1) = 100 * dater(fbdate(1)/100, 1) + hrout
         ELSE IF (hr0 .GT. 0.) THEN
C         hrout lt 0, go back a day and fix the 24 hour clock
          fbdate(1) = 100 * dater(fbdate(1)/100, -1)
          hrout = 24 + hrout
          fbdate(1) = fbdate(1) + hrout
        ENDIF
C       END of multiple duplicate buoys

      ELSE
          PRINT *,'Only one occurrence of the buoy'
          READ (tfile(1), 9009) 
     1      fj(1), fsklon(1), fsklat(1), 
     1        frlong(1), frlat(1), fbdate(1), fid(1)

      ENDIF

          WRITE (*, 9009) 
     1      fj(1), fsklon(1), fsklat(1), 
     1        frlong(1), frlat(1), fbdate(1), fid(1)



         
        
       

      RETURN
      END
