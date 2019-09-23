      LOGICAL FUNCTION polardb(id, code, rlat, rlong, date, dp, p, 
     1 dt, t, ddir, dir, dsp, sp, dsst, sst)

C     True if the buoy is a polar (lat > 40) drifting buoy.
C     Bob Grumbine 30 March 1995

      IMPLICIT none

!      INCLUDE "buoy.inc"
      REAL rlat, rlong


      IF (  ABS(rlat) .GT. 40. .AND. code .EQ. DB )  THEN
        polardb = .TRUE.
       ELSE
        polardb = .FALSE.
      ENDIF

      RETURN
      END
