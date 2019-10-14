      LOGICAL FUNCTION timer(id, code, rlat, rlong, date, dp, p, 
     1       dt, t, ddir, dir, dsp, sp, dsst, sst, refhh, window)

C     Return true if the buoy observation time is within the window period
C       of the reference date/hour.

      IMPLICIT none

!      INCLUDE "buoy.inc"
      REAL rlat, rlong
      
      INTEGER hh, refhh, window, hmin, hmax
      LOGICAL land
      CHARACTER*8 tmp
      
C     This is currently assuming that the day part of the date is
C       appropriate.

 9009 FORMAT (6x, I2)
 9008 FORMAT (I8)
      WRITE (tmp, 9008) date
      READ (tmp, 9009) hh
      land = .TRUE.
      hmin = refhh - window
      hmax = refhh + window
      IF (hmin .LT. 0) THEN
        hmin = 24 + hmin
        land = .FALSE.
      ENDIF
      IF (hmax .GE. 24) THEN
        hmax = hmax - 24
        land = .FALSE.
      ENDIF

      IF (land) THEN
        IF ( hh .GE. hmin .AND. hh .LE. hmax ) THEN
          timer = .TRUE.
         ELSE
          timer = .FALSE.
        ENDIF
      ELSE
       IF (hh .GE. hmin .OR. hh .LE. hmax) THEN
         timer = .TRUE.
        ELSE
         timer = .FALSE.
       ENDIF
      ENDIF


      RETURN
      END
