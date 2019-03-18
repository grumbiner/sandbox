      SUBROUTINE putboy(id, code, rlat, rlong, date, dp, p, dt, t, 
     1                  ddir, dir, dsp, sp, dsst, sst, unit, iret)

      IMPLICIT none

      CHARACTER*7 id
      INTEGER code, lat, long
      REAL rlat, rlong
      INTEGER dp, p, dt, t, ddir, 
     1    dir, dsp, sp,
     3    dsst, sst
      INTEGER date, unit, iret

 9001 FORMAT (A7, I4, I5, I6, I9, 5(I5,I4))

C     Return buoys to the integer NOS convention
      lat = INT(rlat * 100. + 0.5)
      IF (rlong .GT. 360. ) rlong = AMOD(rlong, 360.)
      IF (rlong .LT. 0. ) rlong = rlong + 360.
      long =  INT( -(rlong -360. )*100. + 0.5)
      WRITE (unit,9001,ERR=1001)
     1       id, code, lat, long, date, 
     2    dp, p, dt, t, ddir, dir, dsp, sp,
     3    dsst, sst

      RETURN

 1001 CONTINUE
      PRINT *,'Write error in putbuoy '
      STOP
 
      RETURN
      END
