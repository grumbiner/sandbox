      SUBROUTINE getboy(id, code, rlat, rlong, date, dp, p, dt, t, 
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

      iret = -1

 1000 CONTINUE
        READ (unit,9001,END=2000,ERR=1001)
     1       id, code, lat, long, date, 
     2    dp, p, dt, t, ddir, dir, dsp, sp,
     3    dsst, sst
        rlat = lat/100.
C       Convert to degrees east, rather than west
        rlong = 360. - long/100.

        iret = 0
        RETURN

 1001 CONTINUE
CD        PRINT *,'Read error '
        iret = 2
        RETURN

 2000 CONTINUE
CD        PRINT *,'End of file '
        iret = 3
      
      RETURN
      END
