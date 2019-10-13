      SUBROUTINE cdtread(hhmm, dd, mo, yy, sp, dir, u, v, temp, press, 
     1                   seq, ierr)

      INTEGER hhmm, dd, mo, yy, seq, ierr;
      REAL sp, dir, u, v, temp, press;
      INTEGER idir

      ierr = 0
      READ (*,*, ERR=2000, END=2000) hhmm, dd, mo, yy, sp, idir, 
     1                       u, v, temp, press, seq
      dir = idir
      
      RETURN

 2000 CONTINUE
      ierr = 1
      RETURN
      END
