      PROGRAM test
      INCLUDE "inv.inc"
      REAL a0, r0, r0dot
      EXTERNAL a0, r0, r0dot
      REAL ier

      OPEN (10, FILE='bering.dat', STATUS='OLD', FORM='FORMATTED')

      ier = sset(0.)
      IF (ier .NE. 0.) THEN
        PRINT *,'Failed to read in table'
        STOP
      ENDIF

      DO 1000 i = 1, nstep
        t = (i-1)*dt
        WRITE (*,9001) a0(t), r0(t), -r0dot(a0, t, dt), 
     1     ABS ( (r0(t)+r0dot(a0, t, dt) ) /r0dot(a0, t, dt) )
 1000 CONTINUE
 9001 FORMAT (F9.4, 2F11.6,2x, E13.6)

      STOP
      END 
