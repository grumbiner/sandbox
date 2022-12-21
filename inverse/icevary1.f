      PROGRAM icevary
C     Variational inversion of concentration and growth rate information.
C     -- Coupled to a full ice thickness distribution evolution model.
C     Robert Grumbine 5/31/94.
 
      IMPLICIT none
      INCLUDE "inv.inc"

C     Evolution parameters
      REAL y(nstep), a(nstep), b(nstep), c(nstep), w(nstep), rhs(nstep)
      REAL aimprov(nstep), rimprov(nstep)

C     Inversion parameters.
      REAL beta, sset, t, a0, r0, r0dot
      EXTERNAL a0, r0, r0dot
      
      REAL ier
      INTEGER i, j, nvary
      REAL rfrac
      
      OPEN (10, FILE='bering.dat', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='freezout', FORM='FORMATTED', STATUS='UNKNOWN')

      ier = sset(0.)
      IF (ier .NE. 0.) THEN
        PRINT *,'Error in reading from sset '
        STOP
      ENDIF

      PRINT *,'Enter beta'
      READ (*, 9001) beta
      PRINT *, beta
 9001 FORMAT (E13.6)
      PRINT *,'What fraction of the whole run do you want?'
      READ (*,9001) rfrac

C     Find the 'smoothed' values.
      nvary = INT(rfrac*nstep+0.5)
      PRINT *,'Will take ',nvary , ' steps'
      DO 1000 i = 1, nvary 
        w(i) = 1.
        y(i) = a0((i-1)*dt)
 1000 CONTINUE
      CALL varwght(y, r0, a, b, c, rhs, dt, beta, nstep, w, 3)
      CALL tridig(a, b, c, rhs, aimprov, nstep)

      DO 2000 i = 2, nvary -1
        rimprov(i) = - (aimprov(i+1)-aimprov(i-1) ) / 2. / dt
 2000 CONTINUE
      rimprov(1) = - (aimprov(2)-aimprov(1))/dt
      rimprov(nvary) = - (aimprov(nvary) - aimprov(nvary-1))/dt

      DO 3000 i = 1, nvary
        t = (i-1)*dt
        WRITE (*,9003) t, aimprov(i), a0(t),
     1                    rimprov(i), r0(t), -a0(t)+aimprov(i)
     2  , r0dot(a0, t, dt) + r0(t)  
        WRITE (12,9003) t, aimprov(i), a0(t),
     1                    rimprov(i), r0(t), -a0(t)+aimprov(i)
     2  , r0dot(a0, t, dt) + r0(t)  
 3000 CONTINUE
        
 9003 FORMAT (F8.2, 2F8.4, 2F10.6, F9.6, 2F8.4)

      STOP
      END
