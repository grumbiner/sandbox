      PROGRAM icevary
C     Variational inversion of concentration and growth rate information.
C     -- Coupled to a full ice thickness distribution evolution model.
C     Robert Grumbine 5/31/94.
C     Version to work from an ice analysis grid. 1/13/95 Bob Grumbine
 
      IMPLICIT none
      INCLUDE "inv.inc"

C     Evolution parameters
      REAL y(nstep), a(nstep), b(nstep), c(nstep), w(nstep), rhs(nstep)
      REAL aimprov(nstep), rimprov(nstep)

C     Inversion parameters.
      REAL beta, sset, t, a0, r0, r0dot
      EXTERNAL a0, r0, r0dot
      
      REAL ier
      INTEGER i, j, nvary, ni, nj, windex
      REAL rfrac
      CHARACTER*60 fname
      LOGICAL nonzer

C     Open the files and set parameters
 9009 FORMAT (A60)
      READ (*,9009) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      READ (*,9009) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='UNKNOWN')
      PRINT *,'Enter beta'
      READ (*, 9001) beta
      PRINT *, beta
 9001 FORMAT (E13.6)
      PRINT *,'What fraction of the whole run do you want?'
      READ (*,9001) rfrac

      
C     Now loop over all pixels for all times
      DO 5000 nj = 1, NY
      DO 5000 ni = 1, NX 
        ier = sset(0.)
        IF (ier .NE. 0.) THEN
          PRINT *,'Error in reading from sset '
          STOP
        ENDIF

C     Find the 'smoothed' values.
        nvary = INT(rfrac*nstep+0.5)
        PRINT *,'Will take ',nvary , ' steps'
        nonzer = .FALSE.
        DO 1000 i = 1, nvary 
          IF (a0((i-1)*dt) .GT. 1.28 ) THEN
            w(i) = 0.
          ELSE IF ( a0((i-1)*dt) .GT. 1.00) THEN
            w(i) = 1.
            y(i) = 1.
            nonzer = .TRUE.
          ELSE
            nonzer = .TRUE.
            w(i) = 1.
            y(i) = a0((i-1)*dt)
          ENDIF
 1000   CONTINUE

        IF (.NOT. nonzer) THEN
C         There are no ice concentration points, all are bad data, land,
C           or no data.
          CALL vset(w, nstep, 0.)
          CALL vset(y, nstep, 0.)
          CALL vset(rimprov, nstep, 0.)
          CALL vset(aimprov, nstep, 0.)
          GO TO 3001
        ENDIF 
C       Now must handle no data end points:
        IF (w(1) .EQ. 0.) THEN
          windex = 1
 1001   CONTINUE
          windex = windex+1
          IF (w(windex) .EQ. 0. .OR. y(windex) .GT. 1.28 .AND. 
     1        windex .LT. nvary) THEN
            GO TO 1001
          ELSE IF (y(windex) .LT. 1.28) THEN
            y(1) = y(windex)
            w(1) = 1.0
            GO TO 1002
          ELSE IF (windex .EQ. nvary) THEN
            CALL vset(w, nstep, 0.)
            CALL vset(y, nstep, 0.)
            CALL vset(rimprov, nstep, 0.)
            CALL vset(aimprov, nstep, 0.)
            GO TO 3001
          ENDIF
 1002   CONTINUE
        ENDIF

        IF (w(nvary) .EQ. 0.) THEN
          windex = nvary
 1003   CONTINUE
          windex = windex - 1
          IF (w(windex) .EQ. 0. .OR. y(windex) .GT. 1.28 .AND. 
     1        windex .GT. 1) THEN
            GO TO 1003
          ELSE IF (y(windex) .LT. 1.28) THEN
            y(nvary) = y(windex)
            w(nvary) = 1.0
            GO TO 1004
          ELSE IF (windex .EQ. 1) THEN
            CALL vset(w, nstep, 0.)
            CALL vset(y, nstep, 0.)
            CALL vset(rimprov, nstep, 0.)
            CALL vset(aimprov, nstep, 0.)
            GO TO 3001
          ENDIF
 1004   CONTINUE
        ENDIF

          


        CALL varwght(y, r0, a, b, c, rhs, dt, beta, nstep, w, 3)
        CALL tridig(a, b, c, rhs, aimprov, nstep)
  
        DO 2000 i = 2, nvary -1
          rimprov(i) = - (aimprov(i+1)-aimprov(i-1) ) / 2. / dt
 2000   CONTINUE
        rimprov(1) = - (aimprov(2)-aimprov(1))/dt
        rimprov(nvary) = - (aimprov(nvary) - aimprov(nvary-1))/dt

 3001   CONTINUE 
        DO 3000 i = 1, nvary
          t = (i-1)*dt
CD          WRITE (*,9003) t, aimprov(i), a0(t),
CD     1                      rimprov(i), r0(t), -a0(t)+aimprov(i)
CD     2    , r0dot(a0, t, dt) + r0(t)  
          WRITE (12,9003) t, aimprov(i), a0(t),
     1                      rimprov(i), r0(t), -a0(t)+aimprov(i)
     2    , r0dot(a0, t, dt) + r0(t)  
 3000   CONTINUE
        
 5000 CONTINUE

 9003 FORMAT (F8.2, 2F8.4, 2F10.6, F9.6, 2F8.4)

      STOP
      END
