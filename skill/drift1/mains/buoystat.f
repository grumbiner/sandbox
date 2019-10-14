      PROGRAM buoystat
C     Compute statistics from drifting buoys

      IMPLICIT none

      INTEGER nobs
      PARAMETER (nobs = 210000)
      CHARACTER*7 id(nobs), name
      INTEGER code(nobs), lat(nobs), long(nobs)
      REAL rlat(nobs), rlong(nobs)
      INTEGER dp(nobs), p(nobs), dt(nobs), t(nobs), ddir(nobs), 
     1    dir(nobs), dsp(nobs), sp(nobs),
     3    dsst(nobs), sst(nobs)
      CHARACTER*9 date(nobs)
      INTEGER i, j, ntot, loc(12)
      CHARACTER*60 fname
      REAL sum(12,2)

CD      OPEN (10, FILE='buoys', FORM='FORMATTED', STATUS='OLD')

      i = 0
 1000 CONTINUE
        i = i + 1
        READ (10,9001,END=2000,ERR=1001)
     1       id(i), code(i), lat(i), long(i), date(i), 
     2    dp(i), p(i), dt(i), t(i), ddir(i), dir(i), dsp(i), sp(i),
     3    dsst(i), sst(i)
        GO TO 1000
 1001 CONTINUE
        i = i - 1
        PRINT *,'Read error ',i
        GO TO 1000

 2000 CONTINUE
      
 9001 FORMAT (A7, I4, I5, I6, A9, 5(I5,I4))
      
      ntot = i - 1
C     Convert to degrees lat, long
      DO 3000 i = 1, ntot
        rlat(i) = lat(i)/100.
        rlong(i) = long(i)/100.
 3000 CONTINUE

      j = 0 
      name = ' '
      sum = 0.0
      loc = 0
      DO 3100 i = 1, ntot
        IF (id(i) .NE. name) THEN
          IF (loc(6) .GT. 15 .AND. sum(6,1)/loc(6)/10. .LT. 5.
     1         .AND. ABS(sum(1,1)/loc(1)) .GT. 40.) THEN 
            DO 3101 j = 1, 8
              IF (loc(j) .GT. 1) THEN
                sum(j,1) = sum(j,1)/loc(j)
                sum(j,2) = ABS(sum(j,2) - sum(j,1)*sum(j,1)*loc(j) )
     1                   /(loc(j)-1)
               ELSE
                sum(j,1) = 0.0
                sum(j,2) = 0.0
              ENDIF
 3101       CONTINUE
            sum(1,1) = 10.*sum(1,1)
            sum(1,2) = 100.*sum(1,2)
            sum(2,1) = 10.*sum(2,1)
            sum(2,2) = 100.*sum(2,2)
            sum(4,1) = sum(4,1)-10000.
            PRINT *,' '
CD            PRINT *,'buoy, obs ',name, '  ',code(i-1),loc(1)
            WRITE (*,9003) name, code(i-1), loc(1),
     1    (sum(j,1)/10.,j=1,8)
            WRITE (*,9003) name, code(i-1), loc(1),
     1    (sum(j,2)**0.5/10.,j=1,8)
            WRITE (*,9004) name, code(i-1), loc(1),
     1    (loc(j),j=1,8)
          ENDIF
 9003     FORMAT (A8, I4, I4, 3(F6.1),1x,F8.2,8(F7.2))
 9004     FORMAT (A8, I4, I4, 3I6,I8,8(I7))
          name = id(i)
          sum = 0.0
          loc = 0
         ELSE
          IF (rlat(i) .LT. 99.99) THEN
            loc(1) = loc(1) + 1
            sum(1,1) = sum(1,1) + rlat(i)
            sum(1,2) = sum(1,2) + rlat(i)*rlat(i)
          ENDIF
          IF (rlong(i) .LT. 361.) THEN
            loc(2) = loc(2) + 1
            sum(2,1) = sum(2,1) + rlong(i)
            sum(2,2) = sum(2,2) + rlong(i)*rlong(i)
          ENDIF
CD          IF (dp(i) .NE. 9999) THEN
          IF (rlat(i) .LT. 99.99 .AND. rlong(i) .LT. 361. .AND. 
     1        dp(i) .NE. 9999) THEN
            loc(3) = loc(3) + 1
            sum(3,1) = sum(3,1) + dp(i)
            sum(3,2) = sum(3,2) + dp(i)*dp(i)
          ENDIF
          IF (p(i) .NE. -99) THEN
            loc(4) = loc(4) + 1
            IF (p(i) .GT. 500.) THEN
              p(i) = p(i)+9000.
             ELSE
              p(i) = p(i)+10000.
            ENDIF
            sum(4,1) = sum(4,1) + p(i)
            sum(4,2) = sum(4,2) + p(i)*p(i)
          ENDIF
          IF (rlat(i) .LT. 99.99 .AND. rlong(i) .LT. 361. .AND. 
     1        dt(i) .NE. 9999) THEN
CD          IF (dt(i) .NE. 9999 .AND. ABS(dt(i)) .LT. 250.) THEN
            loc(5) = loc(5) + 1
            sum(5,1) = sum(5,1) + dt(i)
            sum(5,2) = sum(5,2) + dt(i)*dt(i)
          ENDIF
          IF (t(i) .NE. -99) THEN
            loc(6) = loc(6) + 1
            sum(6,1) = sum(6,1) + t(i)
            sum(6,2) = sum(6,2) + t(i)*t(i)
          ENDIF
CD          IF (ddir(i) .NE. 9999) THEN
CD          loc(1) = loc(1) + 1
CD            sum(7,1) = sum(7,1) + ddir(i)
CD            sum(7,2) = sum(7,2) + ddir(i)*ddir(i)
CD          ENDIF
CD          IF (dir(i) .NE. -99) THEN
CD          loc(1) = loc(1) + 1
CD            sum(8,1) = sum(8,1) + dir(i)
CD            sum(8,2) = sum(8,2) + dir(i)*dir(i)
CD          ENDIF
CD          IF (dsp(i) .NE. 9999) THEN
CD          loc(1) = loc(1) + 1
CD            sum(9,1) = sum(9,1) + dsp(i)
CD            sum(9,2) = sum(9,2) + dsp(i)*dsp(i)
CD          ENDIF
CD          IF (sp(i) .NE. -99) THEN
CD            loc(7) = loc(7) + 1
CD            sum(7,1) = sum(7,1) + sp(i)
CD            sum(7,2) = sum(7,2) + sp(i)*sp(i)
CD          ENDIF
          IF (dsst(i) .NE. 9999) THEN
            loc(7) = loc(7) + 1
            sum(7,1) = sum(7,1) + dsst(i)
            sum(7,2) = sum(7,2) + dsst(i)*dsst(i)
          ENDIF
          IF (sst(i) .NE. -99) THEN
            loc(8) = loc(8) + 1
            sum(8,1) = sum(8,1) + sst(i)
            sum(8,2) = sum(8,2) + sst(i)*sst(i)
          ENDIF
        ENDIF
 3100 CONTINUE

      STOP
      END
