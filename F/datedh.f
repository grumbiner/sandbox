      INTEGER FUNCTION datedh(date, dh)
C     Function to return a 8 digit date code for a given difference
C       from the initial.
      IMPLICIT none

      CHARACTER*8 tmp
      INTEGER date, dh, dd, mm, yy, hh, tdd, tmm, tyy, thh
      INTEGER tmp2, ndays(12)
      ndays(1) = 31
      ndays(2) = 28
      ndays(3) = 31
      ndays(4) = 30
      ndays(5) = 31
      ndays(6) = 30
      ndays(7) = 31
      ndays(8) = 31
      ndays(9) = 30
      ndays(10) = 31
      ndays(11) = 30
      ndays(12) = 31

 9001 FORMAT (I8)
 9002 FORMAT (4I2)
      WRITE (tmp, 9001) date
CD      PRINT *,'date , dh = ', date, dh, 'tmp = ',tmp
      READ (tmp, 9002) yy, mm, dd, hh

      IF (dh .GT. 24) THEN
        PRINT *,'Cannot handle jumps of more than a day'
        datedh = -1
        RETURN
      ENDIF


      thh = hh 
      tdd = dd 
      tmm = mm
      tyy = yy
      
      IF (thh .GE. 24 ) THEN
        tmp2 = thh / 24
        thh = MOD(thh, 24)
        tdd = tdd + tmp2
      ENDIF

      thh = thh + dh
      IF (thh .GE. 24 ) THEN
        tmp2 = thh / 24
        thh = MOD(thh, 24)
        tdd = tdd + tmp2
      ENDIF
      IF (thh .LT. 0 ) THEN
        tmp2 = thh / 24
        thh  = thh + (tmp2 + 1) *24
        tdd = tdd + tmp2
      ENDIF

        
      
      IF (tdd .GT. ndays(tmm) ) THEN
        IF (tmm .EQ. 2) THEN
C         leap year testing
          IF (MOD(yy, 4) .EQ. 0 ) THEN
            IF (tdd .EQ. 29) THEN
C             do nothing
             ELSE
              tdd = tdd - 29
              tmm = tmm + 1
            ENDIF
           ELSE
            tdd = tdd - 28
            tmm = tmm + 1
          ENDIF 
            
        ELSE
          tdd = tdd - ndays(mm)
          tmm = tmm + 1
        ENDIF
      ENDIF

      IF (tmm .GT. 12) THEN
CD        PRINT *,'in tmm > 12 test'
        tmm = tmm - 12
        tyy = tyy + 1
      ENDIF

      IF (tyy .GT. 99) THEN
        PRINT *,'Passed a century break!  You may have problems'
      ENDIF
 
C     Handle moving back across months and years
      IF (tdd .LE. 0) THEN
CD        PRINT *,'in tdd <= 0 test'
        tmm = tmm - 1
        IF (tmm .LE. 0) THEN
          tyy = tyy - 1
          tmm = 12 + tmm
        ENDIF
        IF (tmm .NE. 2) THEN
          tdd = ndays(tmm) + tdd
         ELSE
          IF (MOD(tyy, 4) .EQ. 0) THEN
            PRINT *,'leap year code'
            tdd = 29 + tdd
           ELSE
            tdd = 28 + tdd
          ENDIF
        ENDIF
        IF (tmm .LE. 0) THEN
          tyy = tyy - 1
          tmm = 12 + tmm
        ENDIF
      ENDIF

      IF (tmm .LE. 0) THEN
CD        PRINT *,'in tmm <= 0 test'
        tmm = tmm + 12
        tyy = tyy - 1
      ENDIF

      IF (tyy .LT. 0) THEN
        PRINT *,'Passed a century break!  You may have problems'
        tyy = 100 + tyy
      ENDIF
     
      IF (tmm .GT. 12 .OR. tmm .LE. 0) THEN 
        PRINT *,'some sort of error on months'
        PRINT *,tyy, tmm, tdd
        PRINT *,'original date = ',date, dh
        STOP
      ENDIF

      datedh = thh + 100* (tdd + 100 * (tmm + 100 * tyy) )
CD      PRINT *,'datedh ',datedh, tyy, tmm, tdd, delta

      RETURN
      END      
