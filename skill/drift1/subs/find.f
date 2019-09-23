C====================================================================
      SUBROUTINE find(mm, dd, yy, nday, id, flat, flong)
C     From within the 'find' file, locate the next location of the
C       platform id given.  Original time = yymmdd00, new is dd+nday
C       wrapped as needed.
C     Bob Grumbine 21 April 1994

      IMPLICIT none

      INTEGER mm, dd, yy, nday
      INTEGER tmm, tdd, tyy
      CHARACTER*7 id, tid, oid
      REAL flat, flong
      INTEGER ilat, ilong
      INTEGER days(12)
      DATA days /31,28,31,30,31,30,31,31,30,31,30,31/
      
      INTEGER pdate, date

      tmm = mm
      tdd = dd
      tyy = yy
      oid = id
      flat = 0.
      flong = 0.

      pdate = ((tyy*100+tmm)*100+tdd)*100
      IF (tdd+nday .LE. days(tmm) ) THEN
         pdate = pdate + nday*100
        ELSE IF (tmm .EQ. 2) THEN
         PRINT *,'Must write the leap year code'
        ELSE
         PRINT *,'Crossed month boundary'
         tdd = tdd + nday - days(tmm)
         tmm = tmm + 1
         pdate = ((tyy*100+tmm)*100+tdd)*100
      ENDIF
C     Need case for mm = 13.
CD      PRINT *,'Args to find ',mm, dd, yy, oid, pdate, nday 

      REWIND (13)
 9001 FORMAT (A7, 5x, I5, I6, I9)
 1000 CONTINUE
        READ (13, 9001, END=1100, ERR=1101) tid, ilat, ilong, date

        IF (oid .EQ. tid) THEN
CD         PRINT *,'testing date ',pdate, tid, ilat, ilong, date
         IF ( ( (pdate - date .GE. 76) .AND. (pdate - date .LE. 79) )
     1             .OR. ABS(pdate - date) .LE. 3) THEN
             flat = ilat / 100.
             flong = ilong / 100.
             RETURN
           ENDIF
          ELSE
           GO TO 1000
         ENDIF
 1101    CONTINUE
CD           PRINT *,'Read error'
           GO TO 1000

 1100 CONTINUE
CD      PRINT *,'Match not found, flat, flong set to 0,0'
      flat = 0.0
      flong = 0.0

      RETURN
      END   
