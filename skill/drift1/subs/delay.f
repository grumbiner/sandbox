      INTEGER FUNCTION delay1(date, dateold)
C     Find the time between two observations
C     Work from a single date/time argument.
C     Bob Grumbine 30 March 1995.

      IMPLICIT none
      INTEGER date, dateold

      INTEGER yy, dd, mm, hh, yyold, ddold, hhold, mmold
      INTEGER dely, delm, deld, delh
      INTEGER i, index, daysin(12)
      REAL time
      CHARACTER*8 tmp

 9009 FORMAT (I8)
 9008 FORMAT (I2, I2, I2, I2)
      WRITE (tmp, 9009) date
      READ (tmp, 9008) yy, mm, dd, hh
      WRITE (tmp, 9009) dateold
      READ (tmp, 9008) yyold, mmold, ddold, hhold

      daysin(1) = 31
      daysin(2) = 28
      daysin(3) = 31
      daysin(4) = 30
      daysin(5) = 31
      daysin(6) = 30
      daysin(7) = 31
      daysin(8) = 31
      daysin(9) = 30
      daysin(10) = 31
      daysin(11) = 30
      daysin(12) = 31
      IF ( MOD(yy, 4) .EQ. 0) THEN
        daysin(2) = 29
      ENDIF

      delh = hh - hhold
      deld = dd - ddold
      delm = mm - mmold
      dely = yy - yyold
      IF (delh .LT. 0) THEN
        deld = deld - 1
        delh = delh + 24
      ENDIF
      IF (deld .LT. 0) THEN
        delm = delm - 1
        deld = deld + daysin(mmold)
      ENDIF
      IF (delm .LT. 0) THEN
        dely = dely - 1
        delm = delm + 12
      ENDIF
      IF (dely .LT. 0) THEN
        PRINT *,'error in years, supposed to be forward in time'
        delh = -delh
        deld = -deld
        delm = 0
      ENDIF

      time = delh + 24 * deld
      IF (delm .GT. 0) THEN
        DO 1000 i = 1, delm
          index = MOD(mmold+i, 12)
          time = time + 24 * daysin(index)
 1000   CONTINUE
      ENDIF


      delay1 = time

      RETURN
      END
