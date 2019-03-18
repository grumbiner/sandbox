      INTEGER FUNCTION delymdh(yy, mm, dd, hh, yyold,
     1              mmold, ddold, hhold)
C     Find the time(hrs) between two observations given their ymdh times.
C     Bob Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER yy, dd, mm, hh, yyold, ddold, hhold, mmold
      REAL time
      
        IF (yy .EQ. yyold .AND.
     1          mm .EQ. mmold .AND. dd .EQ. ddold    )
     2    THEN
C         Two obs within the same day
          time =  hh - hhold
        ELSE IF (dd .NE. ddold .AND. (yy .EQ. yyold .AND.
     1          mm .EQ. mmold)  ) THEN
          time = 24*(dd - ddold) + hh - hhold
        ELSE IF (mm .NE. mmold .AND. yy .EQ. yyold) THEN
          IF ( (mm - mmold) .GT. 1) THEN
C           Not consecutive months.  Problem.
            time = 0.0
            PRINT *,' dating problem , months'
           ELSE
            time =  24*dd + hh - hhold
          ENDIF
        ELSE IF (yy .NE. yyold) THEN
          IF ( yy-yyold .GT. 1) THEN
            time = 0.0
            PRINT *,' dating problem, years '
           ELSE
            time =  24*dd + hh - hhold
          ENDIF
        ENDIF
 3200 CONTINUE

      delymdh = time

      RETURN
      END
