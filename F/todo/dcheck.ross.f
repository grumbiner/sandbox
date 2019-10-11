C***********************************************************----------!!
      PROGRAM dcheck
C     Program to check the self-consistency of the data, and write
C       out selected portions for graphical, and other analysis.
C     Robert Grumbine 27 Sep 1995

      IMPLICIT none

C     Declare data matrices:
      INTEGER uct(9200), day(9200), month(9200), year(9200)
      REAL speed(9200), uvel(9200), vvel(9200)
      REAL temp(9200), press(9200), cond(9200)
      INTEGER dir(9200)

C     Declare local variables:
      INTEGER unit, n
      LOGICAL yes
      REAL q1(9200) 

C     Put some of the arrays into extended storage so that they won't
C       cause a stack overflow.
CHP$EMA  uct, day, month, year, q1 

C***********************************************************----------!!

C     Read in the data:
      PRINT *,'How many data points are there?'
      READ (*,9009) n
 9009 FORMAT (I6)

      CALL rin(uct, day, month, year, speed, dir, uvel, vvel, 
     1          temp, press, cond, n)

C***********************************************************----------!!
C     Ask about doing various analyses.
      unit = 12
      PRINT *,'Would you like to write out (u*u+v*v)**.5 ?'
      IF (yes(.FALSE.)) CALL pythag(n, uvel, vvel, unit)

      PRINT *,'Would you like to write out speed*(cos(dir),sin(dir)) ?'
      IF (yes(.FALSE.)) CALL velcmp(n, speed, dir, unit)  

      PRINT *,'Would you like to write out the speed?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (n, speed, unit)
        unit = unit + 1
      ENDIF

      PRINT *,'Would you like to write out u?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (n, uvel, unit) 
        unit = unit + 1
      ENDIF

      PRINT *,'Would you like to write out v?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (n, vvel, unit)
        unit = unit + 1
      ENDIF

      PRINT *,'Would you like to write out temperature?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (n, temp, unit)
        unit = unit + 1
      ENDIF

      PRINT *,'Would you like to write out the pressure?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (n, press, unit)
        unit = unit + 1
      ENDIF

C     PRINT *,'Would you like to write out the conductivity?'
C     IF (yes(.FALSE.)) THEN
C       CALL ritout (n, cond, unit)
C       unit = unit + 1
C     ENDIF

      PRINT *,'Would you like to write out the direction?'
      IF (yes(.FALSE.)) THEN
        DO 1000 i = 1, n
          q1(i) = FLOAT(dir(i))
 1000   CONTINUE
        CALL ritout (n, q1, unit)
        unit = unit + 1
      ENDIF

      END
C***********************************************************----------!!
