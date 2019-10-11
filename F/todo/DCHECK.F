C***********************************************************----------!!
      PROGRAM dcheck
C     Program to check the self-consistency of the data, and write
C       out selected portions for graphical, and other analysis.

C     Declare data matrices:
      INTEGER uct(9200), day(9200), month(9200), year(9200)
      REAL speed(9200), uvel(9200), vvel(9200)
      REAL temp(9200), press(9200), cond(9200)
      INTEGER dir(9200)

C     Declare local variables:
      INTEGER i, n
      LOGICAL yes
      REAL q1(9200) 

C     Put some of the arrays into extended storage so that they won't
C       cause a stack overflow.
$EMA  uct, day, month, year, q1 

C***********************************************************----------!!

C     Read in the data:
      CALL rin(uct, day, month, year, speed, dir, uvel, vvel, 
     1          temp, press, cond, n)


C***********************************************************----------!!
C     Ask about doing various analyses.

      PRINT *,'Would you like to write out u?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (uvel, n, 15) 
        CLOSE( 15, STATUS='KEEP')
      ENDIF

      PRINT *,'Would you like to write out v?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (vvel, n, 15)
        CLOSE( 15, STATUS='KEEP')
      ENDIF

      PRINT *,'Would you like to write out temperature?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (temp, n, 15)
        CLOSE( 15, STATUS='KEEP')
      ENDIF

      PRINT *,'Would you like to write out the pressure?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (press, n, 15)
        CLOSE( 15, STATUS='KEEP')
      ENDIF

      PRINT *,'Would you like to write out the conductivity?'
      IF (yes(.FALSE.)) THEN
        CALL ritout (cond, n, 15)
        CLOSE( 15, STATUS='KEEP')
      ENDIF

      PRINT *,'Would you like to write out the direction?'
      IF (yes(.FALSE.)) THEN
        DO 1000 i = 1, n
          q1(i) = FLOAT(dir(i))
 1000   CONTINUE
        CALL ritout (q1, n, 15)
        CLOSE( 15, STATUS='KEEP')
      ENDIF

      END
C***********************************************************----------!!
