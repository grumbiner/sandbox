      SUBROUTINE starup
 
      INTEGER i, unit, num
      CHARACTER*60 fname
 
      SAVE num
 
      PRINT *,'How many data files will you be graphing?'
      READ (*,9003) num
 
C     Open all data files, use long pathnames.
      unit = 10
      DO 1000 i = 1, num
 
        PRINT *,'What is the file name?'
        READ (*,9004) fname
        OPEN (unit, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        unit = unit + 1
 
 1000 CONTINUE
 
C     Initialize the graphics.
      CALL gropen
 
C     Now everything is ready for graphics.
 
 9003 FORMAT (I2)
 
 9004 FORMAT (A60)
 
      RETURN
 
C***********************************************************----------!!
      ENTRY endit
 
C     Close all files
      DO 2000 unit = 10, 9+num
        CLOSE (unit, STATUS='KEEP')
 2000 CONTINUE
 
C     Close down the graphics
      CALL grclos
 
      RETURN
      END
