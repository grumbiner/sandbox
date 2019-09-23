      PROGRAM extract
C     Extract a time series for a given grid point from satellite data.
      IMPLICIT none

      INTEGER nx, ny, north, south
CD      PARAMETER (nx(1) = 224)
CD      PARAMETER (ny(1) = 152)
CD      PARAMETER (nx(2) = 166)
CD      PARAMETER (ny(2) = 158)
      PARAMETER (ny    = 224)
      PARAMETER (nx    = 152)
      PARAMETER (north = 1  )
      PARAMETER (south = 2  )

      INTEGER*1 imag1(ny,nx)

      CHARACTER*60 fname
      INTEGER i, m, n
      INTEGER x, y, start, last, index, loc
      CHARACTER*2 tempor

 9001 FORMAT (A60)
 9002 FORMAT (I3)

      PRINT *,'What do you want to call the history file?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')

      PRINT *,'What is the base name of the month?'
      READ (*,9001) fname
      PRINT *,'What is first day in the month?'
      READ (*,9002) start
      PRINT *,'What is the last day in the month?'
      READ (*,9002) last
      loc = INDEX(fname,' ')
      fname(loc+2:loc+5) = '.n3c'

      PRINT *,'What x coordinate would you like?'
      READ (*,9002) x
      PRINT *,'What y coordinate would you like?'
      READ (*,9002) y

      DO 1000 i = start, last
        IF (i .LT. 10) THEN
          WRITE (tempor(1:1),9004) 0
          WRITE (tempor(2:2),9004) i
          PRINT *,'tempor ',tempor
         ELSE
          WRITE (tempor,9003) i
        ENDIF
 9003   FORMAT (I2)
 9004   FORMAT (I1)       
        fname(loc:loc+1) = tempor
        OPEN (UNIT=10, FORM='BINARY', STATUS='OLD', 
     1        FILE=fname, ERR=2000)
        READ (10) imag1
        CLOSE (UNIT=10)
        PRINT *,'i, imag(x,y)',i,imag1(x,y), imag1(y,x)
        WRITE (12,9009) i, imag1(y,x)
        GO TO 1000
 2000   PRINT *,'some error in opening the file',i
 1000 CONTINUE
 
 9009 FORMAT (I3,I5)

      END
