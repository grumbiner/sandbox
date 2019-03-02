      PROGRAM bstrim
C     Trim up the bsice data file as created by G. Wohl and L. Breaker
C     Three levels of trimming:
C       1) cut the lines which are duplicated
C       2) cut out lines wherein all data values are zero
C       3) cut out lines wherein any data values are zero
C     Robert Grumbine 8 September 1994

      IMPLICIT none

      INTEGER nlin
      
      INTEGER i, day, j
      CHARACTER*60 lines(1000)
      REAL dat(1000,8), x, y

      CHARACTER*60 fname

      PRINT *,'What is the data file name?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
 9001 FORMAT (A60)

      nlin = 0
 1000 CONTINUE
        nlin = nlin + 1
        READ (10,9001,END=2000) lines(nlin)
        GO TO 1000
 2000 CONTINUE
      nlin = nlin - 1
      PRINT *,'Have read in ',nlin,' lines.'

      PRINT *,'What would you like to call the unduplicated file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      j = 1
      WRITE (11,9001) lines(j)
      DO 3000 i = 2, nlin
        IF (lines(i) .EQ. lines(j)) THEN
          j = i
         ELSE
          WRITE (11,9001) lines(i)
          j = i
        ENDIF
 3000 CONTINUE

C     Now set about doing the data trimming.  First must read 
C       in to data array.  Work with the duplicate-deleted
C       file.
      REWIND (11)
      nlin = 0
 4000 CONTINUE
        nlin = nlin + 1
        READ (11,9001,END=4100) lines(nlin)
        GO TO 4000
 4100 CONTINUE
      nlin = nlin - 1
      PRINT *,'Have read in ',nlin,' lines.'

      REWIND (11)
      DO 4200 i = 1, nlin
CD        PRINT *,'i = ',i
        READ (11,9002) day,(dat(i,j),j=2,8)
        dat(i,1) = FLOAT(day)
 4200 CONTINUE
 9002 FORMAT (I3,7F7.3)
 9003 FORMAT (F4.0,7F7.3)

      PRINT *,'What would you like to call the file for any nonzero?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      PRINT *,'What would you like to call the file with only nonzero?'
      READ (*,9001) fname
      OPEN (13, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      PRINT *,'entering 5000 loop'

      DO 5000 i = 1, nlin
        PRINT *,'in 5000 loop, nlin = ',nlin
        x = 1.
        y = 0.
        WRITE (*,9003) (dat(i,j),j=1,8)
        DO 5100 j = 2, 8 
          x = x*dat(i,j)
          y = y+dat(i,j)
 5100   CONTINUE
        IF (x .NE. 0.) THEN
          WRITE (13,9003) (dat(i,j),j=1,8)
        ENDIF
        IF (y .NE. 0.) THEN
          WRITE (12,9003) (dat(i,j),j=1,8)
        ENDIF
 5000 CONTINUE

      STOP
      END
