      PROGRAM levels

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)

      REAL u(nx, ny), v(nx, ny)
      REAL lo(nx, ny), up(nx, ny)
      REAL refer
      INTEGER i, j, tstep
      CHARACTER*60 fname

 9999 CONTINUE

      PRINT *,'What is the name of the mean file?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the deviation file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')

      PRINT *,'What time step do you want?'
      READ (*,9002) tstep
      DO 1000 i = 1, tstep

	READ (10) u
	READ (11) v

 1000 CONTINUE

      PRINT *,'What reference speed do you want?'
      READ (*,9003) refer

      DO 2000 j = 1, ny
	DO 2010 i = 1, nx
	  lo(i,j) = u(i,j)-v(i,j)
	  up(i,j) = u(i,j)+v(i,j)
 2010   CONTINUE
 2000 CONTINUE

      lo(1,1) = refer/SQRT(2.)
      up(1,1) = refer/SQRT(2.)

      PRINT *,'What do you want to call the upper level field?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What do you want to call the lower level field?'
      READ (*,9001) fname
      OPEN (13, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (12) up
      WRITE (13) lo

      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
      CLOSE(13)

      PRINT *,'Would you like to work on another pair?'
      READ (*,9001) fname
      IF (fname(1:1) .NE. 'n') GO TO 9999

 9001 FORMAT (A60)

 9002 FORMAT (I3)

 9003 FORMAT (E13.6)

      END
