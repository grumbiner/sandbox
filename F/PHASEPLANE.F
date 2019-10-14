      PROGRAM pplane
C     Program to find phase plane of the flux file from cshelf model.

      INTEGER jdp, nx, ny
      PARAMETER (jdp = 9504)
      PARAMETER (nx  = 20  )
      PARAMETER (ny  = 18  )

      INTEGER i, j, n
      REAL x(jdp), y(jdp), z(jdp, nx)
      CHARACTER*60 fname
      LOGICAL yes

C     Read in the number of points to find the phase plane for.
      PRINT *,'How many data points are there?'      
      READ (*,9003) n 

      PRINT *,'What is the flux file name?'
      READ (*,9004) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      DO 100 i = 1, n
        READ (10) (z(i,j),j=1,nx)
  100 CONTINUE

  101 CONTINUE
      PRINT *,'Which row do you want to analyze?'
      READ (*,9003) j
      DO 1000 i = 1, n-1
        x(i) = (z(i,j) + z(i+1,j))/2.
        y(i) = z(i+1,j) - z(i,j)
 1000 CONTINUE

      PRINT *,'Do you want the output unformatted?'
      IF (yes(.TRUE.)) THEN
        PRINT *,'What is the name of the flux file?'
        CALL ritout(x, n-1, 11)
        PRINT *,'What is the name of the delta flux file?'
        CALL ritout(y, n-1, 12)

       ELSE
        PRINT *,'What do you want to call the data file?'
        READ (*,9004) fname
        OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
        DO 2000 i = 1, n-1
          WRITE (11,9002) x(i), y(i)
 2000   CONTINUE
        CLOSE (11, STATUS='KEEP')

      ENDIF

      PRINT *,'Would you like to analyze another row?'
      IF (yes(.FALSE.)) GO TO 101

 9002 FORMAT (2E13.5)
 
 9003 FORMAT (I10)

 9004 FORMAT (A60)

      END
