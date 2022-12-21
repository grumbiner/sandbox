      PROGRAM transl

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      REAL x(nx, ny)

      CHARACTER*60 fname
      INTEGER i, j, k

      PRINT *, 'What is the file name?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')

      PRINT *,'At which step?'
      READ (*,9003) k

      DO 1000 j = 1, k
        READ (10) i
        READ (10) x
 1000 CONTINUE

      PRINT *,'Output file name?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (11) x

 9001 FORMAT (A60)

 9002 FORMAT (8E9.3)

 9003 FORMAT (I4)
      END
