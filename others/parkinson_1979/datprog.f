      PROGRAM datprog
C     Create pseudo-data for testing Parkinson '79, '83 model.
C     Bob Grumbine 2/13/92.

      INTEGER nx, ny
      PARAMETER (nx = 42)
      PARAMETER (ny = 31)

      REAL hice(nx, ny), hsno(nx, ny), free(nx, ny), tocn(nx, ny)
      REAL hiref, hsref, fref, toref

      INTEGER i, j
      CHARACTER*60 fname

      PRINT *,'What is the reference air temperature?'
      READ (*,9001) hiref
      PRINT *,'What is the reference dew point?'
      READ (*,9001) hsref
      PRINT *,'What is reference u wind speed?'
      READ (*,9001) fref
      PRINT *,'What is the reference v wind speed?'
      READ (*,9001) toref

      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          hice(i,j) = hiref
          hsno(i,j) = hsref
          free(i,j) = fref
          tocn(i,j) = toref
 1010   CONTINUE
 1000 CONTINUE

      PRINT *,'What is the output file name?'
      READ (*,9002) fname
      OPEN (UNIT=10, FILE=fname, FORM='UNFORMATTED',
     1  STATUS='NEW')
      DO 2000 i = 1, 12
        WRITE (10) hice
        WRITE (10) hsno
        WRITE (10) free
        WRITE (10) tocn
 2000 CONTINUE

      CLOSE (10, STATUS='KEEP')

 9001 FORMAT (E13.6)

 9002 FORMAT (A60)

      END
