      PROGRAM a
CD      INTEGER nx, ny, nf
CD      PARAMETER (nx = 128) 
CD      PARAMETER (ny = 128)
      INCLUDE "grid.inc"
      INTEGER nf
      REAL x(nx, ny)
      REAL y(nx, ny)
      CHARACTER*60 fname

      PRINT *,'file name?'
      READ (*,9001) fname
 9001 FORMAT (A60)

      nf = 0
      CALL cread(x, y, nf, nx, ny, fname)

CD      PRINT *,x

      OPEN (10, FILE='fread', FORM='UNFORMATTED', STATUS='NEW')
      IF (nf .GE. 1) WRITE (10) x
      IF (nf .GE. 2) WRITE (10) y

      STOP
      END
