      PROGRAM itest

      IMPLICIT none

      INTEGER mode, start, size, no, nactual, fdes
      CHARACTER*80 fname

      INCLUDE "clib.inc"
      INCLUDE "locale.inc"
      INTEGER bacio, banio

      INTEGER jret
      INTEGER nx, ny
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)
      REAL conc2(nx, ny), conc(nx, ny)
      INTEGER i, j
  
      mode = BAOPEN_RONLY + BAREAD + BACLOSE
      WRITE (fname, 9001) "testnh"
 9001 FORMAT (A6)

      start = 0
      size  = SIZEOF_INTEGER
      no    = nx * ny
      PRINT *,'fname = ',fname
      jret = banio(mode, start, size, no, nactual, fdes, 
     1                fname, conc)

      PRINT *,'jret, nactual = ',jret, nactual
      IF (jret .LT. 0) THEN
        PRINT *,'failed to work'
        STOP
      ENDIF

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          WRITE (*,9002) i,j,conc(i,j)
 1100   CONTINUE
 1000 CONTINUE
 9002 FORMAT (2I4, 2F6.2)
    
      END
