      PROGRAM pinterp
C     Interpolate through time to patch ssmi records on 0.5 degree grid.
C     Bob Grumbine 2 January 1996

      INTEGER nx, ny
      PARAMETER (nx = 720)
      PARAMETER (ny = 360)
      REAL a1(nx, ny), a2(nx, ny)
      CHARACTER*1 interp(nx, ny)
      INTEGER i, j

      READ (10) a1
      READ (11) a2
      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          interp(i, j) = CHAR( INT ( 0.5 + 100.*(a1(i,j) + 
     1                                           a2(i,j))/2 ) ) 
CD          WRITE (*,9001) i, j, ICHAR( interp(i,j) )
 1100   CONTINUE
 1000 CONTINUE
      WRITE (12) interp

 9001 FORMAT (2I4, I4)
 
      STOP
      END
