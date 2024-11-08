      PROGRAM space
C     Test out working with ice concentration constraint in space

      IMPLICIT none

      INTEGER nx, ny, itmax
      PARAMETER (nx = 5*77)
      PARAMETER (ny = 5*93)
      PARAMETER (itmax = 90)

      REAL a(nx, ny), a0(nx, ny), b(nx, ny), w(nx, ny)
      REAL dlim, beta, dx, dy
      PARAMETER (dx = 5*25.4E3)
      PARAMETER (dy = 5*25.4E3)
      PARAMETER (beta = 0.04*dx**2)
      PARAMETER (dlim = 0.005)
      CHARACTER*1 cout(nx, ny)

      INTEGER i, j
      REAL dmax

CD      PRINT *,'calling readin' 
      CALL readin(a0, nx, ny)
CD      PRINT *,'returned from readin'
      CALL findw(a0, w, nx, ny)
CD      PRINT *,'returned from findw'
      DO 100 j = 1, ny
      DO 100 i = 1, nx
        a(i,j) = 0.
 100  CONTINUE
  
      dmax = 0.
      i = 0
 2000 CONTINUE
        i = i + 1
        CALL iter(a, a0, w, b, beta, nx, ny, dx, dy, dmax)
        PRINT *,i, dmax
        IF (i .LE. itmax .AND. dmax .GT. dlim) GO TO 2000 

      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (a0(i,j) .EQ. 1.57) a(i,j) = 1.57
        IF (a(i,j) .LT. 0.15) a(i,j) = 0.0
CD        WRITE (*,9001) i, j, a0(i,j), a(i,j), a0(i,j) - a(i,j)
        cout(i,j) = CHAR( INT( 100.*a(i,j) + 0.5) ) 
 1000 CONTINUE
 9001 FORMAT (2I3,2F7.2,F8.3)
      WRITE (11) cout

      STOP
      END
