      PROGRAM space
C     Test out working with ice concentration constraint in space

      IMPLICIT none

      INTEGER nx, ny, itmax
      PARAMETER (nx = 69)
      PARAMETER (ny = 71)
      PARAMETER (itmax = 90)

      REAL a(nx, ny), a0(nx, ny), b(nx, ny), w(nx, ny)
      REAL dlim, beta, dx, dy
      PARAMETER (dx = 5*25.4E3)
      PARAMETER (dy = 5*25.4E3)
      PARAMETER (beta = 0.04*dx**2)
      PARAMETER (dlim = 0.005)

      INTEGER i, j
      REAL dmax
 
      CALL aread(a0, nx, ny, "north")
      CALL findw2(a0, w, nx, ny)
CD      DO 100 j = 1, ny
CD      DO 100 i = 1, nx
CD        a(i,j) = 0.
CD 100  CONTINUE
      CALL afill(a0, a, nx, ny)
  
      dmax = 0.
      i = 0
 2000 CONTINUE
        i = i + 1
        CALL iter(a, a0, w, b, beta, nx, ny, dx, dy, dmax)
CD        PRINT *,i, dmax
        IF (i .LE. itmax .AND. dmax .GT. dlim) GO TO 2000 

      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (ABS(a0(i,j)-1.57) .LE. 0.01) a(i,j) = 1.57
        IF (a(i,j) .LT. 0.15) a(i,j) = 0.0
        WRITE (*,9001) i-1, j-1, a0(i,j), a(i,j), a0(i,j) - a(i,j)
 1000 CONTINUE
 9001 FORMAT (2I3,2F7.2,F8.3)

      STOP
      END
