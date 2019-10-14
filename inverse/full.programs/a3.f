      PROGRAM space
C     Test out working with ice concentration constraint in space

      IMPLICIT none

      INTEGER nx, ny, itmax
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)
      PARAMETER (itmax = 60)

      REAL a(nx, ny), a0(nx, ny), b(nx, ny), w(nx, ny)
      REAL dlim, beta, dx, dy
      PARAMETER (dx = 5*25.4E3)
      PARAMETER (dy = 5*25.4E3)
      PARAMETER (beta = 0.04*dx**2)
      PARAMETER (dlim = 0.01)

      INTEGER i, j
      REAL dmax
 
      CALL readin(a0, nx, ny)
      CALL findw(a0, w, nx, ny)
      DO 100 j = 1, ny
      DO 100 i = 1, nx
        a(i,j) = 0.
 100  CONTINUE
  

C     Now get first
      dmax = 0.
      i = 0
 2000 CONTINUE
        i = i + 1
        CALL iter(a, a0, w, b, beta, nx, ny, dx, dy, dmax)
        PRINT *,i, dmax
        IF (i .LE. itmax .AND. dmax .GT. dlim) GO TO 2000 

      DO 1000 j = 2, ny-1
      DO 1000 i = 2, nx-1
        IF (a0(i,j) .EQ. 1.57) a(i,j) = 1.57
        IF (a(i,j) .LT. 0.15) a(i,j) = 0.0
        WRITE (*,9001) i, j, a0(i,j), a(i,j), a0(i,j) - a(i,j)
 1000 CONTINUE
 9001 FORMAT (2I3,2F7.2,F8.3)

      STOP
      END
C********************************************
      SUBROUTINE iter(a, a0, w, b, beta, nx, ny, dx, dy, dmax)
C     Relaxation solution to beta*lapl(a) = w*(a-a0) 
      INTEGER i, j, nx, ny
      REAL beta, dx, dy
      REAL a(nx, ny), a0(nx, ny), w(nx, ny), b(nx, ny)
      REAL dmax

      dmax = 0.
      DO 1000 j = 2, ny-1
      DO 1000 i = 2, nx-1
        b(i,j) = (a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1) 
     1            + w(i,j)*dx**2/beta*a0(i,j) ) 
     2          / (4+w(i,j)*dx**2/beta )
     3          - a(i,j)
        dmax = AMAX1(dmax, ABS(b(i,j)) )
        a(i,j) = a(i,j) + b(i,j)
 1000 CONTINUE
CD      PRINT *,'dmax = ',dmax

      RETURN
      END

      SUBROUTINE findw(a0, w, nx, ny)
C     Find the weighting function for a0
      INTEGER nx, ny, i, j
      REAL a0(nx,ny), w(nx, ny)
    
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (a0(i,j) .GT. 1.28) THEN
          w(i,j) = 0.0
        ELSE IF (a0(i,j) .GT. 1.00) THEN
CD          a0(i,j) = 1.0
          w(i,j) = 1.0
        ELSE
          w(i,j) = 1.0
        ENDIF
 1000 CONTINUE

      RETURN
      END
C********************************************
      SUBROUTINE readin(a0, nx, ny)
      INTEGER nx, ny, i, j
      CHARACTER*60 fname
      REAL a0(nx, ny)
      
      fname = "beta"
      OPEN (10, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      DO 1000 j = 1, ny
      DO 1001 i = 1, nx
        READ (10,*) d1, d2, a0(d1,d2)
 1001 CONTINUE
 1000 CONTINUE

      RETURN
      END 
C********(*********(*********(*********(*********(*********(*********(!!
      SUBROUTINE laplac(diffus, ss, nx, ny, dx, dy, diffu)
      IMPLICIT none

      INTEGER nx, ny
      REAL diffus(nx, ny), ss(nx, ny)
      REAL dx, dy, diffu

      INTEGER i, j
      REAL difu, value
 
      difu  = diffu/dx/dx

C     Evaluate the diffusion:
C     Laplacean in the central part of the domain.  Centered in 
C       x and y.
      DO 100 j = 2, ny-1
        DO 101 i = 2, nx-1
          diffus(i,j) =
     1    ss(i+1,j) -4.*ss(i,j) +ss(i-1,j)
     3  + ss(i,j+1) +ss(i,j-1)
  101   CONTINUE
  100 CONTINUE

C     Diffusion along walls
      DO 200 j = 1, ny
        diffus(1,j) = 0.0
        diffus(nx,j) = 0.0
 200  CONTINUE
      DO 210 i = 1, nx
        diffus(i,1) = 0.0
        diffus(i,ny) = 0.0
  210 CONTINUE

      DO 2000 j = 1, ny
        DO 2100 i = 1, nx
          diffus(i,j) = diffus(i,j)*difu
 2100   CONTINUE
 2000 CONTINUE


CD      PRINT *,'laplac'

      RETURN
      END
