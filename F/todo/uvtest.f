      PROGRAM uvtest
C     Robert Grumbine 27 Sep 1995

      IMPLICIT none
      
      INTEGER k, nx, ny
      PARAMETER (k = 2)
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      
      REAL delx, f, beta, ahm, pi, href, value
      PARAMETER (delx = 2.0E4)
      PARAMETER (f    = -1.4E-4)
      PARAMETER (beta = 7.0E-12)
      PARAMETER (ahm  = 8.E4)
      PARAMETER (pi   = 3.14159264)
      PARAMETER (href = 500.0)
      PARAMETER (value = 0.2E-6)
      
      REAL psi(nx, ny), ut(nx, ny), vt(nx, ny)
      
      DOUBLE PRECISION a(nx,-k:k), b(nx), w(nx), piv(nx, -k:-1)
      
      REAL we(nx, ny), dx, dy
      INTEGER lx, ly, i, j
      DOUBLE PRECISION gamma, delta
      CHARACTER*60 fname
      
      dx = delx
      dy = delx
      lx = (nx-1)*2
      ly = (ny-1)*2
      DO 1000 i = 0, ny-1
        DO 1010 j = 0, nx-1
          we(i+1,j+1) = VALUE*SIN(2.*pi*i/lx)*SIN(2.*pi*j/ly)
 1010   CONTINUE
 1000 CONTINUE
C     Note that the forcing (b) will be f*we/beta/h

C*************************************************----------++++++++++!!
C     This is the portion where the solution is found.
C     Now initialize the matrices for matsol.  Aside - b not used here
CD      PRINT *,'Start time ',LONG(362)
      DO 2000 j = -k, k
        DO 2010 i = 1, nx
          a(i,j) = 0.D0
 2010   CONTINUE
 2000 CONTINUE
C     Now compute the constants used in filling a
      gamma = -DBLE(ahm)/DBLE(delx)**4
      delta =  DBLE(beta)/2.D0/DBLE(delx)
C     Interior portion of the grid.
      DO 2100 i = 1+k, nx-k
        a(i,-2) =  gamma
        a(i,-1) = -4.D0*gamma-delta
        a(i, 0) =  6.D0*gamma
        a(i, 1) = -4.D0*gamma+delta
        a(i, 2) =  gamma
 2100 CONTINUE
C     Now apply boundary condition at i = 1, stream fn. = 0 and
C       normal grad = 0.  (psi = 0 and no slip.)
      a(1, 0)  =  1.D0*delta
      a(2,-1)  = -1.D0*delta
      a(2, 0)  =  1.D0*delta
C     Apply the same conditions at i = nx
      a(nx, 0)   =  1.D0*delta
      a(nx-1, 0) = -1.D0*delta
      a(nx-1, 1) =  1.D0*delta
C     The multiplication by delta is a trick to get all numbers in array
C       to be of comparable magnitude.  Given the boundary conditions, 
C       the scaling cancels out anyhow.  BG.  7-17-90.
C     Note that must have b(1) = b(nx) = 0.0 also.
C     Now make the de-composition. Piv is used within matsol only
CD      DO 2200 i = 1, nx
CD          WRITE (*,9002) (a(i,j),j=-k,k)
CD 2200 CONTINUE
CD      PAUSE
CD 9002 FORMAT (5D13.6)
      CALL matsol(k, nx, a, b, piv)
CD      PRINT *,'Finished matrix fill and invert',LONG(362)
C     Ready now to start loading up the forcing and retrieving the soln.
C     Print out f*we/beta/h
CD      DO 2900 j = 1, ny
CD        WRITE (*,9003) (we(i,j),i=1,nx)
CD 2900 CONTINUE
CD 9003 FORMAT (7E13.6)
      DO 3000 j = 1, ny
        b(1)    = 0.D0
        b(2)    = 0.D0
        b(nx)   = 0.D0
        b(nx-1) = 0.D0
        DO 3100 i = 3, nx-2
          b(i) = DBLE(we(i,j)*f/href)
 3100   CONTINUE
        CALL BAKSOL(k, nx, a, b, w)
        DO 3200 i = 1, nx
          psi(i,j) = SNGL(b(i))
 3200   CONTINUE
 3000 CONTINUE
CD      PRINT *,'Finished finding psi',LONG(362)
      
C*************************************************----------++++++++++!!
C     Now, given a psi function, compute the velocities.
C     Now should have a complete soln for psi.
C     Compute the velocity field which corresponds.
C     NOW THAT WE HAVE psi, COMPUTE ut, vt.                             MOD03780
      DO 4000 j = 2, ny-1                                               MOD03790
        DO 4010 i = 2, nx-1                                             MOD03800
          ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy                      MOD03810
          vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx                      MOD03820
 4010   CONTINUE                                                        MOD03830
 4000 CONTINUE                                                          MOD03840
      DO 4020 j = 2, ny-1                                               MOD03850
        i = 1                                                           MOD03860
        ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy                        MOD03870
        vt(i,j) =  (psi(i+1,j)-psi(i  ,j))   /dx                        MOD03880
CD        vt(i,j) = 0.0                                                 MOD03890
        i = nx                                                          MOD03900
        ut(i,j) = -(psi(I,j+1)-psi(i,j-1))/2./dy                        MOD03910
        vt(i,j) =  (psi(I  ,j)-psi(i-1,j))   /dx                        MOD03920
 4020 CONTINUE                                                          MOD03930
      DO 4030 i = 2, nx-1                                               MOD03940
        j = 1                                                           MOD03950
        ut(i,J) = -(psi(i,j+1)-psi(i,j))/dy                             MOD03960
        vt(i,J) =  (psi(i+1,j)-psi(i-1,j))/2./dx                        MOD03970
        j = ny                                                          MOD03980
        ut(i,j) = -(psi(i,j)-psi(i,j-1))/dy                             MOD03990
        vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx                        MOD04000
 4030 CONTINUE                                                          MOD04010

CD      DO 5000 j = 1, ny
CD        WRITE (*,9003) (ut(i,j),i=1,nx)
CD        WRITE (*,9003) (vt(i,j),i=1,nx)
CD        WRITE (*,9003) (psi(i,j),i=1,nx)
CD 5000 CONTINUE
 
      PRINT *,'What would you like to call the output file for psi?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What do you want to call the velocity file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) psi
      WRITE (11) ut
      WRITE (11) vt
      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')

 9001 FORMAT (A60)
 
      END      
      PROGRAM uvtest
      IMPLICIT none
      
      INCLUDE grid.inc
      
      REAL delx, f, beta, ahm, pi, href, value
      PARAMETER (delx  =  2.0E4)
      PARAMETER (f     = -1.4E-4)
      PARAMETER (beta  =  7.0E-12)
      PARAMETER (ahm   =  8.0E4)
      PARAMETER (pi    =  3.14159264)
      PARAMETER (href  = 500.0)
      PARAMETER (value =  0.2E-6)
      
      REAL psi(nx, ny), psi2(nx, ny), psi3(nx, ny)
      REAL ut (nx, ny), vt (nx, ny)
      REAL ut2(nx, ny), vt2(nx, ny), vt3(nx, ny)
      REAL delta(nx, ny), delta2(nx, ny)
    
      REAL we(nx, ny), dx, dy
      INTEGER lx, ly, i, j
      CHARACTER*60 fname
      
      dx = delx
      dy = delx
      lx = (nx-1)*2
      ly = (ny-1)*2
      DO 1000 i = 0, ny-1
        DO 1010 j = 0, nx-1
          we(i+1,j+1) = value*SIN(2.*pi*j/ly)
 1010   CONTINUE
 1000 CONTINUE
C*************************************************----------++++++++++!!
CD      PRINT *,'Calling the old scheme ',LONG(362)
      CALL UVTROP(ut2, vt2, psi2, we, href, dx, dy, f, beta, ahm)
CD      PRINT *,'Calling the exact soln ',LONG(362)
      CALL vexact(vt3, we, psi3, href, dx, f, beta, ahm, nx, ny)
CD      PRINT *,'Done with the exact sol',LONG(362)
C*************************************************----------++++++++++!!
 
      PRINT *,'What would you like to call the output file for psi?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What do you want to call the velocity file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) psi2
      WRITE (10) psi3
      WRITE (11) vt2
      WRITE (11) vt3

      PRINT *,'What would you like to call the screen output?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      DO 6000 j = 1, ny
CD        WRITE ( *,9005) vt2(1,j), 
CD     1 (-3.*psi(1,j)+4.*psi(2,j)-psi(3,j))/2./dx, 
CD     2 (-11.*psi(1,j)+18.*psi(2,j)-9.*psi(3,j)+2.*psi(4,j))/6./dx
CD     3  , j
        DO 6100 i = 1, nx/4
          WRITE (*,9005) vt2(i,j), vt3(i,j), j
 6100   CONTINUE
 6000 CONTINUE
C*************************************************----------++++++++++!!
 9005 FORMAT (2(E13.6,4x),I3)
 
      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')

 9001 FORMAT (A60)
 
      END
