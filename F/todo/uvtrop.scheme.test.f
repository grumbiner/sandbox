      PROGRAM uvtest
C     Robert Grumbine 27 Sep 1995

      IMPLICIT none
      
      INCLUDE "grid.inc"
      
      REAL delx, f, beta, ahm, pi, href, value
      PARAMETER (delx  =  2.0E4)
      PARAMETER (f     = -1.4E-4)
      PARAMETER (beta  =  7.0E-12)
      PARAMETER (ahm   =  8.0E4)
      PARAMETER (pi    =  3.14159264)
      PARAMETER (href  = 500.0)
      PARAMETER (value =  0.2E-6)
      
      REAL psi(nx, ny), psi2(nx, ny)
      REAL ut (nx, ny), vt (nx, ny)
      REAL ut2(nx, ny), vt2(nx, ny)
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
C     Now Call the new scheme.
CD      PRINT *,'Calling the new scheme ',LONG(362)
      CALL utrop2(psi, we, delx, ahm, beta, f, href)
      CALL stmfcn(psi, ut, vt, dx, dy, nx, ny)
CD      PRINT *,'Calling the old scheme ',LONG(362)
      CALL UVTROP(ut2, vt2, psi2, we, href, dx, dy, f, beta, ahm)
CD      PRINT *,'Returned from the old scheme ',LONG(362)
C*************************************************----------++++++++++!!
 
      PRINT *,'What would you like to call the output file for psi?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What do you want to call the velocity file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) psi
      WRITE (10) psi2
      WRITE (11) ut
      WRITE (11) vt
      WRITE (11) ut2
      WRITE (11) vt2
      CALL vort(delta,  ut,  vt,  nx, ny, dx, dy)
      CALL vort(delta2, ut2, vt2, nx, ny, dx, dy)

      PRINT *,'What would you like to call the screen output?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      DO 6000 j = 1, ny
        DO 6100 i = 1, nx
CD          IF (vt2(i,j)*ut2(i,j) .NE. 0.0)
CD     1        WRITE (*,9005) ut(i,j)/ut2(i,j), vt(i,j)/vt2(i,j)
            IF (delta2(i,j) .NE. 0.0) 
     1        WRITE (12,9005) delta(i,j)/delta2(i,j), delta2(i,j), j
 6100   CONTINUE
 6000 CONTINUE
 9005 FORMAT (E13.6,4x,E13.6,4x,I3)
 
      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')

 9001 FORMAT (A60)
 
      END
