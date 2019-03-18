      PROGRAM buoyancy
C     model convection over a polynya.
      REAL gee, are, pnot, cp
      PARAMETER (gee  = 9.81)
      PARAMETER (are  = 287.04)
      PARAMETER (pnot = 1.01325E5)
      PARAMETER (cp   = 1004.)
      REAL tref, rhoref, pref
      PARAMETER (tref = 253.)
      
      INTEGER nz, nt
      PARAMETER (nz = 500)
      REAL w(nz), t(nz), rho(nz)
      
      REAL dz, dt
      INTEGER i

C     Output parameters
      REAL thi, rholo, rhohi, wlo, whi
      PARAMETER (thi   =  1.E-0)
      PARAMETER (rholo = -1.E-3)
      PARAMETER (rhohi =  5.E-2)
      PARAMETER (wlo   = -1.0)
      PARAMETER (whi   =  20.0)
      CHARACTER*60 fname
      
      PRINT *,'What is the name of the w output file?'
      READ (*,9001) fname
      OPEN (UNIT = 10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What is the name of the t output file?'
      READ (*,9001) fname
      OPEN (UNIT = 11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What is the name of the rho output file?'
      READ (*,9001) fname
      OPEN (UNIT = 12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What is the name of the screen file?'
      READ (*,9001) fname
      OPEN (UNIT=6, FILE=fname, FORM = 'FORMATTED', STATUS='NEW')
      
      PRINT *,'What is the height interval?'
      READ (*,9002) dz
      PRINT *,'What is the time interval?'
      READ (*,9002) dt
      PRINT *,'How many time steps?'
      READ (*,9003) nt
      
 9001 FORMAT (A60)
 9002 FORMAT (E13.6)
 9003 FORMAT (I5)
 
C     Initialize the arrays
      DO 100 i = 1, nz
        w(i)   = 0.0
        t(i)   = 0.0
        rho(i) = 0.0
  100 CONTINUE
  
C     Set up the iterative loop
      DO 1000 i = 1, nt
        CALL extrap(w, t, rho, nz, dz, dt)
        CALL ncsa(w,     wlo,   whi, nz, 10)
        CALL ncsa(t,    0.0 ,   thi, nz, 11)
        CALL ncsa(rho, rholo, rhohi, nz, 12) 
 1000 CONTINUE
 
      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')
      
      END