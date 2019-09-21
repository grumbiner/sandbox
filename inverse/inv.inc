C     General purpose include file for the inversion tests.
C     Robert Grumbine 30 July 1998

C     Section 1, control of general physical/numerical parameters
C     h0  is the thickness of minimally observable sea ice
C     fref is a reference freezing rage
C     dh   is the ice thickness interval 
C     days is the number of days to run
C     mult is ?
C     cfl  is the cfl number to set dt to
 
      REAL h0, fref, dh, days, mult, cfl, dt
      PARAMETER (h0   = 0.10)
      PARAMETER (dh   = 0.05)
      PARAMETER (fref = 0.10) ! Note, question of whether to use m/day or m/sec
      PARAMETER (days = 10.0)
      PARAMETER (cfl = 0.25 )
      PARAMETER (dt  = 0.125 )
CD      PARAMETER (dt  = dh * cfl / fref )
      PARAMETER (mult  = (dh/cfl) )

C     Section 2, control of memory
      INTEGER nthick, nvars, nstep
      PARAMETER (nthick = fref/dh*5 + 3.5)
      PARAMETER (nvars = 4)
      PARAMETER (nstep = days / (dh/fref*cfl) + 1.5 )

C     Section 3, spatial variations
      INTEGER nx, ny, deflate
      PARAMETER (deflate = 4)
      PARAMETER (nx = 385/deflate)
      PARAMETER (ny = 465/deflate)
      REAL dx, dy
      PARAMETER (dx = 25.4E3*deflate)
      PARAMETER (dy = 25.4E3*deflate)

