C Author: Bob Grumbine
C LAST MODIFIED: 25 November 1994.

      REAL PSTAR, ECCEN, ECM2, CSTAR, ZMAX, XMIN, GMIN
      REAL MMAX, VRMAX, WT, ZMIN
      PARAMETER (PSTAR = 5.0E3) !5E5 for fresh water, 5E3 for salt
      PARAMETER (ECCEN = 2.0)
      PARAMETER (ECM2  = 1./ECCEN/ECCEN)
      PARAMETER (CSTAR = 20.)
      PARAMETER (ZMAX  = 2.5E8)  !Note that this multiplies P.
      PARAMETER (ZMIN  = 0.0E8)  !While this is the actual value.
      PARAMETER (GMIN  = 1.E-20)
      PARAMETER (MMAX  = 20   )   !Maximum iterations in relaxation
      PARAMETER (VRMAX = 0.5E-2)  !max delta-V
      PARAMETER (WT    = 1.1  )   !Weight in SOR method 
