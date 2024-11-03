      INTEGER L, M, LM, MM, LP, MP, LMDP, LDO, LM2, MM2 
      INTEGER divisor
      PARAMETER (divisor = 1 )
      PARAMETER (L = 385/divisor - 1 )
      PARAMETER (M = 465/divisor - 1 )
      PARAMETER (LM = L-1)
      PARAMETER (MM = M-1)
      PARAMETER (LP = L+1)
      PARAMETER (MP = M+1)
      PARAMETER (LMDP = LP*MP)
      PARAMETER (MM2  = M-2  )
      PARAMETER (LM2  = L-2  )
      PARAMETER (LDO  = 32*(L+M) )

      REAL LATMIN, DLAT, DLON, DXDEG, DX, DY, DT
      INTEGER PTYPE, proj
      REAL polei, polej

      PARAMETER (LATMIN = +81.25)
      PARAMETER (DLAT   =   2.5 )
      PARAMETER (DLON   =   5.0 )
      PARAMETER (PTYPE  = 3     )
      PARAMETER (proj   = 200   )   !Base projection for remapping
      PARAMETER (POLEI  = 190./divisor )
      PARAMETER (POLEJ  = 230./divisor )
      PARAMETER (DXDEG  = 111.1E+3)
      PARAMETER (dx     = 25.4E3*divisor)
      PARAMETER (dy     = 25.4E3*divisor)
      PARAMETER (dt     = 3600.*12.)
      INTEGER NLEVEL
      PARAMETER (NLEVEL = 7)
C     Make mixed layer reference depth a regional parameter
      REAL HMLREF
      PARAMETER (HMLREF = 75.)

      REAL sgn, rearth, eccen2, slat, slon, xorig, yorig
      PARAMETER (sgn    =    1.0)
      PARAMETER (rearth = 6378.273E3)
      PARAMETER (eccen2 =    0.006693883)
      PARAMETER (slat   =   60.0)
      PARAMETER (slon   =  -10.0)
      PARAMETER (xorig  = -(polei)*dx)
      PARAMETER (yorig  = -(polej)*dy)