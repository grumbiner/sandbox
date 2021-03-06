      INTEGER L, M, LM, MM, LP, MP, LMDP, LDO, LM2, MM2 
      PARAMETER (L = 68)
      PARAMETER (M = 76)

      PARAMETER (LM = L-1)
      PARAMETER (MM = M-1)
      PARAMETER (LP = L+1)
      PARAMETER (MP = M+1)
      PARAMETER (LMDP = LP*MP)
      PARAMETER (MM2  = M-2  )
      PARAMETER (LM2  = L-2  )
      PARAMETER (LDO  = 16*(L+M) )

      REAL LATMIN, DLAT, DLON, DXDEG, DX, DY, DT
      INTEGER PTYPE
      REAL POLEI, POLEJ
      PARAMETER (LATMIN = +81.25)
      PARAMETER (DLAT   =   2.5 )
      PARAMETER (DLON   =   5.0 )
      PARAMETER (PTYPE  = 3     )
      PARAMETER (POLEI  = 37.   )
      PARAMETER (POLEJ  = 44.   )
      PARAMETER (DXDEG  = 111.1E+3)
      PARAMETER (DX     = 127.0E+3)
      PARAMETER (DY     = 127.0E+3)
      PARAMETER (DT     = 4.32E4  )
      INTEGER NLEVEL
      PARAMETER (NLEVEL = 7)
      
      REAL sgn, rearth, eccen2, slat, xorig, yorig
      PARAMETER (sgn    =    1.0)
      PARAMETER (rearth = 6378.273E3)
      PARAMETER (eccen2 =    0.006693883)
      PARAMETER (slat   =   60.0)
      PARAMETER (xorig  = polei*dx)
      PARAMETER (yorig  = polej*dy)
