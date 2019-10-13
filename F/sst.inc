      REAL ilatmin, ilonmin, idlat, idlon, ilatmax, ilonmax
      PARAMETER (idlat = 1./8.)
      PARAMETER (idlon = 1./8.)
      PARAMETER (ilonmin = -95.0)
      PARAMETER (ilatmin = 35.0)
      PARAMETER (ilatmax = 65.0)
      PARAMETER (ilonmax = -45.0)
      INTEGER innx, inny
      PARAMETER (inny = (ilatmax-ilatmin)/idlat + 0.5 + 1.0)
      PARAMETER (innx = (ilonmax-ilonmin)/idlon + 0.5 + 1.0)
 

