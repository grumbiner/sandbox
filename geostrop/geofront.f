      PROGRAM geofront
C     Test front end for calling irgeos
C     LAST MODIFIED 2 November 1992

      INTEGER idim, jdim
      PARAMETER (idim = 384)
      PARAMETER (jdim = 190)
      INTEGER nwave 
      PARAMETER (nwave = 126)
    
      REAL pswv(2*(nwave+1)*(nwave+2)), temp(2*(nwave+1)*(nwave+2))
      REAL height(2*(nwave+1)*(nwave+2))
      REAL ug(idim, jdim), vg(idim, jdim)

      INTEGER i, j
      REAL p0, t0, h0
      PARAMETER (p0 = 20.)
      PARAMETER (t0 = 260.)
      PARAMETER (h0 = 0.0)

      DO 1010 i = 1, (nwave+1)*(nwave+2)*2
          temp(i) = 0.0
          height(i) = 0.0
          pswv(i) = 0.0
 1010   CONTINUE
      temp(1) = t0
      height(1) = h0
      pswv(1) = LOG(101.325)
      pswv(1135) = LOG(p0)
      pswv(1136) = LOG(p0)

      CALL irgeos(pswv, temp, height, ug, vg)
      PRINT *,'ug, vg', ug, vg

      END
