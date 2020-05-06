C**************************************************************************
C                                                                         *
C            THREE-LAYER SEA ICE VERTICAL THERMODYNAMICS                  *
C                                                                         *
C Based on:  M. Winton, "A reformulated three-layer sea ice model",       *
C submitted to Journal of Atmospheric and Oceanic Technology, 12/98       *
C                                                                         *
C                                                                         *
C        -> +---------+ <- ts - diagnostic surface temperature ( <= 0C )  *
C       /   |         |                                                   *
C     hs    |  snow   | <- 0-heat capacity snow layer                     *
C       \   |         |                                                   *
C        => +---------+                                                   *
C       /   |         |                                                   *
C      /    |         | <- t1 - upper 1/2 ice temperature; this layer has *
C     /     |         |         a variable (T/S dependent) heat capacity  *
C   hi      |...ice...|                                                   *
C     \     |         |                                                   *
C      \    |         | <- t2 - lower 1/2 ice temp. (fixed heat capacity) *
C       \   |         |                                                   *
C        -> +---------+ <- base of ice fixed at seawater freezing temp.   *
C                                                                         *
C                                                                         *
C Contact => Mike Winton, GFDL/NOAA, P.O. Box 308, Princeton, NJ 08542    *
C            e-mail:  mw@gfdl.gov                                         *
C            web:     http://www.gfdl.gov/~mw/                            *
C                                                                         *
C**************************************************************************

      subroutine ice3lay(hs, hi, t1, t2, ts, hf, sol, hfd, fb,
     &                   snow, dt, tmelt, bmelt)
      implicit none
C ***************************************************************
C                                                               *
C    Argument    Description            Units       Changed?    *
C    --------    -----------            -----       --------    *
C                                                               *
C      hs       Snow Thickness           m               yes    *
C      hi       Ice Thickness            m               yes    *
C      ts       Surface Temperature      deg C           yes    *
C     (note:  ts must be input, and will be updated)            *
C      t1       Temp @ Midpt of Ice1     deg C           yes    *
C      t2       Temp @ Midpt of Ice2     deg C           yes    *
C      hf       Net non-solar and upIR                          *
C               heat flux @ surface      watt/m^2        no     *
C      sol      Net solar incoming top   watt/m^2        no     *
C      hfd      Heat flux derivat @ sfc  watt/(m^2deg-C) no     *
C      fb       Heat Flux from Ocean     watt/m^2        no     *
C      snow     Snowfall Rate            m/sec           no     *
C      dt       timestep                 sec             no     *
C      tmelt    top melt during dt       m               yes    *
C      bmelt    top melt during dt       m               yes    *
C                                                               *
C NOTE:  This interface has been kept as similar as possible to *
C        that of the Semtner three-layer model available from   *
C        NCAR (http://www.cgd.ucar.edu:80/ccr/bettge/ice/)      *
C ***************************************************************
C
      real hs, hi, ts, t1, t2, hf, sol, hfd, fb, snow, dt, tmelt, bmelt
C
C********************************************************
C properties of ice, snow, and seawater (like NCAR CSM) *
C KS - conductivity of snow - 0.31 W/(mK)               *
C DS - density of snow - 330 kg/(m^3)                   *
C I0 - ice surface penetrating solar fraction           *
C KI - conductivity of ice  - 2.03 W/(mK)               *
C DI - density of ice  - 905 kg/(m^3)                   *
C CI - heat capacity of fresh ice - 2100 J/(kg K)       *
C LI - latent heat of fusion - 334e3 J/(kg-ice)         *
C SI - salinity of sea ice                              *
C MU - relates freezing temp. to salinity               *
C TFI - sea ice freezing temp. = -mu*salinity           *
C TFW - seawater freezing temperature -1.8 C            *
C********************************************************
C
      real KS, DS, I0, KI, DI, CI, LI, SI, MU, TFI, TFW
C
C variables for temperature calculation [see Winton (1998) section II.A.]
C
      real A, B, I
      real TSF
      real K12, K32
      real A10, B10, A1, B1, C1
C
C variables for ice resizing [see Winton (1998) section II.B.]
C
      real h1, h2, dh, f1

      KS = 0.31
      DS = 330.0
      I0 = 0.3
      KI = 2.03
      DI = 905.0
      CI = 21e2
      LI = 334e3
      SI = 1.0
      MU = 0.054
      TFI = -MU*SI
      TFW = -1.8

      if (hs .gt. 0.0) then
         tsf = 0.0
         I = 0.0
      else
         tsf = TFI
         I = -I0*sol
      endif
C
C Compute ice temperature
C
      A = hf+sol+I
      B = hfd
      K12 = 4*KI*KS/(KS*hi+4*KI*hs)
      K32 = 2*KI/hi

      A10 = DI*hi*CI/(2*dt) + K32*(4*dt*K32+DI*hi*CI)
     &                       /(6*dt*K32+DI*hi*CI)
      B10 = -DI*hi*(CI*t1+LI*TFI/t1)/(2*dt) - I
     &          -K32*(4*dt*K32*TFW+DI*hi*CI*t2)
     &              /(6*dt*K32+DI*hi*CI)

      A1 = A10+K12*B/(K12+B)
      B1 = B10+A*K12/(K12+B)
      C1  = DI*hi*LI*TFI/(2*dt)
      t1 = -(sqrt(B1*B1-4*A1*C1)+B1)/(2*A1)
      ts = (K12*t1-A)/(K12+B)
      
      if (ts .gt. tsf) then
         A1 = A10+K12
         B1 = B10-K12*tsf
         t1 = -(sqrt(B1*B1-4*A1*C1)+B1)/(2*A1)
         ts = tsf
         tmelt = (K12*(t1-tsf)-(A+B*tsf))*dt
      else
         tmelt = 0.0
         hs = hs + snow*dt
      endif

      t2 = (2*dt*K32*(t1+2*TFW)+DI*hi*CI*t2)
     &     /(6*dt*K32+DI*hi*CI)

      bmelt = (fb+4*KI*(t2-TFW)/hi)*dt
C
C Resize the ice ...
C
      h1 = hi/2
      h2 = hi/2
C
C ... top ...
C
      if (tmelt .le. hs*DS*LI) then
         hs = hs - tmelt/(DS*LI)
      else
         h1 = h1 - (tmelt-hs*DS*LI)/(DI*(CI-LI/t1)*(TFI-t1))
         hs = 0.0
      endif
C
C ... and bottom
C
      if (bmelt .lt. 0.0) then
         dh = -bmelt/(DI*(LI+CI*(TFI-TFW)))
         t2 = (h2*t2+dh*TFW)/(h2+dh)
         h2 = h2+dh
      else
         h2 = h2-bmelt/(DI*(LI+CI*(TFI-t2)))
      endif
C
C if ice remains, even up 2 layers, else, pass negative energy back in snow
C
      hi = h1 + h2

      if (hi .gt. 0.0) then
         if (h1 .gt. hi/2) then
            f1 = 1-2*h2/hi
            t2 = f1*(t1+LI*TFI/(CI*t1))+(1-f1)*t2
         else
            f1 = 2*h1/hi
            t1 = f1*(t1+LI*TFI/(CI*t1))+(1-f1)*t2
            t1 = (t1-sqrt(t1*t1-4*TFI*LI/CI))/2
         endif
      else
         hs = hs + (h1*(CI*(t1-TFI)-LI*(1-TFI/t1))
     &             +h2*(CI*(t2-TFI)-LI))/LI
         hi = 0.0
         t1 = TFW
         t2 = TFW
      endif
      return
      end

