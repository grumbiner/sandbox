       subroutine genqsat(t,qsat,ndata,p)
c
c abstract: obtain saturation specific humidity for given temperature.
c
c --> t:      input temperatures
c <-- qsat:   output saturation specific humidities
c --> ndata:  number of temperatures
c --> p:      pressure (mb) of each temperature
c
c *** Suggested modifications by B. Ferrier on 28 June 2000
c
      parameter (QLIMIT=1.E-20, RD=287., RV=461.5, T0C=273.16, 
     &           EPS=RD/RV, EPS1=EPS-1., T15C=T0C-15.)
c--------
      dimension t(ndata),qsat(ndata),p(ndata)
c--------
      qsat=0.
      do i=1,ndata
        if(t(i).lt.1.e19) then
          pint=amax1 (25., amin1 (1050.,p(i)))      ! 25 mb <= p <= 1050 mb
          tint=amax1 (180., amin1 (330., t(i) ) )   ! 180 K <= T <= 330 K
c-----------------------------------------------------------------------
c------- Function FPVS returns the saturation vapor pressure in --------
c------- kilo Pascals, which is w/r/t water (T>=0C) or ice (T<0C) ------
c-----------------------------------------------------------------------
C         esat=10.*FPVS(tint)                      ! Units in mb
C         qsat(i)=amax1(eps*esat/(pint+eps1*esat), qlimit)   ! Units of kg/kg
c-----------------------------------------------------------------------
c------- Optional calculation of linearly varying saturation vapor -----
c------- pressure between 0 C (water) and -15 C (ice).  Not used   -----
c------- (Function FPVS returns the sat vapor pressure w/r/t water) ----
c-----------------------------------------------------------------------
           if (tint.lt.t0c .and. tint.ge.t15c) then
             esati=10.*FPVS(tint)      ! Sat vapor pressure w/r/t ice (mb)
             esatw=10.*FPVS0(tint)     ! Sat vapor pressure w/r/t water (mb)
             weight=amax1 (0., amin1 (1., (t0c-tint)/15. ) )
             esat=weight*esati+(1.-weight)*esatw
             qsat(i)=amax1(eps*esat/(pint+eps1*esat), qlimit)   ! Units of kg/kg
           else
             esat=10.*FPVS(tint)                      ! Units in mb
             qsat(i)=amax1(eps*esat/(pint+eps1*esat), qlimit)   ! Units of kg/kg
           end if
        endif
      end do
      return
      end
