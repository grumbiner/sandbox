subroutine emiss(knchpf,kprof,kchan,kochan,wsp10,zasat,zlsat,&
     isflg,lndsea,emissav,pems5,ts5,soiltype5,soilt5,soilm5, &
     vegtype5,vegf5,snow5,polar,frq,mchannel,itype,nsdata, &
     jppf,jpch,jpchus)
!                .      .    .                                       .
! subprogram:    emiss       compute emissivity for IR and microwave 
!   prgmmr: treadon          org: w/nmc20    date: 98-02-20
!
! abstract: compute surface emissivity for IR and microwave channels.
!
! program history log:
!   98-02-20  treadon - gather all emissivity calculations from 
!               setuprad and move into this subroutine.
!
! usage: call emiss(knchpf,kprof,kchan,kochan,wsp10,zasat,
!                   isflg,lndsea,emissav,nsdata)
!
!   input argument list:
!     knchpf   - total number of profiles (obs) times number of 
!                channels for each profile.
!     kprof    - profile number array
!     kchan    - channel number array
!     kochan   - old channel number array
!     wsp10    - 10 meter wind speed at obs location
!     zasat    - local satellite zenith angle in radians
!     zlsat    - satellite look angle in radians
!     isflg    - snow_ice (=1) /no snow_ice (=0) flag at obs
!                location
!     lndsea   - land (=1) /sea (=0) flag at obs location
!     ts5      - skin temperature
!     soiltype5- soil type
!     soilt5   - soil temperature
!     soilm5   - soil moisture   
!     vegtype5 - vegetation type 
!     vegf5    - vegetation fraction
!     snow5    - snow depth            
!     polar    - polarization          
!     frq      - frequency             
!     mchannel - index for IR surface emissivity
!     itype    - ir/microwave instrument type    
!     nsdata   - number of profiles              
!     jppf     - maximum number of profiles
!     jpch     - maximum number of channels
!     jpch     - maximum number of channels used at one time
!
!   output argument list:
!     emissav  - same as pems5 but for diagnostic array
!     pems5    - surface emissivity at obs location
!
!     NOTE:  pems5 is passed through include file "prfvark3.h"
!
! attributes:
!   language: cft77
!   machine:  cray c90
!
!$$$
!
!  Declaration of passed variables.

!     polar: channel polarization (0=vertical, 1=horizontal, or
!                                  0 to 1=mix of V and H)
!     itype:  0=IR channel, 1=microwave channel

   integer,dimension(jpchus*nsdata):: kprof,kchan,kochan
   integer,dimension(nsdata):: isflg,lndsea
   integer,dimension(jpch):: mchannel,itype
   real,dimension(nsdata):: wsp10,zasat,zlsat
   real,dimension(jpchus,jppf):: emissav
   real,dimension(jppf*jpchus):: pems5
   real,dimension(jppf):: ts5,snow5,soiltype5,soilt5,soilm5,vegtype5,vegf5
   real,dimension(jpch):: frq,polar

! reshape allows multidimensional arrays

   real,parameter,dimension(3,19):: theta = reshape((/ &
      1700.381,25.28534,144.1023,&
      1738.149,25.67787,146.6139,&
      1769.553,26.05250,148.6586,&
      1778.610,26.12333,149.5127,&
      1794.245,26.18523,150.5874,&
      1791.904,26.19991,150.7076,&
      1806.872,26.37132,151.7191,&
      1926.078,27.63825,160.7103,&
      1969.155,28.02767,163.6069,&
      1975.549,27.86465,164.6228,&
      1991.288,27.94312,166.2924,&
      2082.691,28.93558,172.4025,&
      2182.872,29.89974,179.5839,&
      2338.510,31.27507,191.2063,&
      2164.615,28.97152,182.6279,&
      2099.714,29.91868,178.4015,&
      1857.644,29.13640,160.9822,&
      1610.696,26.48602,142.2768,&
      1503.969,24.97931,133.4392 /),(/3,19/))
   real,parameter,dimension(3,2,19):: cemis = reshape((/ &
      .9715104043561414,-1.2034233230944147e-06,-5.8742655960993913e-07,&
      .9263932848727608,-9.4908630939690859e-04, 2.2831134823358876e-05,&
      .9732503924722753,-1.2007007329295099e-06,-5.8767355551283423e-07,&
      .9290947860585505,-9.5233413988900161e-04, 2.2640835623043761e-05,&
      .9745005204317289, 1.2857517639804244e-06,-7.1356127087301190e-07,&
      .9310852809117095,-9.5453509182819095e-04, 2.2562638663187251e-05,&
      .9756204829761132, 1.2979181109743976e-06,-7.1406809362820139e-07,&
      .9329073568177888,-9.5627536945214183e-04, 2.2442358508999558e-05,&
      .9764012672766408,-2.0826654381361387e-06,-4.9103920569405721e-07,&
      .9341937281933334,-9.5764423928102976e-04, 2.2326701573603621e-05,&
      .9770513558720460, 4.1867599593267133e-07,-6.1768073971231931e-07,&
      .9352981872014672,-9.5833614545300181e-04, 2.2261996883974513e-05,&
      .9775970810179080,-1.2289690625562906e-06,-5.2953762169985775e-07,&
      .9362188153954743,-9.5950872922696905e-04, 2.2251301675423482e-05,&
      .9830610391451819, 2.7693589475690676e-07,-5.1580217018207558e-07,&
      .9461121192685766,-9.5718115604053031e-04, 2.1087308573177295e-05,&
      .9840097930773377,-1.4987900189155091e-06,-3.8281408128977038e-07,&
      .9479758694804105,-9.5451252460440695e-04, 2.0800627740862229e-05,&
      .9851056150728580,-6.5768237152417477e-07,-4.2053769829400935e-07,&
      .9502084544618980,-9.4965534997704157e-04, 2.0326602209199427e-05,&
      .9862706396188835,-2.3713068057993353e-06,-2.8671134918457728e-07,&
      .9526580467595886,-9.4614505430749598e-04, 2.0001856872595840e-05,&
      .9875307221489201, 1.3003462826947714e-07,-4.1335288320283954e-07,&
      .9554195617948236,-9.3806678196435643e-04, 1.9407754748128057e-05,&
      .9891153260567763,-8.0730206675976713e-07,-3.1811320626834656e-07,&
      .9590558393678170,-9.2716287670223167e-04, 1.8690586764925213e-05,&
      .9913304557147524,-2.1153391229093421e-08,-3.1094269595901165e-07,&
      .9644162604969492,-9.0342753739935612e-04, 1.7274993357160937e-05,&
      .9925188366950193,-4.6365959315123653e-07,-2.7020120347068712e-07,&
      .9667877170960085,-9.0665804037922043e-04, 1.7083616616646458e-05,&
      .9919408379810360,-2.0563508815953840e-06,-1.8066722718403761e-07,&
      .9627535343397309,-9.7537134133678965e-04, 1.9698263973541952e-05,&
      .9889406296815972,-2.3713068057993353e-06,-2.8671134918457728e-07,&
      .9506051906192242,-1.0642902225813857e-03, 2.4235485973033298e-05,&
      .9828819693848310,-7.4086701870172759e-07,-6.2949258820534062e-07,&
      .9329616683158125,-1.0728027288012200e-03, 2.7209071863380586e-05,&
      .9767410313266288,-9.1750038410238915e-07,-7.9177921107781349e-07,&
      .9192775350344998,-1.0369254272157462e-03, 2.8000594542037504e-05/), &
      (/3,2,19/))

   real,parameter,dimension(59):: emc = (/&
       .175350E+02, -.617670E+00,  .894800E-02,  .318420E+01,&
       .191890E-01, -.108730E-01,  .258180E-03,  .683960E+02,&
      -.406430E+00,  .228320E-01, -.530610E-03,  .476290E+01,&
       .154100E+00, -.337170E-01,  .844280E-03,  .782870E+02,&
      -.434630E-02,  .531250E+01, -.114770E-01,  .314159E+01,&
      -.100000E+01,  .195000E-04,  .255000E+01, -.637182E+01,&
       .253918E-01,  .357569E-04,  .942928E+01, -.332839E-01,&
      -.647724E-04, -.329282E+01,  .965450E-02,  .281588E-04,&
       .252676E+00,  .343867E-02, -.156362E-04, -.156669E-03,&
       .139485E-04, -.407633E-07, -.141316E+00, -.356556E-02,&
       .142869E-04, -.240701E+01, -.563888E-01,  .325227E-03,&
       .296005E+01,  .704675E-01, -.426440E-03, -.751252E+00,&
      -.191934E-01,  .125937E-03, -.288253E+00, -.102655E-02,&
       .226701E-05, -.119072E-02, -.263165E-04,  .114597E-06,&
       .406300E+00,  .200031E-02, -.781635E-05/)

! Explanation for emc :
! FreqGHz: Observation frequency in GHz
! Angdeg: local zenith angle
! Ci: cosine of local zenith angle
! CiCi: cosine squared of local zenith angle
! SiSi: sine squared of local zenith angle
! emc(38): Emissivity model data
! Permittivity model data (Lamkaouchi model)
!   [1-3]: Temperature polynomial coefficients for Tau1 - Lamkaouchi (1996)
!   [4-7]: Temperature polynomial coefficients for Tau2 - Lamkaouchi (1996)
!  [8-11]: Temperature polynomial coefficients for Del1 - Lamkaouchi (1996)
! [12-15]: Temperature polynomial coefficients for Del2 - Lamkaouchi (1996)
! [16-17]: Temperature polynomial coefficients for static permittivity - Lamkaouchi (1996)
! [18-19]: Temperature polynomial coefficients for infinite freq. permittivity - Lamkaouchi (1996)
! Pi is stored for good measure
!    [20]: Stored value of Pi  - temporary, use RTTOV pi when available.
! Large scale correction model version 1: This does *NOT* correct for
! hemispherical scattering and is *NO LONGER USED*
!    [21]: Angle coefficient for large scale correction - see English (1997)
!    [22]: Windspeed coefficient for large scale correction - see English (1997)
!    [23]: Constant for large scale correction - see English (1997)
!    [24]: Reference frequency for large scale correction - see English (1997)
!    [25]: Normalisation frequency for large scale correction - see English (1997)
!    [26]: Scaling factor for large scale correction - see English (1997)
! Bragg scattering correction coefficients
!    [27]: Scaling factor for small scale correction - see English (1997)
! Foam model coefficients for Monahan model
!    [28]: First coefficient in Monahan foam model (neutral stability)  - see English (1997)
!    [29]: Second coefficient in Monahan foam model (neutral stability) - see English (1997)
! Alternative permittivity model (Liebe)
!    [30]: a1 in Liebe's dielectric model - see Liebe (1989)
!    [31]: b1 in Liebe's dielectric model - see Liebe (1989)
!    [32]: c1 in Liebe's dielectric model - see Liebe (1989)
!    [33]: c2 in Liebe's dielectric model - see Liebe (1989)
!    [34]: d1 in Liebe's dielectric model - see Liebe (1989)
!    [35]: d2 in Liebe's dielectric model - see Liebe (1989)
!    [36]: d3 in Liebe's dielectric model - see Liebe (1989)
!    [37]: e1 in Liebe's dielectric model - see Liebe (1989)
!    [38]: e2 in Liebe's dielectric model - see Liebe (1989)
! Version 2 of large scale correction which *DOES»* take account of
! hemispherical scattering.
! 1.) Mixed polarisation mode (nominal V at nadir)
!    [39]: Term a00 in mixed pol of large scale correction model
!    [40]: Term a01 in mixed pol mode of large scale correction model
!    [41]: Term a02 in mixed pol mode of large scale correction model
!    [42]: Term a10 in mixed pol mode of large scale correction model
!    [43]: Term a11 in mixed pol mode of large scale correction model
!    [44]: Term a12 in mixed pol mode of large scale correction model
!    [45]: Term a20 in mixed pol mode of large scale correction model
!    [46]: Term a21 in mixed pol mode of large scale correction model
!    [47]: Term a22 in mixed pol mode of large scale correction model
!    [48]: Term a30 in mixed pol mode of large scale correction model
!    [49]: Term a31 in mixed pol mode of large scale correction model
!    [50]: Term a32 in mixed pol mode of large scale correction model
!    [51]: Term a40 in mixed pol mode of large scale correction model
!    [52]: Term a41 in mixed pol mode of large scale correction model
!    [53]: Term a42 in mixed pol mode of large scale correction model
! 2.) Vertical polarisation mode
!    [54]: Term a00 in vertical pol mode of large scale correction model
!    [55]: Term a01 in vertical pol mode of large scale correction model
!    [56]: Term a02 in vertical pol mode of large scale correction model
!    [57]: Term a10 in vertical pol mode of large scale correction model
!    [58]: Term a11 in vertical pol mode of large scale correction model
!    [59]: Term a12 in vertical pol mode of large scale correction model
!    [60]: Term a20 in vertical pol mode of large scale correction model
!    [61]: Term a21 in vertical pol mode of large scale correction model
!    [62]: Term a22 in vertical pol mode of large scale correction model
!    [63]: Term a30 in vertical pol mode of large scale correction model
!    [64]: Term a31 in vertical pol mode of large scale correction model
!    [65]: Term a32 in vertical pol mode of large scale correction model
!    [66]: Term a40 in vertical pol mode of large scale correction model
!    [67]: Term a41 in vertical pol mode of large scale correction model
!    [68]: Term a42 in vertical pol mode of large scale correction model
! 3. ) Horizontal polarisation mode
!    [69]: Term a00 in horizontal pol mode of large scale correction model
!    [70]: Term a01 in horizontal pol mode of large scale correction model
!    [71]: Term a02 in horizontal pol mode of large scale correction model
!    [72]: Term a10 in horizontal pol mode of large scale correction model
!    [73]: Term a11 in horizontal pol mode of large scale correction model
!    [74]: Term a12 in horizontal pol mode of large scale correction model
!    [75]: Term a20 in horizontal pol mode of large scale correction model
!    [76]: Term a21 in horizontal pol mode of large scale correction model
!    [77]: Term a22 in horizontal pol mode of large scale correction model
!    [78]: Term a30 in horizontal pol mode of large scale correction model
!    [79]: Term a31 in horizontal pol mode of large scale correction model
!    [80]: Term a32 in horizontal pol mode of large scale correction model
!    [81]: Term a40 in horizontal pol mode of large scale correction model
!    [82]: Term a41 in horizontal pol mode of large scale correction model
!    [83]: Term a42 in horizontal pol mode of large scale correction model
!    [84]: Windspeed coefficient in mixed polarisation high U, theta correction
!    [85]: View angle coefficient in mixed polarisation high U, theta correction
!    [86]: Constant coefficient in mixed polarisation high U, theta correction
!    [87]: Windspeed coefficient in vertical polarisation high U, theta correction
!    [88]: View angle coefficient in vertical polarisation high U, theta correction
!    [89]: Constant coefficient in vertical polarisation high U, theta correction
!    [90]: Windspeed coefficient in horizontal polarisation high U, theta correction
!    [91]: View angle coefficient in horizontal polarisation high U, theta correction
!    [92]: Constant coefficient in horizontal polarisation high U, theta correction

! local variables
!
   complex perm1,perm2,rvth,rhth,xperm
!
!************************************************************************
!  Start emiss here
!
!
!  Set constants
   rad2dg = 180./acos(-1.)
!
!
!  Loop over all profiles and channels.  Compute or set the emissivity 
!  for IR and microwave channels.  We initially set the surface 
!  emissivity to 0.9 for all observations.
!
   do nn = 1,knchpf
     n    = kprof(nn)
     kch  = kchan(nn)
     kcho = kochan(nn)
!
!    Compute emissivity for IR channels.  For IR channels, we
!    use the same formula for all surface types.
     if (itype(kch).eq.0) then
       if(lndsea(n) .eq. 0 .and. isflg(n) .eq. 0)then
        wind  = wsp10(n)
        degre = abs(zasat(n)*rad2dg)
        index = mchannel(kch)
!
        aemis  = cemis(1,1,index) + cemis(2,1,index)*wind&
             + cemis(3,1,index)*wind*wind
        bemis  = cemis(1,2,index) + cemis(2,2,index)*wind&
             + cemis(3,2,index)*wind*wind
        ccemis = theta(1,index) + theta(2,index)*wind
!
        pems5(nn) = aemis + (bemis-aemis) *&
             exp(( (theta(3,index)-60.)**2.&
             - (degre-theta(3,index))**2. )/ccemis)
!
       else
        if(lndsea(n) .eq. 1)then
          if(isflg(n) .eq. 1)then
            pems5(nn) = 1.0
          else
            pems5(nn) = 0.97
          end if
        else
          if(isflg(n) .eq. 1)then
            pems5(nn) = 0.98
          end if
        end if
       end if
!        Set emissivity for microwave channels over snow/ice covered
!        surfaces.  By default, the emissivity for microwave channels 
!        over snow-free land points is set to 1.0.
!
!        The loop following this loop computes emissivities for 
!        microwave channels over ice-free ocean points.
!
     elseif (itype(kch).eq.1) then
      if(lndsea(n) .eq. 1)then
        if(frq(kch) .lt. 80.)then
          call landem(zasat(n),frq(kch),soilm5(n),vegf5(n),vegtype5(n), &
            soiltype5(n),soilt5(n),ts5(n),snow5(n),ehorz,evert)
          pcl2=cos(zlsat(n))**2
          psl2=sin(zlsat(n))**2
          term1 = evert*pcl2 + ehorz*psl2
          term2 = evert*psl2 + ehorz*pcl2
          pems5(nn) = (1.-polar(kch))*term1 + polar(kch)*term2
        else
          if(isflg(n) .eq. 1)then
            pems5(nn) = 0.9
          else
            pems5(nn) = 0.95
          end if
        end if
      else
        if(isflg(n) .eq. 1)then
          pems5(nn) = 0.92
        else

!     We only process microwave channels (itype=1) over ocean points
!     (lndsea=0) which are not covered with ice (isflg=0).
!
!
!     Special treatment for microwave channels over ice-free ocean points
!     First set constants.  Then perform the calculation.
!
         freqghz = frq(kch)
         u10mps = wsp10(n)
         pcc=cos(zasat(n))
         pss=sin(abs(zasat(n)))
         pcl2=cos(zlsat(n))**2
         psl2=sin(zlsat(n))**2
         ps2=pss*pss
         pc2=pcc*pcc
         freqghz2=freqghz*freqghz
         u10mps2=u10mps*u10mps
         sec=1.0/pcc
         sec2=sec*sec
         usec=u10mps*sec

! calculate piom (ellison et al.) xperm
!       to calculate xperm of saline water based on
!       piom model.
! convert from kelvin to centigrate and define quadratic and
! cubic functions for later polynomials
        tc=ts5(n)-273.15
        tcsq=tc*tc
        tccub=tcsq*tc
! define two relaxation frequencies, tau1 and tau2
        tau1=emc(1)+emc(2)*tc+emc(3)*tcsq
        tau2=emc(4)+emc(5)*tc+emc(6)*tcsq+emc(7)*tccub
! static xperm estatic=del1+del2+einf
        del1=emc(8)+emc(9)*tc+emc(10)*tcsq+emc(11)*tccub
        del2=emc(12)+emc(13)*tc+emc(14)*tcsq+emc(15)*tccub
        einf=emc(18)+emc(19)*tc
! calculate xperm using double-debye formula
        fen=2.0*emc(20)*freqghz*0.001
        fen2=fen**2.0
        den1=1.0+fen2*tau1*tau1
        den2=1.0+fen2*tau2*tau2
        perm_real1=del1/den1
        perm_real2=del2/den2
        perm_imag1=del1*fen*tau1/den1
        perm_imag2=del2*fen*tau2/den2
        perm_real=perm_real1+perm_real2+einf
        perm_imag=perm_imag1+perm_imag2
        xperm=cmplx(perm_real,perm_imag)
!
!
! calculate complex fresnel reflection coefficients
!       to calculate vertical and horizontal polarised reflectivities
!       given xperm at local incidencence angle for all channels
!       and profiles

         perm1 = csqrt(xperm - ps2)
         perm2  = xperm*pcc
         rhth = (pcc - perm1)/(pcc + perm1)                     
         rvth = (perm2 - perm1)/(perm2 + perm1)
         rvertsr=dble(rvth)
         rvertsi=aimag(rvth)
         rverts=rvertsr*rvertsr+rvertsi*rvertsi
         rhorzsr=dble(rhth)
         rhorzsi=aimag(rhth)
         rhorzs=rhorzsr*rhorzsr+rhorzsi*rhorzsi
! calculate small scale xcorr to reflection coefficients
! the following lines are commented out because a warning will 
! be printed from dcalmkaouchi if freqghz .lt. 10.
           xcorr1=exp(emc(21)*u10mps*pc2/freqghz2)
! calculate large scale geometric correction
!       to calculate a correction to the fresnel reflection coefficients
!       allowing for the presence of large scale roughness      
!
! jc: six coefficients (constant, u, u^2, sec, sec^2, u*sec)	
         zcv1=emc(24)+emc(25)*freqghz+emc(26)*freqghz2
         zcv2=(emc(27)+emc(28)*freqghz+emc(29)*freqghz2)*sec
         zcv3=(emc(30)+emc(31)*freqghz+emc(32)*freqghz2)*sec2
         zcv4=(emc(33)+emc(34)*freqghz+emc(35)*freqghz2)*u10mps
         zcv5=(emc(36)+emc(37)*freqghz+emc(38)*freqghz2)*u10mps2
         zcv6=(emc(39)+emc(40)*freqghz+emc(41)*freqghz2)*usec
         zch1=emc(42)+emc(43)*freqghz+emc(44)*freqghz2
         zch2=(emc(45)+emc(46)*freqghz+emc(47)*freqghz2)*sec
         zch3=(emc(48)+emc(49)*freqghz+emc(50)*freqghz2)*sec2
         zch4=(emc(51)+emc(52)*freqghz+emc(53)*freqghz2)*u10mps
         zch5=(emc(54)+emc(55)*freqghz+emc(56)*freqghz2)*u10mps2
         zch6=(emc(57)+emc(58)*freqghz+emc(59)*freqghz2)*usec
! calculate correction for this polarisation
         xcorr2v=.01*(zcv1+zcv2+zcv3+zcv4+zcv5+zcv6)
         xcorr2h=.01*(zch1+zch2+zch3+zch4+zch5+zch6)
! calculate foam emissivity correction
         ffoam=emc(22)*(u10mps**emc(23))
         evertr=1.0-rverts*xcorr1+xcorr2v
         ehorzr=1.0-rhorzs*xcorr1+xcorr2h
         evert=evertr - ffoam*evertr+ ffoam
         ehorz=ehorzr - ffoam*ehorzr + ffoam
!     
!
!           Combine horizontal and vertical polarizations.
         term1 = evert*pcl2 + ehorz*psl2
         term2 = evert*psl2 + ehorz*pcl2
         pems5(nn) = (1.-polar(kch))*term1 + polar(kch)*term2
!
        end if
       end if
      endif
!
!     Load emissivity into array for radiative transfer model
!     (pems5) and diagnostic output file (emissav).

      pems5(nn)       = max(0.0,min(pems5(nn),1.0))
      emissav(kcho,n) = pems5(nn)
   end do
!
!
!     End of routine.

   return
   end
