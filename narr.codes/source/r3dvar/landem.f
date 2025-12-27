Subroutine Landem(theta, freq, mv, veg_frac, veg_tp, soil_tp, &
     t_soil, t_skin, snow_depth, esh,esv)


!--------------------------------------------------------------------!
!
!	Program function:
!
!     NOAA/NESDIS emissivity model to compute microwave emissivity over
!       various land surface conditions
!
!   Version: Beta 
!
!   Released Date: November 28, 2000
!
!
!	Reference 
!       Weng, F, B. Yan, and N. Grody, 2001: "Development of Mircowave land emissivity model 
!             and data sets for satellite data assimilation and remote sensing applications 
!            (JGR, Revised) 
!
!	Input variables :
!
!       theta            ----  local zenith angle (0 - 60.0)
!       freq             ----  frequency in GHz	( 0 - 90.0) 
!       veg_frac         ----  Vegetation fraction (0 - 1.0)      (GDAS)
!       veg_tp           ----  Vegetation type		     (GDAS, not used)
!                             1: Broadleave Evergreen Trees
!                             2: Broadleave Deciduous Trees
!                             3: Broad & Needle Mixed Forest
!                             4: Needleleave Evergreen Trees
!                             5: Needleleave Deciduous Trees
!                             6: Broadleave Tree with Groundcover (Savana)
!                             7: Groundcover Only (Perenial Groundcover)
!                             8: Broadleave Shrubs with Perenial Groundcover
!                             9: Broadleave Shrubs with Bare Soil
!                             10: Dwarf Trees & Shrubs with Bare Soil
!                             11: Bare Soil'
!                             12: Cultivations (use paramater 7)
!                             13: Glacial		
!
!       soil_tp          ----  Soil type                 (GDAS, not used)
!                             1: Loamy Sand (coarse)
!                             2: Silty Clayloam (medium)
!                             3: Light Clay (fine)
!                             4: Sand Loam (coarse-medium)
!                             5: Sandy Clay (coarse-fine)
!                             6: Clay Loam (medium-fine)
!                             7: Sandy Clay loam (coarse-med-fine)
!                             8: Loam (organic)
!                             9: Ice (use loamy sand property)
!
!
!		t_soil           ----  soil temperature (K)	 (GDAS)
!		t_skin           ----  scattering layer temperature (K)			 (GDAS)
!		mv               ----  volumetric moisture content in soil (0.0 - 1.0)	 (GDAS)
!		snow_depth       ----  scatter medium depth (mm?)			(GDAS)
!
!	Output variables:
!
!
!
!   Important Internal Variables 
!
!		rhob              ----  bulk volume density of the soil (1.18-1.12)
!		rhos              ----  density of the solids (2.65 g.cm^3 for solid soil material)
!		sand              ----  sand fraction (sand + clay = 1.0)
!		clay              ----  clay fraction 
!		lai               ----  leaf area index (eg. lai = 4.0 for corn leaves)
!		sigma             ----  surface roughness formed between medium 1 and 2, 
!                               expressed as he standard deviation of roughtness height (mm)
!	    leaf_thick        ----  leaf thickness (mm)
!		rad               ----  radius of dense medium scatterers (mm)
!		va                ----  fraction volume of dense medium scatterers(0.0 - 1.0)
!       ep                ----  dielectric constant of ice or sand particles, complex value
!                               (e.g, 3.0+i0.0)
!	Program Modification History 
!
!		Code created			     07/99
!
!		Canopy scattering added		 01/00
!
!       Parameterization of          11/00
!         model inputs 
!         based on surface
!         conditions  
!    
!   
!------------------------------------------------------------------------------------------------



   Parameter(rhob = 1.18, rhos = 2.65, sand = 0.8, clay = 0.2)
   real theta,freq,mv,mge,veg_frac,veg_tp,soil_tp,t_soil,t_skin,snow_depth
   real b,theta_i,theta_t,mu,r12_h,r12_v,r21_h,r21_v,r23_h,r23_v, &
      t21_v,t21_h,t12_v,t12_h,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,esh,esv,&
      lai,leaf_thick,rad,sigma,va,ep_real,ep_imag

   complex esoil, eveg, esnow, eair
   eair = cmplx(1.0,-0.0)
   if (snow_depth .gt.0.0) then 
                              ! ice dielectric constant 
     ep_real = 3.2
     ep_imag = -0.0005
     sigma = 1.0
     va = 0.4 + 0.0004*snow_depth
     rad = 1.0 + 0.005*snow_depth
     call snow_diel(freq, ep_real, ep_imag, rad, va, esnow)
     call soil_diel(freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
!    theta_t = asin(real(sin(theta)*csqrt(eair)/csqrt(esnow)))
!    call reflectance(eair, esnow, theta, theta_t, r12_v, r12_h)
!    call transmitance(eair, esnow, theta, theta_t, t12_v, t12_h)
     theta_i = asin(real(sin(theta)*csqrt(eair)/csqrt(esnow)))
     call reflectance(esnow, eair, theta_i,  theta, r21_v, r21_h)
     call transmitance(esnow, eair, theta_i, theta, t21_v, t21_h)
     mu  = cos(theta_i)
     theta_t = asin(real(sin(theta_i)*csqrt(esnow)/csqrt(esoil)))
     call reflectance(esnow, esoil, theta_i, theta_t, r23_v, r23_h)
     call rough_reflectance(freq, theta_i, sigma, r23_v, r23_h)
     call snow_optic(freq,rad,snow_depth,va,ep_real, ep_imag,gv,gh,& 
            ssalb_v,ssalb_h,tau_v,tau_h)
     call two_stream_solution(t_skin,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,r12_h, &
       r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)
   else 
     sigma = 0.5
     lai = 3.0*veg_frac + 0.5
     mge = 0.5*veg_frac
     leaf_thick = 0.07
     mu  = cos(theta)
!    r12_h    = 0.0
!    r12_v    = 0.0
     r21_h    = 0.0
     r21_v    = 0.0
     t21_h    = 1.0
     t21_v    = 1.0
!    t12_v    = 1.0
!    t12_h    = 1.0
     call soil_diel(freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
     theta_t = asin(real(sin(theta)*csqrt(eair)/csqrt(esoil)))
     call reflectance(eair, esoil, theta, theta_t, r23_v, r23_h)
     call rough_reflectance(freq, theta, sigma, r23_v, r23_h)
     call canopy_diel(freq, mge, eveg)
     call canopy_optic(lai,freq,theta,eveg,leaf_thick,gv,gh,ssalb_v,ssalb_h,tau_v,tau_h)
     call two_stream_solution(t_skin,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,r12_h, &
 	      r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)
   endif
   Return
   End
!--------------------OPTIC--------------------------------------------------
!
!  The following subs calculate the optic parameters of various earth materials
!
!
	SUBROUTINE canopy_optic(lai,frequency,theta,esv,d, gv, gh, &
	                        ssalb_v,ssalb_h,tau_v, tau_h)
!-----------------------------------------------------------------------!
!   FUNCTION: COMPUTE OPTIC PARAMETERS
!
!	INPUT VARIABLES
!
!       frequency         ----  frequency (GHz)
!       theta             ----  incident angle
!       lai               ----  leaf area index
!       esv               ----  leaf dielectric constant
!       d                 ----  leaf thickness (mm)
!
!	OUTPUT VARIABLES
!
!       albedos_v          ----  single scattering albedo at v. polarization
!       albedos_h          ----  single scattering albedo at h. polarization
!       taus_v             ----  optical depth at v. polarization           
!       taus_h             ----  optical depth at h. polarization
!       gv                 ----  asymmetry factor for v pol
!       gh                 ----  asymmetry factor for h pol
!
!
!------------------------------------------------------------------------!
    real frequency,theta,d,lai,ssalb_v,ssalb_h,tau_v,tau_h,gv, gh, mu
    complex ix,k0,kz0,kz1,rhc,rvc,esv,expval1,factt,factrvc,factrhc
    real rh,rv,th,tv,av,ah,astar,rstar
    pi = acos(-1.0)
    mu = cos(theta)
    ix  = cmplx(0.0, 1.0)
    k0  = cmplx(2*pi*frequency/300.0, 0.0)   ! 1/mm
    kz0 = k0*mu
    kz1 = k0*sqrt(esv - sin(theta)**2)
    rhc = (kz0 - kz1)/(kz0 + kz1)
    rvc = (esv*kz0 - kz1)/(esv*kz0 + kz1)  
    expval1=cexp(-2.*ix*kz1*d)
    factrvc=1.0-rvc**2*expval1
    factrhc=1.0-rhc**2*expval1
    factt=4.*kz0*kz1*cexp(ix*(kz0-kz1)*d)
    rv = cabs(rvc*(1.0 - expval1)/factrvc)**2
    rh = cabs(rhc*(1.0 - expval1)/factrhc)**2
    th = cabs(factt/((kz1+kz0)**2*factrhc))**2
    tv = cabs(esv*factt/((kz1+esv*kz0)**2*factrvc))**2
!   av = 1.0 - rv - tv
!   ah = 1.0 - rh - th
!   astar = av + ah
!   rstar = rv + rh
!   Randomly oriented leaves
    gv = 0.5
    gh = 0.5
!   tau_v = 0.5*lai*(astar + rstar)
    tau_v = 0.5*lai*(2.0-tv-th)
    tau_h = tau_v
!   tau_h = 0.5*lai*(astar + rstar)
!   ssalb_v = rstar/ (astar + rstar)
    ssalb_v = (rv+rh)/ (2.0-tv-th)
    ssalb_h = ssalb_v
!   ssalb_h = rstar/ (astar + rstar)
    RETURN
    END
!
!
    SUBROUTINE snow_optic(frequency,a,h,f,ep_real,ep_imag,gv,gh,&
               ssalb_v,ssalb_h,tau_v,tau_h)
!   INPUT VARIABLES
!
!       theta             ----  local zenith angle (degree)
!       frequency         ----  frequency (GHz)
!       ep_real           ----  real part of dielectric constant of particles
!       ep_imag           ----  imaginary part of dielectric constant of particles
!       a                 ----  particle radiu (mm)
!       h                 ----  snow depth(mm)
!       f                 ----  fraction volume of snow (0.0 - 1.0)
!
!   OUTPUT VARIABLES
!
!
!       ssalb             ----  single scattering albedo
!       tau               ----  optical depth
!       g                 ----  asymmetry factor
!
!
!
!   INTERNAL VARIABLES
!
!       ks                ---- scattering coeffcient (/mm)
!       ka                ---- absorption coeffient (/mm)
!       kp                ---- eigenvalue of two-stream approximation
!       yr
!       yi
!       y                 ---- = yr+iyi
!
!
    REAL frequency,a,h,f,ssalb_v,ssalb_h,tau_v,tau_h,gv,gh,k
    REAL ks1,ks2,ks3,ks,kr1,kr2,kr3,kr,ki1,ki2,ki3,ki,ka
    real fact1,fact2,fact3,fact4,fact5
    pi = acos(-1.0)
    k = 2.*pi/(300./frequency)
    yr  = (ep_real - 1.0)/(ep_real + 2.0)
    yi =  - ep_imag/(ep_real + 2.0)
    fact1 = (1.0+2.0*f)**2
    fact2 = 1.0-f*yr
    fact3 = (1.0-f)**4
    fact4 = f*(k*a)**3
    fact5 = 1.0+2.0*f*yr
    ks1 = k*sqrt(fact2/fact5)
    ks2 = fact4*fact3/fact1
    ks3 = (yr/fact2)**2
    ks = ks1*ks2*ks3  
    kr1 = fact5/fact2
!   kr2 = 2.0*fact4*fact3/fact1
    kr2 = 2.0*ks2
    kr3 = 2.0*yi*yr/(fact2**3)
    kr = k*sqrt(kr1+kr2*kr3)
    ki1 = 3.0*f*yi/fact2**2
!   ki2 = 2.0*fact4*fact3/fact1
    ki2 = kr2
!   ki3 = (yr/fact2)**2
    ki3 = ks3
    ki  = k**2/(2.0*kr)*(ki1+ki2*ki3)
!   ka = ki - ks
    gv = 0.5 
    gh = 0.5
    ssalb_v = ks / ki
    if(ssalb_v .gt. 0.999) ssalb_v = 0.999
    ssalb_h = ssalb_v
    tau_v = 2.0*ki*h
    tau_h = tau_v
    RETURN 
    END
!
! ----------------------Dielectric----------------------------------------------- 
!  The following subs calculate the dielectric properties of various earth materials
! 
!
    SUBROUTINE soil_diel(freq,t_soil,vmc,rhob,rhos,sand,clay,esm)
!-------------------------------------------------------------------------!
!	PROGRAM FUNCTION:
!     Compute the dilectric constant of the bare soil
! 
!	INPUT VARIABLES
!
!       frequency         ----  frequency (GHz)
!       t_soil            ----  soil temperature
!       vmc               ----  volumetric moisture content (demensionless)
!       rhob              ----  bulk volume density of the soil (1.18-1.12)
!       rhos              ----  density of the solids (2.65 g.cm^3 for
!                               solid soil material)
!       sand              ----  sand fraction (sand + clay = 1.0)
!       clay              ----  clay fraction
!
!	OUTPUT VARIABLES
!
!       esm               ----  dielectric constant for bare soil
!
!	INTERNAL VARIABLES
!
!       esof              ----  the permittivity of free space  
!       eswo              ----  static dieletric constant 
!       tauw              ----  relaxation time of water  
!       s                 ----  salinity
!
!--------------------------------------------------------------------------!
!
        real    f,a,b,tauw,freq,t_soil,vmc,rhob,rhos,sand,clay
	real    s,alpha,beta,ess,rhoef,t,eswi,eswo
        complex ix,esm,esw,es1,es2
!
! 
        pi = acos(-1.0)
!       ix  = cmplx(0.0, 1.0)
!       s = 0.0   
        alpha = 0.65
        beta  = 1.09 - 0.11*sand + 0.18*clay
        ess = (1.01 + 0.44*rhos)**2 - 0.062                              !A2
        rhoef = -1.645 + 1.939*rhob - 0.020213*sand + 0.01594*clay       !A4
        t = t_soil - 273.0
        f = freq*1.0e9
!       the permittivity at the high frequency limit
        eswi = 5.5
!       the permittivity of free space (esof)
        esof = 8.854e-12
!       static dieletric constant (eswo)
        eswo = 87.134+(-1.949e-1+(-1.276e-2+2.491e-4*t)*t)*t
!       a = 1.0+(1.613e-5*t-3.656e-3+(3.210e-5-4.232e-7*s)*s)*s
!       eswo = eswo*a
!       relaxation time of water (tauw)
        tauw = 1.1109e-10+(-3.824e-12+(6.938e-14-5.096e-16*t)*t)*t
!       b = 1.0+(2.282e-5*t-7.638e-4+(-7.760e-6+1.105e-8*s)*s)*s
!       tauw = tauw*b
        if (vmc .gt. 0.0) then
          es1 = cmplx(eswi, - rhoef *(rhos - rhob)/(2.*pi*f*esof*rhos*vmc)) 
        else
          es1 = cmplx(eswi, 0.)
        endif
        es2 = cmplx(eswo - eswi,0.)/cmplx(1., f*tauw)
        esw = es1 + es2
        esm = 1. + (ess**alpha - 1.)*rhob/rhos + vmc**beta*esw**alpha - vmc       
        esm = esm**(1.0/alpha)
        if(aimag(esm) .ge.0.0) esm = cmplx(real(esm), -0.0001)
	RETURN
	END
!        
	SUBROUTINE snow_diel(frequency,ep_real,ep_imag,rad,frac,ep_eff)
!	Porgram function: compute dielectric constant of snow 
!	INPUT VARIABLES
!
!       frequency         ----  frequency (GHz)
!       ep_real           ----  real part of dielectric constant of particle
!       ep_imag           ----  imaginary part of dielectric constant of particle
!       rad               ----  particle radiu (mm)
!       frac              ----  fraction volume of snow (0.0 - 1.0)
!
!	OUTPUT VARIABLES
!
!
!       ep_eff            ----  dielectric constant of the dense medium
!
!
    REAL frequency,rad,frac,k0,yr,yi
    complex  y,ep_r,ep_i,ep_eff,fracy
    pi = acos(-1.0)
    k0 = 2.*pi/(300./frequency)
    yr  = (ep_real - 1.0)/(ep_real + 2.0)
    yi =   ep_imag/(ep_real + 2.0)
    y = cmplx(yr, yi)
    fracy=frac*y
    ep_r = (1.0 + 2.0*fracy)/(1.0 - fracy)
    ep_i = 2.*fracy*y*(k0*rad)**3*(1.-frac)**4/((1.-fracy)**2*(1.+2.*frac)**2)
    ep_eff = ep_r - cmplx(0.,1.)*ep_i
    if (aimag(ep_eff).ge.0.0) ep_eff = cmplx(real(ep_eff), -0.0001)
    RETURN 
    END
    SUBROUTINE canopy_diel(frequency,mg,esv)
!
!------------------------------------------------------------------------!
!    PROGRAM FUNCTION:
!
!     Compute the dielectric constant of the vegetation canopy
!
!    References 
!
!     Ulaby and El-Rayer, 1987: Microwave dielectric spectrum of vegetation Part II, 
!           dual-dispersion model, IEEE Trans Geosci. Remote Sensing, 25, 550-557
!
!    INPUT VARIABLES
!
!     frequency         ----  frequency (GHz)
!     mg                ----  gravimetric water content 
!     
!  
!    OUTPUT VARIABLES
!
!     esv               ----  dielectric constant of leaves
!
!     Geomatrical optics approximation for vegetation canopy
!     work for horizontal leaves
!
!
    real  frequency,  mg, en, vf, vb
    complex  esv, xx
    en = 1.7 - (0.74 - 6.16*mg)*mg
    vf = mg*(0.55*mg - 0.076)
    vb = 4.64*mg*mg/( 1 + 7.36*mg*mg)
    xx = cmplx(0,1.0)
    esv = en + vf*(4.9 + 75.0/(1.0 + xx*frequency/18.0)-xx*(18.0/frequency)) + &
           vb*(2.9 + 55.0/(1.0 + csqrt(xx*frequency/0.18)))
    if (aimag(esv).ge.0.0) esv = cmplx(real(esv), -0.0001)
    RETURN
    END
!
!
!------------------------Reflectivity---------------------------------------------
!
!
     SUBROUTINE reflectance(em1, em2, theta_i, theta_t, rv, rh)
!
!    PROGRAM FUNCTIONS
!
!    Compute the surface reflectivety using Fresnel equations for a rough surface 
!    having a standard deviation of height of sigma  
!
!
!    Input Variables
!      theta_i            ----  incident angle (degree)
!      theta_t            ----  transmitted angle (degree)
!      em1                ----  dielectric constant of the medium 1
!      em2                ----  dielectric constant of the medium 2
!
!    Output Variables
!
!      rv                 ----  reflectivity at vertical polarization
!      rh                 ----  reflectivity at horizontal polarization
!
    real theta_i, theta_t
    real rh, rv,cos_i,cos_t
    complex em1, em2, m1, m2, angle_i, angle_t
!   Compute the refractive index ratio between medium 2 and 1
!   using dielectric constant (n = sqrt(e))
    cos_i=cos(theta_i)
    cos_t=cos(theta_t)
    angle_i = cmplx(cos_i, 0.0)
    angle_t = cmplx(cos_t, 0.0)
    m1 = csqrt(em1)
    m2 = csqrt(em2)
    rv = (cabs((m1*angle_t-m2*angle_i)/(m1*angle_t+m2*angle_i)))**2
    rh = (cabs((m1*angle_i-m2*angle_t)/(m1*angle_i+m2*angle_t)))**2
    RETURN 
    END
!
!------------------------Transmisitivity---------------------------------------------
!
!
     SUBROUTINE transmitance(em1, em2, theta_i, theta_t, tv, th)
!
!    PROGRAM FUNCTIONS
!
!    Compute the surface reflectivety using Fresnel equations for a rough surface 
!    having a standard devoation of height of sigma  
!
!
!    Input Variables
!
!      theta_i            ----  incident angle (degree)
!      theta_t            ----  transmitted angle (degree)
!      em1                ----  dielectric constant of the medium 1
!      em2                ----  dielectric constant of the medium 2
!
!    Output Variables
!      tv                ----  transmisivity at vertical polarization
!      th                ----  transmisivity at horizontal polarization
!
    real theta_i, theta_t
!
    real th, tv, rr, cos_i,cos_t
!
    complex em1, em2, m1, m2, angle_i, angle_t
!   Compute the refractive index ratio between medium 2 and 1
!   using dielectric constant (n = sqrt(e))
    cos_i=cos(theta_i)
    cos_t=cos(theta_t)
    angle_i = cmplx(cos_i, 0.0)
    angle_t = cmplx(cos_t, 0.0)
    m1 = csqrt(em1)
    m2 = csqrt(em2)
    rr = cabs(m2/m1)*cos_t/cos_i
    tv =  rr*(cabs(2.*m1*angle_i/(m1*angle_t + m2*angle_i)))**2
    th =  rr*(cabs(2.*m1*angle_i/(m1*angle_i + m2*angle_t)))**2
    return
    end
    SUBROUTINE rough_reflectance(frequency, theta, sigma, rv, rh)
!
!    PROGRAM FUNCTIONS
!
!    Compute the surface reflectivety for a rough surface 
!    having a standard devoation of height of sigma  
!
!    References:
!      Wang, J. and B. J. Choudhury, 1992: Passive microwave radiation from soil: examples...
!       Passive Microwave Remote Sensing of .. Ed. B. J. Choudhury, etal VSP. 
!       Also Wnag and Choudhury (1982)
!
!    Input Variables
!      frequency
!      theta              ----  local zenith angle (degree)
!      sigma              ----  standard deviation of rough surface height 
!                               smooth surface:0.38, medium: 1.10, rough:2.15 cm
!
!    Internal Variables
!
!      k0                ----  a propagation constant or wavenumber in a free space
!      rh_s
!      rv_s
!
!    Output Variables
!      rv                ----  reflectivity at vertical polarization
!      rh                ----  reflectivity at horizontal polarization
!
!
    real theta, frequency
!   real p, q, sigma, rh, rv, rh_s, rv_s, k0
    real p, q, rh, rv, rh_s, rv_s, sigma
!   pi = acos(-1.0)
!   rh_s = rh
!   rv_s = rv
    rh_s = 0.3*rh
    rv_s = 0.3*rv
!    k0 = 2*pi*frequency/30.0               ! 1/cm 
!   p = 0.3
    q = 0.35*(1.0 - exp(-0.60*frequency*sigma**2.0))  
!   rh = (q*rv_s + (1.0 - q)*rh_s)*p
!   rv = (q*rh_s + (1.0 - q)*rv_s)*p
    rh = rh_s + q*(rv_s-rh_s)
    rv = rv_s + q*(rh_s-rv_s)
    RETURN 
    END
    Subroutine two_stream_solution(b, mu, gv, gh,ssalb_h, ssalb_v, tau_h,tau_v,r12_h, & 
      r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h, esv, esh)
    real b, mu, gv, gh, ssalb_h, ssalb_v, tau_h,tau_v,r12_h, &
      r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h, esv, esh
!   real esh0, esh1, esh2, esv0, esv1, esv2
!   real esh1, esv1
    real alfa_v, alfa_h, kk_h, kk_v, gamma_h, gamma_v, beta_v, beta_h
    real fact1,fact2
    alfa_h = sqrt((1.0 - ssalb_h)/(1.0 - gh*ssalb_h))
    kk_h = sqrt ((1.0 - ssalb_h)*(1.0 -  gh*ssalb_h))/mu
    beta_h = (1.0 - alfa_h)/(1.0 + alfa_h)
    gamma_h = (beta_h -r23_h)/(1.0-beta_h*r23_h)
    alfa_v = sqrt((1.0-ssalb_v)/(1.0 - gv*ssalb_v))
    kk_v = sqrt ((1.0-ssalb_v)*(1.0 - gv*ssalb_v))/mu
    beta_v = (1.0 - alfa_v)/(1.0 + alfa_v)
    gamma_v = (beta_v -r23_v)/(1.0-beta_v*r23_v)
!   esh0 = i0/b*r12_h
!   esh0 = 0.0
!   esh1 = (1.0 - beta_h)*(1.0 + gamma_h*exp(-2.0*kk_h*tau_h))
!   esh1 = esh1/((1.0-beta_h*r21_h)-(beta_h-r21_h)*gamma_h*exp(-2.0*kk_h*tau_h))
!   esh2 = i0/b*t12_h*(beta_h-gamma_h*exp(-2.0*kk_h*tau_h))
!   esh2 = esh2 /((1.0-beta_h*r21_h)-(beta_h-r21_h)*gamma_h*exp(-2.0*kk_h*tau_h))
!   esh2 = 0.0
!   esv0 = i0/b*r12_v
!   esv0 = 0.0
!   esv1 = (1.0-beta_v)*(1.0+gamma_v*exp(-2.0*kk_v*tau_v))
!   esv1 = esv1/((1.0-beta_v*r21_v)-(beta_v-r21_v)*gamma_v*exp(-2.0*kk_v*tau_v))
!   esv2 = i0/b*t12_v*(beta_v - gamma_v*exp(-2.0*kk_v*tau_v))
!   esv2 = esv2 /((1.0-beta_v*r21_v)-(beta_v-r21_v)*beta_v*exp(-2.0*kk_v*tau_v))
!   esv2 = 0.0
!   esh  = esh0 + t21_h*(esh1 + esh2)
!   esv  = esv0 + t21_v*(esv1 + esv2)
    fact1=gamma_h*exp(-2.0*kk_h*tau_h)
    fact2=gamma_v*exp(-2.0*kk_v*tau_v)
    esh  = t21_h*(1.0 - beta_h)*(1.0 + fact1) &
           /(1.0-beta_h*r21_h-(beta_h-r21_h)*fact1)
    esv  = t21_v*(1.0 - beta_v)*(1.0 + fact2) &
           /(1.0-beta_v*r21_v-(beta_v-r21_v)*fact2)
    return
    end
