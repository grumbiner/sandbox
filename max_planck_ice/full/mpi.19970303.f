      PROGRAM ICEMODEL
C=======================================================================
C  Programmed by:
C     W.Brechner Owens      MPI, Hamburg                          Aug.87
C  Modified by:
C     Achim Stoessel        MPI, Hamburg                          May 91
C     Robert Grumbine       NMC, Camp Springs                     Oct 92
C  Purpose:
C     -Dynamic-thermodynamic simulation of Sea Ice [SI] optionally
C       coupled to: -Oceanic Mixed Layer model [OML]
C                   -Atmospheric Surface Layer model [ASL]
C                   -Atmospheric Boundary Layer model [ABL]
C  Method:
C     -main program combines various subroutines and controls outputs
C     -coordinates: orthogonal curvilinear (alternatively performable
C         in cartesian or spherical coordinates)
C     -domain of integration is allowed to be 'open' at lateral edges
C     -cyclic boundary conditions are provided for circumpolar studies
C     -horizontal grid: Arakawa 'B'
C     -time stepping: leapfrog-trapezoidal
C  Structure:
C     ICEMODEL==>INIT===============>BCSINIT
C             ==>OUTSM(LOLD)
C             ==>BCSH(LOLD)
C             ==>BCSV(LOLD)
C             ==>outstr   BG
C        ---->==>FORFLD=============>BCSQ
C        |  <-==>PRESSB(LOLD)
C        |  | ==>INITREL============>OUTBCS
C        |  |           ============>RELCON(LOLD,LOLD)==>BCOEF(LOLD)
C        |  |           ============>RELAX(LOLD,LNEW)===>DDX(LNEW)
C        |  |                                        ===>DDY(LNEW)
C        |  |                                        ===>MADV(LOLD,LNEW)
C        |  ->==>PLAST(LOLD)========>STRAIN(LOLD)
C        |                  ========>OUTBCS
C        |    ==>RELCON(LOLD,LOLD)
C        |    ==>RELAX(LOLD,3)
C        |    ==>PLAST(3)
C        |    ==>RELCON(LOLD,3)
C        |    ==>RELAX(3,LNEW)
C        |    ==>SADVECT(LOLD,LNEW)
C        |    ==>BCSH(LNEW)
C        |    ==>SADVECT(LNEW,LNEW)
C        |    ==>SDIFFUS(LOLD)-->BCSFLX
C        |    ==>BCSH(LNEW)
C        |    ==>GROWTH(LOLD,LNEW)==>SHDEF(LNEW)========>STRAIN(LNEW)
C        |                        ==>SHWARA
C                                    SHWARA replaced by a read in FORFLD
C        |                        ==>OBUDGET/ECMBUDO/EKMAO
C        |                        ==>BUDGET/ECMBUDI/EKMAH
C        |                        ==>PMLEX
C        |    ==>OUTSM(LNEW)
C        |    ==>BCSH(LNEW)
C        |    ==>output      BG
C        |    LOLD<-->LNEW---
C        |                  |
C        -----------<--------
C  Interface:
C     -INPUT,OUTPUT: standard control output
C     -TAPE10: definition masks for domain
C     -TAPE11: wind forcing
C     -TAPE12: oceanic forcing (temperature, salinity, currents)
C     -TAPE20: ice compactness results for selected dates (plots)
C     -TAPE13: atmospheric temperature forcing
C     -TAPE15: ice velocity results for plots
C     -TAPE16: results printed in domain's shape
C     -TAPE17: various results for plots
C     -TAPE18: accumulated results for summation plots
C     -TAPE19: humidity forcing
C     -TAPE8 : precipitation forcing
C     -TAPE14: atmospheric pressure forcing
C  Externals(subroutines):
C     -INIT:    sets model parameters and initial fields
C     -OUTSM:   smoothes variables in outflow grid cells
C     -BCSH:    sets cyclic boundary cond. for values in grid center
C     -BCSV:    sets cyclic boundary cond. for values at grid edge
C     -FORFLD:  reads in forcing fields
C     -PRESSB:   calculates ice strength
C     -INITREL: performes initial relaxation to balance initial values
C     -PLAST:   calculates viscosities
C     -RELCON:  calculates diagnostic terms of momentum balance
C     -RELAX:   solves momentum balance by overrelaxation
C     -SADVECT: calculates advection terms for continuity equation
C     -SDIFFUS: calculates diffusion terms for continuity equation
C     -GROWTH:  represents thermodynamic part of the model
C     -DRUCKF:  prints integer results in domain-mask shape
C-----------------------------------------------------------------------
C  References:
C-----------------------------------------------------------------------
C     -Businger, J. A., J. C. Wyngaard, Y. Izumi and E. F. Bradley (1971)
C         Flux profile relationships in the atmospheric surface layer. 
C         J. Atm. Sci., 28, 181-189.
C     -ECMWF Research Department (1985): Research manual 3, ECMWF 
C         forecast model, physical parameterization.  ECMWF Met. Bull.,
C         M1.6/2(1), Rev. 1.
C     -Fiedler, F. and H. A. Panofsky (1972): The geostrophic drag
C         coefficient and the effective roughness length.  Quart.
C         J. Roy. Met. Soc., 98, 213-220.
C     -Gordon, A. L. and T. Baker (1982): Objective contouring and the
C         grid point data set.  In: Southern Ocean Atlas, Columbia
C         University Press, NY, 2, 15-29.
C     -Hibler, W. D. (1979): A dynamic thermodynamic sea ice model.
C         J. Phys. Oceanogr., 9, 815-846.
C     -Hibler, W. D. (1980) Documentation for a two-level dynamic-thermo-
C         dynamic sea ice model. CRREL, Hanover, N.H., Spec. Rep., 80-8.
C     -Hibler, W. D. III and S. F. Ackley (1983): Numerical simulation 
C         of the Weddell Sea pack ice.  J. Geophys. Res., 88, 2873-2887.
C     -Hibler,W.D.(1984): The role of sea ice dynamics in modeling CO2
C         increases. In: Climate processes and climate sensitivity,
C         eds.: J.Hansen and T.Takahashi, Geophys.Monogr.,29,238-253.
C     -Hockney,R.W. and C.R.Jesshope (1981): Parallel computers, archi-
C         tecture, programming and algorithms. Adam Hilger LTD, Bristol.
C     -Jaeger, L. (1976): Monatskarten des Niederschlages fuer die ganze 
C         Erde.  Berichte des Deutschen Wetterdienstes, 139.
C     -Koch, C. (1986): Numerische Simulation der Wechselwirkungen 
C         zwischen Meereis und Atmosphaere im Bereich der Weddellsee.
C         Meteorologisches Institut der rheinischen Friedrich-
C         Wilhelms Universitaet, Bonn, Dissertation.
C     -Koch,C.(1988): A coupled sea ice - atmospheric boundary layer
C         model. Part I: Description of the model and 1979 standard run.
C         Beitr.Phys.Atmosph., 61(4), 344-354.
C     -Kraus, E. B. and J. S. Turner (1967): A one-dimensional model of
C         the seasonal thermocline, Part II, Tellus, 19, 98-105.
C     -Lemke, P. (1987): A coupled one-dimensional sea ice-ocean model
C         J. Geophys. Res., 92, 13164-13172.
C     -Lemke,P., W.B.Owens and W.D.Hibler III(1990): A coupled sea ice -
C         mixed layer - pycnocline model for the Weddell Sea. J.Geophys.
C         Res., 95(C6), 9513-9525.
C     -Leppaeranta,M. and W.D.Hibler III (1985): The role of plastic ice
C         interaction in marginal ice zone dynamics. J.Geophys.Res., 90,
C         (C6), 11899-11909.
C     -Louis,J.F.(1979): A parametric model of vertical eddy fluxes in
C         the atmosphere. Bound.Layer Met.,17,187-202.
C     -Maykut, G. A. (1982): Large-scale heat exchange and ice production
C         in the central Arctic.  J. Geophys. Res., 87, 7971-7984.
C     -Mesinger,F. and A.Arakawa (1976): Numerical Methods used in atmo-
C         spheric models. GARP Publ.Ser.No.17.
C     -Niiler, P. P. and E. B. Kraus (1977): One-dimensional models of 
C         the upper ocean.  In: Modelling and Prediction of the Upper
C         layers of the ocean, ed: E. B. Kraus, Pergamon Press,
C         New York, 143-172.
C     -Owens,W.B. and P.Lemke(1990): Sensitivity Studies with a sea ice-
C         mixed layer - pycnocline model in the Weddell Sea. J.Geophys.
C         Res., 95(C6), 9527-9538.
C     -Parkinson, C. L. and W. M. Washington (1979): A large-scale 
C         numerical model of sea ice. J. Geophys. Res., 84, 311-337.
C     -Semtner, A. J. Jr., (1976): A model for the thermodynamic 
C         growth of sea ice in numerical investigations of climate.
C         J. Phys. Oceanogr., 6, 379-389.
C     -Stoessel,A., P.Lemke and W.B.Owens(1990): Coupled sea ice - mixed
C         layer simulations for the Southern Ocean. J.Geophys.Res., 95
C         (C6), 9539-9555.
C     -Stoessel,A.(1990): Meereismodellierung im Suedlichen Ocean. Max-
C         Planck-Inst.f.Meteorologie, Hamburg, Examensarbeit Nr.6.
C     -Trenberth, K. E. and J. G. Olson (1988): ECMWF global analyses
C         1979-1986: circulation statistics and data evolution.  NCAR,
C         Boulder, Colorado, Tech. Note, TN-300+STR.
C=======================================================================
      IMPLICIT none

      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
      INCLUDE "rheology.inc"
      INCLUDE "oml.inc"
CD      REAL hmlref
C=======================================================================
C     hmlref - reference (maximum) depth for mixed layer 
C=======================================================================
C  Parameter:
C     -L: number of grid points in X-direction (even number only!)
C     -M: number of grid points in Y-direction (even number only!)
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      REAL FM, F, COSPHI, SINPHI
C=======================================================================
C  Common CORR: contains coriolis parameter related variables
C     -FM:      coriolis parameter for grid center [1/s]
C     -F:       coriolis parameter for grid edge [1/s]
C     -COSPHI:  COS of grid center latitude []
C     -SINPHI:  SIN of grid center latitude []
C=======================================================================
      COMMON/IPARM/H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN
C=======================================================================
C  Common IPARM: contains sea ice related parameters
C     -RHOICE:  sea ice density [kg/m**3]
C     -HO:      lead closing parameter for ice compactness equation [m]
C     -HNU:     harmonic diffusion coefficient [m**2/s]
C     -HNU2:    biharmonic diffusion coefficient [m**4/s]
C     -ARMIN:   minimum (cut off) ice compactness [100%=1]
C     -ARMAX:   maximum ice compactness [100%=1]
C     -HMIN:    minimum (cut off) ice thickness [m]
C     -RHOSNO:  snow density [kg/m**3]
C     -CC:      volumetric heat capacity of water [J/m**3/K]
C=======================================================================
      COMMON/DRV/DXSQ, DYSQ, SX2, SY2, SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
C=======================================================================
C  Common DRV: contains grid size related parameters
C     -DX:      grid size in X-direction (before use of metric!)[m]
C     -DXSQ:    DX*DX [m**2]
C     -DY:      grid size in Y-direction [m]
C     -DYSQ:    DY*DY [m**2]
C     -SX2:     0.5/DXSQ [1/m**2]
C     -SY2:     0.5/DYSQ [1/m**2]
C     -SXY:     0.25/(DX*DY) [1/m**2]
C=======================================================================
      COMMON/STP/T, NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      REAL T
      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
C=======================================================================
C  Common STP: contains time step related parameters
C     -T:       current time step [s]
C     -DT:      time step []
C     -NTMES:   number of time steps for specific integration []
C     -NRST:    time steps between writing RESTART record []
C     -NRREC:   number of RESTART records to read []
C     -NPLT:    time steps between plot outputs []
C     -NSTAT:   time steps between statistics output []
C     -IIC:     running time step (INTEGER)[]
C     -NFLD:    time step limit for beginning of output []
C     -NSTA:    time steps between areal prints (NSTAT + 5)[]
C=======================================================================
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
C=======================================================================
C  Common COORD: contains metric coefficients
C     -PM:      metric coefficients for X-direction []
C     -PN:      metric coefficients for Y-direction []
C     -DNDX:    metric term for centripetal forces in Y-direction []
C     -DMDX:    metric term for centripetal forces in X-direction []
C=======================================================================
CD      COMMON/VEL/U(L,M,3), V(L,M,3)
      REAL U(L,M,3), V(L,M,3)
C=======================================================================
C  Common VEL: ice velocity field (defined st grid edge points)
C     -U:       X-component of ice velocity [m/s]
C     -V:       Y-component of ice velocity [m/s]
C=======================================================================
CD      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
C=======================================================================
C  Common THCK: thickness fields (defined on grid center points)
C     -H:       mean ice thickness [m]
C     -A:       ice compactness [100%=1]
C     -HSN:     snow thickness [m]
C=======================================================================
      COMMON/FRWND/CDWIN, SINWIN, COSWIN, UWIN(L,M), VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN 
C=======================================================================
C  Common FRWND: atmospheric parameters and wind forcing
C     -RHOAIR:  air density [kg/m**3]
C     -CDWIN:   atmospheric drag coefficient []
C     -SINWIN:  SIN of ABL turning angle []
C     -COSWIN:  COS of ABL turning angle []
C     -UWIN:    X-component of wind velocity [m/s]
C     -VWIN:    Y-component of wind velocity [m/s]
C=======================================================================
      COMMON/FRWAT/SINWAT, COSWAT, UWAT(L,M), VWAT(L,M)
      REAL SINWAT, COSWAT, UWAT, VWAT
C=======================================================================
C  Common FRWAT: oceanic parameters and current forcing
C     -RHOWAT:  water density [kg/m**3]
C     -CDWAT:   oceanic drag coefficient []
C     -SINWAT:  SIN of OBL turning angle []
C     -COSWAT:  COS of OBL turning angle []
C     -UWAT:    X-component of current velocity [m/s]
C     -VWAT:    Y-component of current velocity [m/s]
C=======================================================================
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1  ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL TAIR, TD, ACL, PA, UG, TA, RPREC
C=======================================================================
C  Common THFOR: atmospheric thermodynamic forcing variables
C     -TAIR:    air temperature [C]
C     -TD:      relative humidity [%]
C     -ACL:     cloudiness [100%=1]
C     -PA:      surface air pressure [hPa -> Pa]
C     -UG:      wind velocity [m]
C     -TA:      air temperature [K]
C     -RPREC:   precipitation rate [mm/month -> m/s]
C=======================================================================
C  Common THPAR: contains various thermodynamic parameters, moved into
C    physical.inc
C     -SUBL:    latent heat of sublimation [J/kg]
C     -VAPL:    latent heat of vaporization [J/kg]
C     -D3:      Stefan-Boltzmann constant * emissivity [W/m**2/K**4]
C     -CON:     thermal conductivity of sea ice [W/m/K]
C     -ALBI:    sea ice albedo []
C     -ALBM:    melting sea ice albedo []
C     -ALBW:    open water albedo []
C     -ALBSN:   snow albedo []
C     -ALBSNM:  melting snow albedo []
C     -TMELT:   melting point [K]
C     -TFREZ:   freezing point for ocean [C]
C     -tfreez:  Stand-alone function written by Bob Grumbine to compute
C                 freezing point as a function of salinity.  13 Sep. 1994
C     -CONSN:   thermal conductivity of snow [W/m/K]
C=======================================================================
C  Common VISCP: contains viscosity parameters, moved to rheology.inc
C     -PSTAR:   empirical ice strength parameter [N/m**2]
C     -CSTAR:   empirical ice strength constant []
C     -ECCEN:   ratio of compressive to shear strength []
C     -ZMAX:    limiting factor for maximum bulk viscosity []
C     -ZMIN:    minimum bulk viscosity [kg/s]
C     -GMIN:    maximum viscous creep rate [1/s]
C     -ECM2:    1/ECCEN**2
C=======================================================================
      COMMON/PRESS/P(L,M)
      REAL P
C=======================================================================
C  Common PRESS: equation of state for ice pressure
C     -P:       ice strength [N/m]
C=======================================================================
CD      COMMON/TEMP/TICE(0:L,0:M)
      REAL TICE(0:L, 0:M)
C=======================================================================
C  Common TEMP: surface temperature
C     -TICE:    surface temperature of ice or snow, resp.[C]
C=======================================================================
CD      COMMON/TEMPM/TICM(0:L,0:M,NLEVEL)
      REAL TICM(0:L, 0:M, NLEVEL)
C=======================================================================
C  Common TEMPM: surface temperatures for seven-level heat balance
C     -TICM:    surface temp. for seven ice thickness categories [C]
C               Number of levels made adjustable via icegrid.inc RG
C=======================================================================
C  Common RELAXP: parameters for overrelaxation routine, moved to 
C    rheology.inc
C     -MMAX:    maximum iteration steps []
C     -VRMAX:   cut off velocity difference between iteration steps[m/s]
C     -WT:      relaxation factor []
C=======================================================================
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
C=======================================================================
C  Common MASK: contains definition points for specific domain
C     -VM:      mask for grid edge points []
C     -HM:      mask for grid center points []
C     -OM:      mask to separate outflow grid points []
C     -FLM:     mask for fluxes normal to boundaries []
C=======================================================================
CD      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
CD     1  QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
CD     2  QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M), 
CD     3  QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M)
      REAL QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

      REAL bathy(0:L, 0:M)
      
C=======================================================================
C  Common PML: contains variables for OML model
C     -QS:      OML salinity [ppt]
C     -QT:      OML temperature [C]
C     -QH:      OML depth [m]
C     -QSB:     salinity at base of second oceanic layer [ppt]
C     -QTB:     temperature at base of second oceanic layer [C]
C     -QHB:     depth of second oceanic layer [m]
C     -QDS:     halocline thickness [m]
C     -QDT:     thermocline thickness [m]
C     -QHSTO:   heat storage in equivalent [m] of ice thickness
C     -HS:      salinity content of water column [ppt*m]
C     -HT:      heat content of water column [C*m]
C     -QV:      cubic ice vel. for OML kinetic energy input [(m/s)**3]
C     -QRHO:    surface buoyancy flux [1/s]
C     -QW:      kinetic energy input for OML [(m/s)**3]
C     -IEN:     flag for calculation of entrainment []
C     -FW:      net freezing rate [m]
C     -MLFIX:   flag for either fixed or variable OML []
C    Bathy:     Array of bottom depths, for bottom-limited mixed layers.
C               Bob Grumbine 10 Jul 1995.
C=======================================================================
CD      COMMON/GEO/PI, RAD
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M)
C=======================================================================
C  Common GEO: mathematical parameters
C     -PI:      circle number []
C     -RAD:     factor for conversion of degree into radiant (PI/180)[]
C=======================================================================
CD      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      REAL SURTYP, SURFWIN
C=======================================================================
C  Common ABLM: parameters for ABL model
C     -ZOW:     roughness length over water [m]
C     -ZOI:     roughness length over ice [m]
C     -CLB:     rhoice * lat. heat of fusion at ice bottom [J/m**3]
C     -CLO:     rhoice * lat. heat of fusion at ice surface [J/m**3]
C     -FAKTH:   rhoair * v.Karman const.* spec.heat of dry air[J/m**3/K]
C     -ABLFIX:  switch for simulation with or without ABL []
C     -SURFWIN: switch for fixed wind turning or calculated by ABL model
C     -ECMTYP:  switch for simulation with ASL or ABL
C     -SURTYP:  Replace ABLFIX and ECMTYP with a single variable 
C                 to specify the boundary layer.
C=======================================================================
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      REAL CD, SINBET, COSBET, BETA, TAUX, TAUY
C=======================================================================
C  Common TAU: variables of ABL model derived dynamic forcing of sea ice
C     -CD:      drag coefficient []
C     -SINBET:  SIN of wind turning angle []
C     -COSBET:  COS of wind turning angle []
C     -BETA:    wind turning angle [deg]
C     -TAUX:    X-component of wind stress [N/m**2]
C     -TAUY:    Y-component of wind stress [N/m**2]
C=======================================================================
CD      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
CD     1  ,UST1(0:L,0:M), TMPL1(0:L,0:M)
C=======================================================================
C  Common FLUX: heat flux and ABL stability related variables
C     -FLSE:    sensible heat flux [W/m**2]
C     -FLLA:    latent heat flux [W/m**2]
C     -WMUE1:   stability parameter []
C     -UST1:    friction velocity [m/s]
C     -TMPL1:   Richardson number or mod.Monin-Obukhov length []or[1/m]
C=======================================================================
      COMMON/OUTFLOW/NOUT, IOUT(LDO), JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
C=======================================================================
C  Common OUTFLOW: contains outflow cells
C     -NOUT:    number of outflow cells []
C     -IOUT:    X-coordinate of outflow cell []
C     -JOUT:    Y-coordinate of outflow cell []
C=======================================================================
CD      COMMON/PMLPARM/DCVM, WUP, COSGAM, RTC, STC, QTOC
      REAL DCVM, WUP, COSGAM, RTC, STC, QTOC
C=======================================================================
C  Common PMLPARM: contains parameters for OML model, many moved to
C    oml.inc
C     -SICE:    sea ice salinity [ppt]
C     -QHW:     emp. parameter for dissipation of mechanical energy [m]
C     -QHS:     emp. parameter for dissipation of convective energy [m]
C     -DCVM:    upper limit for dissipation of convective energy []
C     -ENTMAX:  upper limit for entrainment velocity [m]
C     -WUP:     upwelling velocity (specified empirically) [m/s]
C     -CW:      drag coefficient for kinetic energy input []
C     -COSGAM:  COS of current turning angle (?)
C     -BETAS:   expansion coefficient for salinity [1/ppt]
C     -BETAT:   expansion coefficient for temperature [1/K]
C     -EPSAA:   limitations for entrainment velocity and conv.[(m/s)**2]
C     -RTC:     time constant for mixed layer retreat []
C     -STC:     salinity time constant []
C     -QTOC:    oceanic heat flux (specified) [W/m**2]
C=======================================================================
CD      COMMON/SNOFLG/SNOFLG
      REAL SNOFLG
C=======================================================================
C  Common SNOFLG: contains switch for inclusion of snow
C=======================================================================
C=======================================================================
CD      COMMON/WORK/TMP(0:L,0:M), RH(0:L,0:M), RA(0:L,0:M), DFX(0:L,0:M),
CD     1  DFY(0:L,0:M), RSN(0:L,0:M), WRK(0:L,0:M,4), QTM(0:L,0:M), 
CD     2  SH(0:L,0:M)
      REAL RH(0:L, 0:M), RA(0:L, 0:M), RSN(0:L, 0:M)
      REAL QTM(0:L, 0:M), ATMFLX(0:L,0:M)
C=======================================================================
C  COMMON WORK: CONTAINS FIELDS WITH CONTINUOUSLY VARYING OCCUPATION
C**REMARK: THE VARIABLES OF THIS COMMON BLOCK WILL BE DESCRIBED IN EACH
C    SUBROUTINE SEPARATELY; ALL OTHER COMMON BLOCKS ARE DESCRIBED IN
C    THE MAIN ROUTINE ONLY.
C     -TMP:     DUMMY VARIABLE
C     -RH:      ADVECTION AND DIFFUSION TERMS OF ICE THICKNESS CONT.EQ.
C     -RA:      ADV. AND DIFF. TERMS OF ICE COMPACTNESS CONT.EQ.
C     -DFX:     DUMMY VARIABLE
C     -DFY:     DUMMY VARIABLE
C     -RSN:     ADVECTION AND DIFFUSION TERMS OF ICE THICKNESS CONT.EQ.
C     -WRK:     DUMMY VARIABLE
C     -QTM:     OCEANIC HEAT FLUX
C     -ATMFLX:      ATMOSPHERIC HEAT FLUX (WEIGHTED WITH ICE COMPACTNESS)
C=======================================================================
C     Output fields.  BG
      REAL FLAGI1(L,M), FLAGI2(L,M), FLAGI(0:L,0:M)
CD      REAL HOSM(2), HOSNSM(2), 
CD      REAL SHA(0:L,0:M), QTMA(0:L,0:M), FRS(0:L,0:M),
CD     2 SB(0:L,0:M), BM(0:L,0:M), SHM(0:L,0:M), HMM(0:L,0:M), STAUX(L,M),
CD     3 STAUY(L,M), STAUM(0:L,0:M), TAUM(0:L,0:M), SU(L,M), UMM(L,M), 
CD     4 SV(L,M)
C      STICM(0:L,0:M), STA(0:L,0:M),  SFLSE(0:L,0:M), SQTM(0:L,0:M), VMM(L,M)
C=======================================================================
C     -HOSM:    SPATIAL AVERAGE OF ICE THICKNESS AT OUTFLOW CELLS
C     -HOSNSM:  SPATIAL AVERAGE OF SNOW THICKNESS AT OUTFLOW CELLS
C     -FLAGI1:  GRID EDGE MASK FOR PLOTS SHOWING ICE COVERED AREAS ONLY
C     -FLAGI2:  SAME AS FLAGI1, BUT FOR MONTHLY AVERAGED VALUES
C     -FLAGI:   SAME AS FLAGI1, BUT FOR GRID CENTER POINTS
C     -SHA:     TIME AVERAGED ATMOSPHERIC HEAT FLUX
C     -QTMA:    TIME AVERAGED OCEANIC HEAT FLUX
C     -FRS:     SUMMATION OVER TIME OF ICE GROWTH RATE
C     -SB:      SUMMATION OVER TIME OF ICE COMPACTNESS
C     -BM:      TIME AVERAGE OF ICE COMPACTNESS
C     -SHM:     SUMMATION OVER TIME OF ICE THICKNESS
C     -HMM:     TIME AVERAGE OF ICE THICKNESS
C     -STAUX:   SUMMATION OVER TIME OF X-COMPONENT OF WIND STRESS
C     -STAUY:   SUMMATION OVER TIME OF Y-COMPONENT OF WIND STRESS
C     -STAUM:   SUMMATION OVER TIME OF SCALAR WIND STRESS
C     -TAUM:    TIME AVERAGE OF SCALAR WIND STRESS
C     -SU:      SUMMATION OVER TIME OF X-COMPONENT OF ICE VELOCITY
C     -UMM:     TIME AVERAGE OF X-COMPONENT OF ICE VELOCITY
C     -SV:      SUMMATION OVER TIME OF Y-COMPONENT OF ICE VELOCITY
C     -VMM:     TIME AVERAGE OF Y-COMPONENT OF ICE VELOCITY, unused
C     -STA:     SUMMATION OVER TIME OF AIR TEMPERATURE, unused
C     -SQTM:    SUMMATION OVER TIME OF OCEANIC HEAT FLUX, unused
C     -SFLSE:   SUMMATION OVER TIME OF SENSIBLE HEAT FLUX, unused
C     -STICM:   SUMMATION OVER TIME OF ICE/SNOW SURFACE TEMPERATURE, unused
C=======================================================================
C  Variables for dynamics
      REAL BU(L,M), BV(L,M), AMAS(L,M), FX(L,M), FY(L,M)
      REAL ETA(L,M), ZETA(L,M), E11(L,M), E12(L,M), E22(L,M)

C      Local declarations
      INTEGER NOLRREC, INTYP
      LOGICAL noice, overload
      INTEGER I, J, LOLD, LNEW, LRHS, LLHS, JJC
      REAL DELT

C-----------------------------------------------------------------------
C  SET INITIAL VALUES OF RUNNING INDICES
C-----------------------------------------------------------------------
      LOLD=1
      LNEW=2
C-----------------------------------------------------------------------
C  CALL SUBROUTINE INIT TO SET UP PARAMETERS OF THE MODEL
C-----------------------------------------------------------------------
CD      PRINT *,'Calling INIT'
CD      CALL INIT(INTYP, bathy, hmlref, DCVM, WUP, COSGAM, RTC, STC, QTOC,
      CALL INIT(INTYP, bathy, DCVM, WUP, COSGAM, RTC, STC, QTOC,
     1     SNOFLG,
     2     TICE, TICM,
     3     U, V,
     4     SURTYP, SURFWIN,
     5     QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, 
     6     QV, QRHO, QW, FW, IEN, MLFIX, 
     7     H, A, HSN)
C-----------------------------------------------------------------------
C  MAKE SURE THE OUTFLOW GRID POINTS ARE SMOOTHED
C-----------------------------------------------------------------------
CD      CALL OUTSM(H, 1, HOSUM, HOSM(1), 
CD     1 PN, PM, OM, NOUT, IOUT, JOUT)
CD      PRINT *,'Calling outsm'
CD      CALL OUTSM(A, 1, SCR1, SCR2, 
CD     1 PN, PM, OM, NOUT, IOUT, JOUT)
CD      CALL OUTSM(HSN, 1, HSNOSUM, HOSNSM(1), 
CD     1 PN, PM, OM, NOUT, IOUT, JOUT)
CBG Outflow is now irrelevant to the model, as domain is taken to have
CBG   no outflow.  This will need to be revisited for regional modelling.
CBG   Robert Grumbine 24 February 1997
C-----------------------------------------------------------------------
C  INITIAL CALL FOR CYCLIC BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      CALL BCSH(H, LOLD, OM)
      CALL BCSH(A, LOLD, OM)
      CALL BCSH(HSN, LOLD, OM)
      CALL BCSV(U, V, LOLD, VM)
      
C-----------------------------------------------------------------------
C  SET RUNNING SUM VALUES TO 0
C  INITIAL INTEGRATIONS OVER ENTIRE DOMAIN
C  SET RUNNING SUM ARRAY VALUES TO 0
C  PRINT OUT STATISTICS
C-----------------------------------------------------------------------
CD      PRINT *,'Calling outstr'
      CALL outstr(
     1    FLAGI1, FLAGI2, FLAGI,
CD     5    OM, TAUX, TAUY, TA, FLSE, TICM, QH ,
     2    OM, TAUX, TAUY, TA, TICM, QH ,
     3    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     4    CLO, T, 
     5    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS)
C-----------------------------------------------------------------------
C----------------------- MAIN COMPUTATIONAL LOOP -----------------------
C-----------------------------------------------------------------------
      DO 400 IIC=1, NTMES
       T=T+DT
C-----------------------------------------------------------------------
C  FIRST GET THE FORCING FIELDS FOR THIS TIME STEP
C-----------------------------------------------------------------------
CD        PRINT *,'Calling forfld'
       CALL FORFLD(LWDN, SWDN, INTYP, IIC, UWIN, VWIN)
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after forfld, 10,000 ',
     1        LWDN
       ENDIF

C-----------------------------------------------------------------------
C      IF there is no ice cover, skip to the thermodynamics
C-----------------------------------------------------------------------
       IF (noice(A, LP, MP, 2, LOLD)) GO TO 9999

C-----------------------------------------------------------------------
C  CALCULATE ICE PRESSURE WITH OLD VALUES OF ICE THICKNESSES
C-----------------------------------------------------------------------
CD       PRINT *,'Calling pressb'
       CALL PRESSB(LOLD, P, H, A)
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after pressb, 10,000 ',
     1             LWDN
       ENDIF
C-----------------------------------------------------------------------
C  NOW DO VELOCITY INTEGRATION
C**REMARK: IF FIRST TIME, THEN DO INITIAL RELAXATION TO ADJUST VELOCI-
C    TIES TO VISCOUS STRESSES AND FORCING FIELDS.
C-----------------------------------------------------------------------
       IF (T.EQ.DT.AND.NRREC.EQ.0)THEN
CD        PRINT *,'Calling initrel'
CD        CALL INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, DT,
        CALL INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, 
     1               SURTYP, SURFWIN)
       END IF
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after initrel, 10,000 ',
     1             LWDN
       ENDIF

C-----------------------------------------------------------------------
C  DO THE INTEGRATION IN 2 PARTS, FIRST A PREDICTIVE TIMESTEP FOLLOWED
C    BY A CENTERED TIMESTEP
C-----------------------------------------------------------------------
       DO 60 JJC=1, 2
        IF (JJC.EQ.1)THEN
C  THIS IS 1ST TIME THROUGH:
         LLHS=3
         LRHS=LOLD
        ELSE
C  THIS IS SECOND TIME THROUGH:
         LLHS=LNEW
         LRHS=3
C-----------------------------------------------------------------------
C  AVERAGE OLD AND PREDICTED VELOCITIES TO DO A CENTERED TIME STEP
C-----------------------------------------------------------------------
         DO 40 J=2, MM
         DO 40 I=1, L
          U(I, J, LRHS)=.5*(U(I, J, LRHS)+U(I, J, LOLD))
          V(I, J, LRHS)=.5*(V(I, J, LRHS)+V(I, J, LOLD))
   40    CONTINUE
        END IF
C-----------------------------------------------------------------------
C  CALCULATE VISCOSITIES FOR LRHS VELOCITIES
C-----------------------------------------------------------------------
CD        PRINT *,'Calling plast'
        CALL PLAST(LRHS, E11, E22, E12, ZETA, ETA, U, V, H, A, HSN)
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after plast, 10,000 ',
     1             LWDN
       ENDIF
C-----------------------------------------------------------------------
C  CALCULATE THOSE PARTS OF THE EQUATIONS THAT DO NOT DEPEND ON THE NEW
C    VELOCITY, AND STORE THEM IN THE TEMPORARY ARRAYS
C-----------------------------------------------------------------------
CD        PRINT *,'Calling relcon'
        CALL RELCON(LOLD, LRHS, U, V, BU, BV, AMAS, FX, FY, H, 
     1              ETA, ZETA, SURTYP, SURFWIN)
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after relcon, 10,000 ',
     1             LWDN
       ENDIF
C-----------------------------------------------------------------------
C  NOW SOLVE FOR THE NEW VELOCITIES, USING OVER-RELAXATION
C-----------------------------------------------------------------------
        CALL RELAX(LRHS, LLHS, U, V, BU, BV, AMAS, FX, FY, H, 
     1             ETA, ZETA)
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after relax, 10,000 ',
     1             LWDN
       ENDIF
   60  CONTINUE
C-----------------------------------------------------------------------
C  NOW START TO SOLVE THE CONTINUITY EQUATIONS, FIRST USING A FORWARD
C    (EULER) TIME STEP AND THEN A BACKWARD TIME STEP (FORWARD-BACKWARD
C    (MATSUNO) SCHEME)
C-----------------------------------------------------------------------
       DO 120 JJC=1, 2
        IF (JJC.EQ.1)THEN
C  THIS IS 1ST TIME THROUGH:
         LRHS=LOLD
        ELSE
C  THIS IS SECOND TIME THROUGH:
         LRHS=LNEW
        END IF
C  USE HALF THE TIME STEP FOR FORWARD INTEGRATION:
        DELT=.5*FLOAT(JJC)*DT
C-----------------------------------------------------------------------
C  ADD IN HORIZONTAL ADVECTION TERMS
C-----------------------------------------------------------------------
        CALL SADVECT(RH, H, U, V, LRHS, LNEW)
        CALL SADVECT(RA, A, U, V, LRHS, LNEW)
        CALL SADVECT(RSN, HSN, U, V, LRHS, LNEW)
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after sadvect, 10,000 ',
     1             LWDN
       ENDIF
C-----------------------------------------------------------------------
C  ADD IN HORIZONTAL DIFFUSION
C-----------------------------------------------------------------------
        IF (JJC.EQ.2) THEN
         CALL SDIFFUS(RH, H, LOLD)
         CALL SDIFFUS(RA, A, LOLD)
         CALL SDIFFUS(RSN, HSN, LOLD)
        END IF
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after sdiffus, 10,000 ',
     1             LWDN
       ENDIF
C-----------------------------------------------------------------------
C  CALCULATE NEW THICKNESSES AND COMPACTNESS
C-----------------------------------------------------------------------
        DO 110 J=1, MM
        DO 110 I=0, L
         H(I,J,LNEW)=H(I,J,LOLD)+DELT*RH(I,J)
         A(I,J,LNEW)=A(I,J,LOLD)+DELT*RA(I,J)
         HSN(I,J,LNEW)=HSN(I,J,LOLD)+DELT*RSN(I,J)
  110   CONTINUE
C-----------------------------------------------------------------------
C  RESET HORIZONTAL BOUNDARY CONDITIONS FOR H AND A
C-----------------------------------------------------------------------
        CALL BCSH(H, LNEW, OM)
        CALL BCSH(A, LNEW, OM)
        CALL BCSH(HSN, LNEW, OM)
  120  CONTINUE
C
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel after bcsh, 10,000 ',
     1             LWDN
       ENDIF
 9999  CONTINUE
C-----------------------------------------------------------------------
C  KINETIC ENERGY FOR MIXED LAYER, Moved to GROWTH
C-----------------------------------------------------------------------
C  NOW DO THE THERMODYNAMIC GROWTH CALCULATIONS FOR H AND A
C-----------------------------------------------------------------------
CD       PRINT *,'Calling Growth'
       IF (overload(LWDN, LP, MP, 10000.)) THEN
         PRINT *,'Overloaded LWDN in icemodel before growth, 10,000 ',
     1             LWDN
       ENDIF
CD       CALL GROWTH(LOLD, LNEW, LWDN, SWDN, bathy, hmlref,
       CALL GROWTH(LOLD, LNEW, LWDN, SWDN, bathy, 
     1  DCVM, WUP, COSGAM, RTC, STC, QTOC, SNOFLG, TICM,
     2  U, V,
     3  QTM, ATMFLX, SURTYP,
     5     QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, 
     6     QV, QRHO, QW, FW, IEN, MLFIX,
     7  IIC, H, A, HSN, 
     8  H0, ARMIN, ARMAX, HMIN,
     9  TAIR, TD, ACL, PA, UG, TA, RPREC,
     1  OM)
C-----------------------------------------------------------------------
C  SMOOTH OUTFLOW THICKNESSES AND COMPACTNESSES
C-----------------------------------------------------------------------
CD       CALL OUTSM(H, LNEW, HOSUM, HOSM(LNEW), 
CD     1 PN, PM, OM, NOUT, IOUT, JOUT)
CD       CALL OUTSM(A, LNEW, SCR1, SCR2, 
CD     1 PN, PM, OM, NOUT, IOUT, JOUT)
CD       CALL OUTSM(HSN, LNEW, HOSNSUM, HOSNSM(LNEW), 
CD     1 PN, PM, OM, NOUT, IOUT, JOUT)
C-----------------------------------------------------------------------
C  RESET HORIZONTAL BOUNDARY CONDITIONS FOR H AND A
C-----------------------------------------------------------------------
       CALL BCSH(H, LNEW, OM)
       CALL BCSH(A, LNEW, OM)
       CALL BCSH(HSN, LNEW, OM)
C-----------------------------------------------------------------------
C----- THE REST OF THE COMP. LOOP IS RESERVED FOR VARIOUS OUTPUTS ------
C-----------------------------------------------------------------------
      CALL output(
     1    FLAGI1, FLAGI2, FLAGI, 
CD     1    OM, TAUX, TAUY, TA, FLSE, TICM, QH ,
     2    OM, TAUX, TAUY, TA, TICM, QH ,
     3    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     4    CLO, T, 
     5    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  GET READY FOR NEW TIME STEP: SWITCH OLD AND NEW INDICES
C-----------------------------------------------------------------------
       LOLD=LNEW
       LNEW=3-LOLD
  400 CONTINUE
C-----------------------------------------------------------------------
C---------------------- END OF COMPUTATIONAL LOOP ----------------------
C-----------------------------------------------------------------------
      NOLRREC = 0
      WRITE (*, 980) NRREC-NOLRREC
      STOP
  980 FORMAT (' ',I5,' RESTART RECORDS WRITTEN')
      END
      SUBROUTINE BCOEF(LRHS, BU, BV, ZETA, ETA, AMAS, U, V)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS              MPI, HAMBURG                    1987
C  PURPOSE:
C     -CALCULATION OF THE SYMMETRIC COEFFICIENT CONTRIBUTION TO THE IN-
C       TERNAL ICE STRESS
C     -CALCULATION OF COORDINATE TRANSFORMATION CONTRIBUTION TO THE
C       ADVECTION TERMS
C  METHOD:
C     -SOLVES THOSE PARTS OF THE FINITE DIFFERENCE APPROXIMATION OF
C       EQS.5 AND 6 IN HIBLER (79) THAT DO NOT DEPEND ON THE VELOCITIES,
C       I.E. DIFFERENTIATES PARTIALLY THE VISCOSITIES
C  INTERFACE:
C     -LRHS: RUNNING INDEX OF OLD OR INTERMEDIATE TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LRHS
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
      REAL U(L, M, 3), V(L, M, 3)
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
      REAL AMAS(L,M), BU(L,M), BV(L,M), ZETA(L,M), ETA(L,M)
C=======================================================================
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER I, J
C=======================================================================
      DO 10 J=2,MM
      DO 10 I=2,LM
C-----------------------------------------------------------------------
C  FIRST DIFFERENTIATE THE BULK VISCOSITIES
C-----------------------------------------------------------------------
C  CONTRIBUTION TO -D[ZETA*D(U)/DX]/DX FOR U EQUATION:
       BU(I,J)=SX2*((ZETA(I,J)  +ZETA(I,J-1))
     1            /(PN(I,J)  +PN(I,J-1))  *(PM(I,J)  +PM(I,J-1))
     2             +(ZETA(I-1,J)+ZETA(I-1,J-1))
     3            /(PN(I-1,J)+PN(I-1,J-1))*(PM(I-1,J)+PM(I-1,J-1)))
C  CONTRIBUTION TO -D[ZETA*D(V)/DY]/DY FOR V EQUATION:
       BV(I,J)=SY2*((ZETA(I,J)  +ZETA(I-1,J))
     1            /(PM(I,J)  +PM(I-1,J))  *(PN(I,J)  +PN(I-1,J))
     2             +(ZETA(I,J-1)+ZETA(I-1,J-1))
     3            /(PM(I,J-1)+PM(I-1,J-1))*(PN(I,J-1)+PN(I-1,J-1)))
C-----------------------------------------------------------------------
C  NEXT DIFFERENTIATE THE SHEAR VISCOSITIES
C-----------------------------------------------------------------------
C  ADD CONTRIBUTION TO -D[ETA*D(U)/DX]/DX TO U EQUATION:
       BU(I,J)=BU(I,J)+SX2*((ETA(I,J)  +ETA(I,J-1))
     1                /(PN(I,J)  +PN(I,J-1))  *(PM(I,J)  +PM(I,J-1))
     2                     +(ETA(I-1,J)+ETA(I-1,J-1))
     3                /(PN(I-1,J)+PN(I-1,J-1))*(PM(I-1,J)+PM(I-1,J-1)))
C  ADD CONTRIBUTION TO -D[ETA*D(V)/DY]/DY TO V EQUATION:
       BV(I,J)=BV(I,J)+SY2*((ETA(I,J)  +ETA(I-1,J))
     1                /(PM(I,J)  +PM(I-1,J))  *(PN(I,J)  +PN(I-1,J))
     2                     +(ETA(I,J-1)+ETA(I-1,J-1))
     3                /(PM(I,J-1)+PM(I-1,J-1))*(PN(I,J-1)+PN(I-1,J-1)))
C  ADD CONTRIBUTION TO -D[ETA*D(U)/DY]/DY TO U EQUATION:
       BU(I,J)=BU(I,J)+SY2*((ETA(I,J)  +ETA(I-1,J))
     1                /(PM(I,J)  +PM(I-1,J))  *(PN(I,J)  +PN(I-1,J))
     2                     +(ETA(I,J-1)+ETA(I-1,J-1))
     3                /(PM(I,J-1)+PM(I-1,J-1))*(PN(I,J-1)+PN(I-1,J-1)))
C  ADD CONTRIBUTION TO -D[ETA*D(V)/DX]/DX TO V EQUATION:
       BV(I,J)=BV(I,J)+SX2*((ETA(I,J)  +ETA(I,J-1))
     1                /(PN(I,J)  +PN(I,J-1))  *(PM(I,J)  +PM(I,J-1))
     2                     +(ETA(I-1,J)+ETA(I-1,J-1))
     3                /(PN(I-1,J)+PN(I-1,J-1))*(PM(I-1,J)+PM(I-1,J-1)))
C-----------------------------------------------------------------------
C  ADD IN CONTRIBUTION TO ADVECTION DUE TO COORDINATE TRANSFORMATION
C-----------------------------------------------------------------------
C  ADD COEFFICIENT FOR M*U*D(U)/DX:
       BU(I,J)=BU(I,J)+AMAS(I,J)*U(I,J,LRHS)
     1        *(1./(PN(I,J)+PN(I,J-1))-1./(PN(I-1,J)+PN(I-1,J-1)))/DX
C  ADD COEFFICIENT FOR M*V*D(U)/DY:
       BU(I,J)=BU(I,J)+AMAS(I,J)*V(I,J,LRHS)
     1        *(1./(PM(I,J)+PM(I-1,J))-1./(PM(I,J-1)+PN(I-1,J-1)))/DY
C  ADD COEFFICIENT FOR M*U*D(V)/DX:
       BV(I,J)=BV(I,J)+AMAS(I,J)*U(I,J,LRHS)
     1        *(1./(PN(I,J)+PN(I,J-1))-1./(PN(I-1,J)+PN(I-1,J-1)))/DX
C  ADD COEFFICIENT FOR M*V*D(V)/DY:
       BV(I,J)=BV(I,J)+AMAS(I,J)*V(I,J,LRHS)
     1        *(1./(PM(I,J)+PM(I-1,J))-1./(PM(I,J-1)+PN(I-1,J-1)))/DY
   10 CONTINUE
      RETURN
      END
      SUBROUTINE BCSFLX(FX,FY)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -SETS FLUXES NORMAL TO BOUNDARIES TO ZERO EXCEPT WHERE CYCLIC
C       BOUNDARY CONDITIONS APPLY
C  METHOD:
C     -USES FLUX MASK DETERMINED IN SUBROUTINE BCSINIT
C  INTERFACE:
C     -FX: FLUX IN X-DIRECTION
C     -FY: FLUX IN Y-DIRECTION
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
      REAL FX(0:L,0:M), FY(0:L,0:M)
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  SET DIFFUSIVE FLUXES ACROSS THE BOUNDARIES TO 0
C-----------------------------------------------------------------------
      DO 10 J=0,M
      DO 10 I=0,L
       FX(I,J)=FLM(I,J,1)*FX(I,J)
       FY(I,J)=FLM(I,J,2)*FY(I,J)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BOUNDARY CONDITIONS AS NEEDED
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 3) GO TO 9999
      DO 30 J=2,MM
       FX(1,J)=FX(LM,J)
       FY(1,J)=FY(LM,J)
       FX(L,J)=FX(2,J)
       FY(L,J)=FY(2,J)
   30 CONTINUE

 9999 CONTINUE

      RETURN
      END
      SUBROUTINE DDX(U, V, ETA, LN, K, TMP)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINES FELLD,FELLD1,FELLIP IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  LAST MODIFIED: 16 Augusg 1994.
C  PURPOSE:
C     -CALCULATES THE X-DERIVATIVES OF THAT PART OF THE INTERNAL ICE
C       STRESS WHICH DEPENDS ON THE CURRENT VELOCITIES
C  METHOD:
C     -SYMMETRIC DERIVATIVES ARE PARTLY SOLVED (THE REST WAS SOLVED IN
C       SUBROUTINE BCOEF), WHILE THE MIXED DERIVATIVES ARE COMPLETELY
C       SOLVED
C     -NOTE: FOR THE MIXED DERIVATIVES THE METRIC TERMS ASSOCIATED WITH
C       1/DY AND THE SIDE OF THE CELL USED FOR THE FLUX CANCEL, SO THIS
C       IS THE SAME AS THE CARTESIAN VERSION
C  INTERFACE:
C     -U:   X-COMPONENT OF ICE VELOCITY
C     -V:   Y-COMPONENT OF ICE VELOCITY
C     -ETA: BULK OR SHEAR VISCOSITY
C     -LN:  RUNNING INDEX FOR CURRENT TIME STEP
C     -K:   INDEX FOR ALTERNATING PATTERN
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LN, K
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
      REAL TMP(L,M)
C=======================================================================
C     -TMP: TEMPORARY ARRAY
C=======================================================================
      REAL U(L,M,3), V(L,M,3), ETA(L,M)
      INTEGER I, J, IB
C=======================================================================
CD      PRINT *,'Entered ddx'
      DO 10 J=2,MM
       IB=MOD(J+K,2)+2
       DO 10 I=IB,LM,2
C  CALCULATE THE CONTRIBUTION TO D[ETA*D(U)/DX]/DX:
        TMP(I,J)=SX2*((ETA(I,J)  +ETA(I,J-1))  *U(I+1,J,LN)
     1               *(PM(I,J)  +PM(I,J-1))  /(PN(I,J)  +PN(I,J-1))
     2               +(ETA(I-1,J)+ETA(I-1,J-1))*U(I-1,J,LN)
     3               *(PM(I-1,J)+PM(I-1,J-1))/(PN(I-1,J)+PN(I-1,J-1)))
C  CALCULATE D[ETA*D(V)/DY]/DX:
        TMP(I+1,J)=SXY
     1             *(ETA(I,J)    *((V(I,J+1,LN)  +V(I+1,J+1,LN))
     2                            -(V(I,J,LN)    +V(I+1,J,LN)))
     3              +ETA(I,J-1)  *((V(I,J,LN)    +V(I+1,J,LN))
     4                            -(V(I,J-1,LN)  +V(I+1,J-1,LN)))
     5              -ETA(I-1,J)  *((V(I-1,J+1,LN)+V(I,J+1,LN))
     6                            -(V(I-1,J,LN)  +V(I,J,LN)))
     7              -ETA(I-1,J-1)*((V(I-1,J,LN)  +V(I,J,LN))
     8                            -(V(I-1,J-1,LN)+V(I,J-1,LN))))
   10 CONTINUE
      RETURN
C=======================================================================
      ENTRY DDY(U, V, ETA, LN, K, TMP)
C=======================================================================
C  PURPOSE:
C     -CALCULATES THE Y-DERIVATIVES OF THAT PART OF THE INTERNAL ICE
C       STRESS WHICH DEPENDS ON THE CURRENT VELOCITIES
C=======================================================================
CD      PRINT *,'Entered ddy'
      DO 110 J=2,MM
       IB=MOD(J+K,2)+2
       DO 110 I=IB,LM,2
C  CALCULATE THE CONTRIBUTION TO D[ETA*D(U)/DY]/DY:
        TMP(I,J)=SY2*((ETA(I,J)  +ETA(I-1,J))  *U(I,J+1,LN)
     1               *(PN(I,J)   +PN(I-1,J))  /(PM(I,J)+PM(I-1,J))
     2               +(ETA(I,J-1)+ETA(I-1,J-1))*U(I,J-1,LN)
     3               *(PN(I,J-1) +PN(I-1,J-1))/(PM(I,J-1)+PM(I-1,J-1)))
C  CALCULATE D[ETA*D(V)/DX]/DY:
        TMP(I+1,J)=SXY
     1             *(ETA(I,J)    *((V(I+1,J,LN)  +V(I+1,J+1,LN))
     2                            -(V(I,J,LN)    +V(I,J+1,LN)))
     3              +ETA(I-1,J)  *((V(I,J,LN)    +V(I,J+1,LN))
     4                            -(V(I-1,J,LN)  +V(I-1,J+1,LN)))
     5              -ETA(I,J-1)  *((V(I+1,J-1,LN)+V(I+1,J,LN))
     6                            -(V(I,J-1,LN)  +V(I,J,LN)))
     7              -ETA(I-1,J-1)*((V(I,J-1,LN)  +V(I,J,LN))
     8                            -(V(I-1,J-1,LN)+V(I-1,J,LN))))
  110 CONTINUE
      RETURN
      END
CD      SUBROUTINE INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, DT,
      SUBROUTINE INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, 
     1    SURTYP, SURFWIN)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     Robert Grumbine          NCEP, Camp Springs, MD               1993
C     Robert Grumbine          NCEP Camp Springs, MD         19 Feb 1997
C  LAST MODIFIED: 6 January 1993
C  LAST MODIFIED: 19 February 1997
C    -- Removed common blocks  
C  PURPOSE:
C     -PERFORMES INITIAL RELAXATION TO BALANCE INITIAL FIELDS
C  METHOD:
C     -SOLVES THE MOMENTUM EQUATION NEGLECTING THE INERTIAL TERMS
C  EXTERNALS:
C     -OUTBCS: SETS VISCOSITIES AND ICE STRENGTH TO 0 AT OUTFLOW POINTS
C     -RELCON: CALCULATES DIAGNOSTIC TERMS OF MOMENTUM BALANCE
C     -RELAX:  SOLVES MOMENTUM BALANCE BY OVERRELAXATION
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
CD      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
CD      REAL T
CD      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
CD      COMMON/VEL/U(L,M,3),V(L,M,3)
      REAL U(L, M, 3), V(L, M, 3)
CD      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
CD      REAL H, A, HSN
      REAL H(0:L, 0:M, 2)
CD      COMMON/WORK/TRK(1:L,1:M,4),AMAS(1:L,1:M),BU(1:L,1:M),BV(1:L,1:M),
CD     1FX(1:L,1:M),FY(1:L,1:M),
CD     1ASY(1:L,1:M),ZETA(1:L,1:M),ETA(1:L,1:M)
CD      REAL TRK, AMAS, BU, BV, FX, FY, ASY, ZETA, ETA
      REAL AMAS(L, M), BU(L, M), BV(L, M), FX(L, M), FY(L, M)
      REAL ZETA(L, M), ETA(L, M)
      REAL SURTYP, SURFWIN
C=======================================================================
C     -TRK:  DUMMY REGISTERS
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC TERMS OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC TERMS OF THE MOM.EQ.
C     -FX:   X-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -FY:   Y-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -ASY:  ASYMMETRIC TERMS OF THE MOMENTUM EQUATION
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  SET UP INITIAL VISCOSITIES
C-----------------------------------------------------------------------
      DO 10 J=1,MM
      DO 10 I=1,L
       ZETA(I,J)=H(I,J,1)*1.E+11
       ETA(I,J)=ZETA(I,J)/4.0
   10 CONTINUE
C-----------------------------------------------------------------------
C  SET VISCOSITIES AND ICE STRENGTH TO 0 AT OUTFLOW GRID CELLS
C-----------------------------------------------------------------------
      CALL OUTBCS(ETA, ZETA)
C-----------------------------------------------------------------------
C  SET UP DIAGNOSTIC PART OF THE MOMENTUM EQUATION
C-----------------------------------------------------------------------
      CALL RELCON(1,1, U, V, BU, BV, AMAS, FX, FY, H, 
     1              ETA, ZETA, SURTYP, SURFWIN)
C-----------------------------------------------------------------------
C  DELETE THE ACCELERATION TERMS
C-----------------------------------------------------------------------
      DO 20 J=2,MM
      DO 20 I=1,L
       IF (BU(I,J) .NE. 0)
     1       BU(I,J)=1./(1./BU(I,J)-AMAS(I,J)/DT)
       IF (BV(I,J) .NE. 0)
     1       BV(I,J)=1./(1./BV(I,J)-AMAS(I,J)/DT)
       FX(I,J)=FX(I,J)-AMAS(I,J)*U(I,J,1)/DT
       FY(I,J)=FY(I,J)-AMAS(I,J)*V(I,J,1)/DT
       AMAS(I,J)=0.0
   20 CONTINUE
C-----------------------------------------------------------------------
C  SOLVE THE MOMENTUM EQUATION BY OVERRELAXATION (ONLY ONCE)
C-----------------------------------------------------------------------
      CALL RELAX(1,2, U, V, BU, BV, AMAS, FX, FY, H, 
     1             ETA, ZETA)
      DO 30 J=1,M
      DO 30 I=1,L
       U(I,J,1)=U(I,J,2)
       V(I,J,1)=V(I,J,2)
   30 CONTINUE

      RETURN
      END
      SUBROUTINE MADV(LR, LN, K, RU, RV, AMAS, U, V)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  PURPOSE:
C     -CALCULATION OF HORIZONTAL ADVECTION TERMS
C  METHOD:
C     -STRAIGHTFORWARD WITH CENTERED DIFFERENCES
C     -NOTE: WE CAN NOT USE THE FLUX FORM AS IS THE NORMAL PRACTICE
C       SINCE THE FLOW CAN BE COMPRESSIBLE
C  INTERFACE:
C     -LR:  RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C     -LN:  RUNNING INDEX FOR INTERMEDIATE OR NEW TIME STEP
C     -K:   INDEX FOR ALTERNATING PATTERN
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LR, LN, K
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
      REAL U(L, M, 3), V(L, M, 3)
      REAL RU(L,M), RV(L,M), AMAS(L,M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -RU:   X-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -RV:   V-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -AMAS: ICE MASS
C=======================================================================
      INTEGER I, J, IB
      REAL TMP(L,M)
C=======================================================================
      DO 10 J=2,MM
       IB=MOD(J+K,2)+2
       DO 10 I=IB,LM,2
C  ADD IN -M*U*D(U)/DX-M*V*D(U)/DY TO U EQUATION:
        RU(I,J)=RU(I,J)-AMAS(I,J)*U(I,J,LR)
     1                  *(U(I+1,J,LN)/(PN(I,J)  +PN(I,J-1))
     2                   -U(I-1,J,LN)/(PN(I-1,J)+PN(I-1,J-1)))/DX
     3                 -AMAS(I,J)*V(I,J,LR)
     4                  *(U(I,J+1,LN)/(PM(I,J)  +PM(I-1,J))
     5                   -U(I,J-1,LN)/(PM(I,J-1)+PM(I-1,J-1)))/DY
C  ADD IN -M*U*D(V)/DX-M*V*D(V)/DY TO V EQUATION:
        RV(I,J)=RV(I,J)-AMAS(I,J)*U(I,J,LR)
     1                  *(V(I+1,J,LN)/(PN(I,J)  +PN(I,J-1))
     2                   -V(I-1,J,LN)/(PN(I-1,J)+PN(I-1,J-1)))/DX
     3                 -AMAS(I,J)*V(I,J,LR)
     4                  *(V(I,J+1,LN)/(PM(I,J)  +PM(I-1,J))
     5                   -V(I,J-1,LN)/(PM(I,J-1)+PM(I-1,J-1)))/DY
C  ADJUST FOR COORDINATE TRANSFORMATION:
        TMP(I,J)=0.25*(PM(I,J)  *PN(I,J)  +PM(I,J-1)  *PN(I,J-1)
     1                +PM(I-1,J)*PN(I-1,J)+PM(I-1,J-1)*PN(I-1,J-1))
        RU(I,J)=RU(I,J)*TMP(I,J)
        RV(I,J)=RV(I,J)*TMP(I,J)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE OUTBCS(ETA, ZETA)
      IMPLICIT none
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
C  PURPOSE:
C     -SETS FORCE DUE TO INTERNAL ICE STRESS TO 0 AT OUTFLOW POINTS
C  METHOD:
C     -VISCOSITIES AND ICE STRENGTH ARE SET TO 0 AT OUTFLOW POINTS
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      COMMON/PRESS/P(L,M)
      REAL P
      COMMON/OUTFLOW/NOUT,IOUT(LDO),JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
CD      COMMON/WORK/WRK(1:L,1:M,10),ZETA(1:L,1:M),ETA(1:L,1:M)
CD      REAL WRK, ZETA, ETA
      REAL ZETA(L, M), ETA(L, M)
C=======================================================================
C     -WRK:  DUMMY REGISTERS
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER N, I, J
C=======================================================================
      DO 210 N=1,NOUT
       I=IOUT(N)
       J=JOUT(N)
       ETA(I,J)=0.0
       ZETA(I,J)=0.0
       P(I,J)=0.0
  210 CONTINUE
      RETURN
      END
      SUBROUTINE PLAST(LRHS, E11, E22, E12, ZETA, ETA, U, V, H, A, HSN)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE PLAST IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  PURPOSE:
C     -CALCULATION OF VISCOSITIES
C  METHOD:
C     -USES STRAIN RATES CALCULATED IN SUBROUTINE STRAIN
C     -USES ICE STRENGTH DETERMINED IN SUBROUTINE PRESSB
C     -DETERMINES STRAIN RATE INVARIANTS (EQ.(9) IN HIBLER (1979))
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C  EXTERNALS:
C     -STRAIN: CALCULATES STRAIN RATE TENSOR
C     -OUTBCS: SETS VISCOSITIES AND ICE STRENGTH TO 0 AT OUTFLOW POINTS
C  LAST MODIFIED: 2 September 1993.
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      INTEGER LRHS
CD      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/PRESS/P(L,M)
      REAL P
CD      COMMON/WORK/TRK(1:L,1:M), E11(1:L,1:M), E22(1:L,1:M), 
CD     1 E12(1:L,1:M), WRK(1:L,1:M,6),
CD     2 ZETA(1:L,1:M), ETA(1:L,1:M)
CD      REAL TRK, E11, E22, E12, WRK, ZETA, ETA
      REAL E11(L,M), E22(L,M), E12(L,M), ZETA(L,M), ETA(L,M)
      REAL U(L,M,3), V(L,M,3)
C=======================================================================
C**REMARK: THIS FORM OF THE COMMON BLOCK INSURES THAT ZETA AND ETA ARE
C    CORRECTLY PASSED TO SUBROUTINES RELCON AND RELAX
C     -TRK:  DUMMY REGISTER
C     -E11:  X-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E22:  Y-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E12:  DEVIATORIC (SHEAR) STRAIN RATE
C     -WRK:  DUMMY REGISTERS
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      REAL TMP(L,M), FLG(L,M)
C=======================================================================
C     -TMP: TEMPORARY REGISTER
C     -FLG: FLAGS FOR CHANGING APPLICATIONS
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  CALCULATE STRAIN RATE TENSOR
C-----------------------------------------------------------------------
CD      PRINT *,'Entered plast'
      CALL STRAIN(LRHS, E11, E12, E22, U, V)
      DO 10 J=1,MM
      DO 10 I=1,L
C-----------------------------------------------------------------------
C  DETERMINE THE 'DELTA' IN EQ.9 OF HIBLER (79)
C-----------------------------------------------------------------------
       TMP(I,J)=SQRT((E11(I,J)**2+E22(I,J)**2)*(1.0+ECM2)
     1 +4.0*ECM2*E12(I,J)**2+2.*E11(I,J)*E22(I,J)*(1.-ECM2))
C-----------------------------------------------------------------------
C  USE THE MAXIMUM VISCOUS CREEP RATE IF DELTA IS SMALLER
C-----------------------------------------------------------------------
       FLG(I,J)=0.5*(1.+SIGN(1.,(TMP(I,J)-GMIN)))
       TMP(I,J)=FLG(I,J)*TMP(I,J)+(1.-FLG(I,J))*GMIN
C  BULK VISCOSITY:
       ZETA(I,J)=0.5*P(I,J)/TMP(I,J)
C-----------------------------------------------------------------------
C  USE MIN AND MAX BULK VISCOSITIES TO CONSTRAIN THE VISCOSITIES - 
C   Relict experiment, deleted per A. Stossel comment of 11 Mar 1993.
C-----------------------------------------------------------------------
C  SHEAR VISCOSITY (EQ.11 IN HIBLER (79)):
       ETA(I,J)=ECM2*ZETA(I,J)
   10 CONTINUE
C-----------------------------------------------------------------------
C  NOW SET VISCOSITIES AND PRESSURE EQUAL TO ZERO AT OUTFLOW PTS
C-----------------------------------------------------------------------
      CALL OUTBCS(ETA, ZETA)

CD      PRINT *,'Leaving plast'
      RETURN
      END
      SUBROUTINE PRESSB(LOLD, P, H, A)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE PLAST IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     R. W. Grumbine           NMC, Camp Springs, MD                1992
C  PURPOSE:
C     -CALCULATION OF ICE STRENGTH (EQ.17 IN HIBLER (79))
C  METHOD:
C     -USES HIBLER (1979) EQUATION OF STATE
C  INTERFACE:
C     -LOLD: RUNNING INDEX FOR OLD TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      REAL  H(0:L,0:M,2), A(0:L,0:M,2)
      REAL  P(L,M)
      INTEGER LOLD, I, J
C=======================================================================
CD      DO 200 J=1,MM
CBG   Try filling while P grid

      IF (LOLD .GT. 2 .OR. LOLD .LT. 1) THEN
        PRINT *,'Lold in Pressb = ', LOLD
        PRINT *,'Cannot continue in routine, setting values to zero'
        DO 100 J = 1, M
        DO 100 I = 1, L
          P(I,J) = 0.0
  100   CONTINUE
        
       ELSE
        DO 200 J=1,M
        DO 200 I=1,L
          P(I,J)=PSTAR*H(I,J,LOLD)*EXP(-CSTAR*(1.0-A(I,J,LOLD)))
  200   CONTINUE
      ENDIF

      RETURN
      END
      SUBROUTINE RELAX(LR,LN, U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA)
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE RELAX IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  LAST MODIFIED: 16 August 1994.
C  LAST MODIFIED: 21 February 1997
C  PURPOSE:
C     -CALCULATES THE ICE VELOCITIES
C     -SOLVES THAT PART OF THE MOMENTUM EQUATION THAT DEPENDS ON THE
C       VELOCITIES AT THE CURRENT TIME STEP
C  METHOD:
C     -SUCCESIVE OVERRELAXATION WITH CHEBYSHEV ACCELERATION (HOCKNEY
C       AND JESSHOPE (1981), PGS.334-341
C     -IN THIS CASE WE ARE UPDATING A CHECKERBOARD, ALTERNATING COLORS,
C       IE FIRST RED, THEN BLACK
C     -THE VELOCITY ARRAYS WILL BE USED AS FOLLOWS:
C       U OR V(I,J,LN)=VELOCITIES AT THE INTERMEDIATE OR NEW TIME STEP
C       U OR V(I,J,LR)=VELOCITIES AT THE OLD OR INTERMEDIATE TIME STEP
C     -WE ARE SOLVING THE EQUATIONS:
C       BU*U-ASY*V=RU+FX
C       BV*V+ASY*U=RV+FY
C       WHICH AFTER TAKING THE RECIPROCAL OF THE SYMMETRIC COEFFICIENTS
C       (BU,BV) AT THE END OF SUBROUTINE RELCON AND SOLVING FOR U AND V
C       YIELDS:
C       U-(ASY*BU)*V=(RU+FX)*BU
C       V+(ASY*BU)*U=(RV+FY)*BV
C  INTERFACE:
C     -LR: RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C     -LN: RUNNING INDEX FOR INTERMEDIATE OR NEW TIME STEP
C  EXTERNALS:
C     -DDX:  CALCULATES X-DERIVATIVES FOR THE INTERNAL ICE STRESS
C     -DDY:  CALCULATES Y-DERIVATIVES FOR THE INTERNAL ICE STRESS
C     -MADV: CALCULATES THE ADVECTION TERMS
C     -BCSV: SETS CYCLIC BOUNDARY CONDITIONS FOR VELOCITIES
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      REAL H(0:L,0:M,2)
      INTEGER LR, LN
C=======================================================================
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
CD      COMMON/VEL/U(L,M,3), V(L,M,3)
      REAL U(L,M,3), V(L,M,3)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
CD      COMMON/WORK/TMP(1:L,1:M), RU(1:L,1:M), RV(1:L,1:M), DEN(1:L,1:M),
CD     1  AMAS(1:L,1:M), BU(1:L,1:M), BV(1:L,1:M), FX(1:L,1:M), 
CD     2  FY(1:L,1:M), ASY(1:L,1:M), ZETA(1:L,1:M), ETA(1:L,1:M)
      REAL AMAS(L,M), BU(L,M), BV(L,M), FX(L,M), FY(L,M), ASY(L,M)
      REAL ETA(L,M), ZETA(L,M)
      REAL TMP(L,M), RU(L,M), RV(L,M), DEN(L,M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -RU:   X-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -RV:   V-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -DEN:  DENOMINATOR OF THE SOLUTION EQUATION
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -FX:   X-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -FY:   Y-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -ASY:  ASYMMETRIC COEFFICIENTS OF THE MOMENTUM EQUATION
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER I, J
      INTEGER MRELAX, K, IB
      REAL WTA, VERR
C-----------------------------------------------------------------------
C  MAKE THE FIRST GUESS FOR THE VELOCITIES
C-----------------------------------------------------------------------
CD      PRINT *,'Entered relax, iteration limit = ', MMAX
      DO 10 J=2,MM
      DO 10 I=2,LM
       U(I,J,LN)=U(I,J,LR)
       V(I,J,LN)=V(I,J,LR)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE DENOMINATOR OF THE SOLUTION EQUATION
C-----------------------------------------------------------------------
      DO 20 J=2,MM
      DO 20 I=2,LM
        DEN(I,J)=1./(1.+(VM(I,J)*ASY(I,J))**2*BU(I,J)*BV(I,J))
   20 CONTINUE
C-----------------------------------------------------------------------
C  START THE ITERATION AT 0
C-----------------------------------------------------------------------
      MRELAX=0
C-----------------------------------------------------------------------
C  START OF RELAXATION LOOP
C-----------------------------------------------------------------------
  120 CONTINUE
C  STOP IF MAXIMUM NUMBER OF ITERATIONS IS EXCEEDED:
      IF (MRELAX.GT.MMAX) GOTO 400
CD      PRINT *,'On iteration ', MRELAX
C-----------------------------------------------------------------------
C  DETERMINE THE RELAXATION FACTOR FOR THE RESIDUAL
C-----------------------------------------------------------------------
      IF ((MRELAX.GT. 0.667*MMAX).OR.(MRELAX.EQ.0))THEN
       WTA=1.0
      ELSE
       WTA=WT
      END IF
C-----------------------------------------------------------------------
C  DO RELAXATION ALTERNATING COLORS FOR CHECKERBOARD PATTERN
C-----------------------------------------------------------------------
      DO 170 K=0,1
C-----------------------------------------------------------------------
C  FIRST DIFFERENTIATE THE BULK VISCOSITIES
C-----------------------------------------------------------------------
C  ENTER D{ZETA*[D(U)/DX+D(V)/DY]}/DX TO U EQUATION:
       CALL DDX(U, V, ZETA, LN, K, TMP)
       DO 125 J=2,MM
        IB=MOD(J+K,2)+2
        DO 125 I=IB,LM,2
        RU(I,J)=TMP(I,J)+TMP(I+1,J)
  125  CONTINUE
C  ENTER D{ZETA*[D(U)/DX+D(V)/DY]}/DY TO V EQUATION:
       CALL DDY(V, U, ZETA, LN, K, TMP)
       DO 130 J=2,MM
        IB=MOD(J+K,2)+2
        DO 130 I=IB,LM,2
        RV(I,J)=TMP(I,J)+TMP(I+1,J)
  130  CONTINUE
C-----------------------------------------------------------------------
C  NEXT DIFFERENTIATE THE SHEAR VISCOSITIES
C-----------------------------------------------------------------------
C  ADD D{ETA*[D(U)/DX-D(V)/DY]}/DX TO U EQUATION:
       CALL DDX(U, V, ETA, LN, K, TMP)
       DO 135 J=2,MM
        IB=MOD(J+K,2)+2
        DO 135 I=IB,LM,2
        RU(I,J)=RU(I,J)+TMP(I,J)-TMP(I+1,J)
  135  CONTINUE
C  ADD D{ETA*[D(V)/DY-D(U)/DX]}/DY TO V EQUATION:
       CALL DDY(V, U, ETA, LN, K, TMP)
       DO 140 J=2,MM
        IB=MOD(J+K,2)+2
        DO 140 I=IB,LM,2
        RV(I,J)=RV(I,J)+TMP(I,J)-TMP(I+1,J)
  140  CONTINUE
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DY TO U EQUATION:
       CALL DDY(U, V, ETA, LN, K, TMP)
       DO 145 J=2,MM
        IB=MOD(J+K,2)+2
        DO 145 I=IB,LM,2
        RU(I,J)=RU(I,J)+TMP(I,J)+TMP(I+1,J)
  145  CONTINUE
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DX TO V EQUATION:
       CALL DDX(V, U, ETA, LN, K, TMP)
       DO 150 J=2,MM
        IB=MOD(J+K,2)+2
        DO 150 I=IB,LM,2
        RV(I,J)=RV(I,J)+TMP(I,J)+TMP(I+1,J)
  150  CONTINUE
C-----------------------------------------------------------------------
C  ADD IN THE ADVECTION TERMS
C-----------------------------------------------------------------------
       CALL MADV(LR,LN,K, RU, RV, AMAS, U, V)
C-----------------------------------------------------------------------
C  CALCULATE CHANGES IN VELOCITIES
C-----------------------------------------------------------------------
       DO 155 J=2,MM
        IB=MOD(J+K,2)+2
        DO 155 I=IB,LM,2
C  ADD THE OTHER PARTS OF THE MOM.EQ. AND DIVIDE BY THE SYMMETRIC COEFF:
         TMP(I,J)  =VM(I,J)*(RU(I,J)+FX(I,J))*BU(I,J)
         TMP(I+1,J)=VM(I,J)*(RV(I,J)+FY(I,J))*BV(I,J)
C  SOLVE FOR THE NEXT GUESS OF THE VELOCITIES:
         RU(I,J)=(TMP(I,J)+ASY(I,J)*BU(I,J)*TMP(I+1,J))*DEN(I,J)
         RV(I,J)=(TMP(I+1,J)-ASY(I,J)*BV(I,J)*TMP(I,J))*DEN(I,J)
C  DETERMINE THE RESIDUAL
         RU(I,J)=WTA*(RU(I,J)-U(I,J,LN))
         RV(I,J)=WTA*(RV(I,J)-V(I,J,LN))
  155  CONTINUE
C-----------------------------------------------------------------------
C  UPDATE THE VELOCITIES
C-----------------------------------------------------------------------
       DO 160 J=2,MM
        IB=MOD(J+K,2)+2
        DO 160 I=IB,LM,2
         U(I,J,LN)=U(I,J,LN)+RU(I,J)
         V(I,J,LN)=V(I,J,LN)+RV(I,J)
  160  CONTINUE
C-----------------------------------------------------------------------
C  SET CYCLIC BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
       CALL BCSV(U,V,LN, VM)
  170 CONTINUE
C-----------------------------------------------------------------------
C  CHECK TO SEE IF CHANGES IN VELOCITIES ARE SMALL ENOUGH FOR US TO STOP
C-----------------------------------------------------------------------
      MRELAX=MRELAX+1
      VERR=0.0
      DO 180 J=2,MM
      DO 180 I=2,LM
       VERR=AMAX1(ABS(RU(I,J)),VERR)
       VERR=AMAX1(ABS(RV(I,J)),VERR)
  180 CONTINUE

      IF (VERR.GT.VRMAX) GOTO 120
      PRINT *,'Relax took ',MRELAX,' iterations to converge '
C**WE HAVE SUCCEEDED IN FINDING A NEW SOLUTION:
      RETURN

C**WE HAVE NOT FOUND A SOLUTION AFTER MMAX ITERATIONS:
  400 CONTINUE
      WRITE (*,2000) IIC,MRELAX
 2000 FORMAT ('At time step ',I3,' after ',I3,' iterations, no solution
     1 found.  Zeroing velocity field')
C     If no solution is found, continue with the old velocity field.
C     Bob Grumbine 17 July 1994.
C     No, If no solution is found, zero the velocity field, due to
C       the time stepping procedure.  24 February 1997
      DO 8000 j = 1, M
        DO 8100 i = 1, L
CBG          U(i,j,LN) = U(i,j,LR)
CBG          V(i,j,LN) = V(i,j,LR)
          U(i,j,LN) = 0.0
          V(i,j,LN) = 0.0
 8100   CONTINUE
 8000 CONTINUE

CD      PRINT *,'Leaving relax'
      RETURN
      END
      SUBROUTINE RELCON(LOLD,LRHS, U, V, BU, BV, AMAS, FX, FY, H, 
     1       ETA, ZETA, SURTYP, SURFWIN)
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE FORM IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  PURPOSE:
C     -LINEARIZATION OF THE MOMENTUM EQUATION TO BE SOLVED IN SUBROUTINE
C       RELAX
C  METHOD:
C     -THIS ROUTINE CALCULATES THOSE PARTS OF THE MOMENTUM EQUATIONS
C       THAT DO NOT DEPEND ON THE U AND V VALUES OF THE NEW TIME STEP
C     -ADDITIONALLY, THE ROUTINE PROVIDES THE SYMMETRICAL AND ASYM-
C       METRICAL COEFFICIENTS FOR U AND V AT THE NEW TIME STEP
C  INTERFACE:
C     -LOLD: RUNNING INDEX FOR OLD TIME STEP
C     -LRHS: RUNNING INDEX FOR THE OLD OR INTERMEDIATE TIME STEP
C  EXTERNALS:
C     -BCOEF: CALCULATES DERIVATIVES OF VISCOSITIES
C  LAST MODIFIED: 27 January 1993
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================

      INTEGER LOLD, LRHS
      
CD      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      REAL SURTYP, SURFWIN

      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      REAL FM, F, COSPHI, SINPHI

      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN

      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY

      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
CD      COMMON/VEL/U(L,M,3), V(L,M,3)
      REAL U(L, M, 3), V(L, M, 3)

CD      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL H(0:L, 0:M, 2)

      COMMON/FRWND/CDWIN, SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN

      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      REAL SINWAT, COSWAT, UWAT, VWAT

      COMMON/PRESS/P(L,M)
      REAL P

      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY

      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      REAL CD, SINBET, COSBET, BETA, TAUX, TAUY

CD      COMMON/WORK/TMP(1:L,1:M), WRK(1:L,1:M,3), AMAS(1:L,1:M),
CD     1  BU(1:L,1:M), BV(1:L,1:M), FX(1:L,1:M),
CD     1  FY(1:L,1:M), ASY(1:L,1:M), ZETA(1:L,1:M), ETA(1:L,1:M)
      REAL AMAS(L,M), BU(L,M), BV(L,M), FX(L,M), FY(L,M), ASY(L,M)
      REAL ZETA(L,M), ETA(L,M)

      REAL TMP(L,M)
      INTEGER I, J
      
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -WRK:  DUMMY ARRAYS
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -FX:   X-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -FY:   Y-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -ASY:  ASYMMETRIC COEFFICIENTS OF THE MOMENTUM EQUATION
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
C-----------------------------------------------------------------------
C  CALCULATE THE ICE MASS
C-----------------------------------------------------------------------
CD      PRINT *,'Entered relcon'
      DO 10 J=2,MM
      DO 10 I=1,L
        AMAS(I,J)=RHOICE*0.25*(H(I-1,J,LOLD)  +H(I,J,LOLD)
     1                      +H(I-1,J-1,LOLD)+H(I,J-1,LOLD))
   10 CONTINUE
C-----------------------------------------------------------------------
C  DET.SYM.PARTS OF INT.ICE STRESS AND ADV.CONTR.DUE TO COORD.TRANSFORM.
C-----------------------------------------------------------------------
      CALL BCOEF(LRHS, BU, BV, ZETA, ETA, AMAS, U, V)
C-----------------------------------------------------------------------
C  START LOOP OVER ALL POINTS FOR THE OTHER TERMS
C-----------------------------------------------------------------------
      DO 600 J=2,MM
C     Loop index changed to avoid referencing non-extant
C       element of P. BG.
         DO 600 I = 2,L
C-----------------------------------------------------------------------
C  ADD IN THE ICE STRENGTH
C-----------------------------------------------------------------------
C  ADD D(-P/2)/DX TO U EQUATION:
       FX(I,J)=-0.5*((P(I,J)  +P(I,J-1))  /(PN(I,J)  +PN(I,J-1))
     1              -(P(I-1,J)+P(I-1,J-1))/(PN(I-1,J)+PN(I-1,J-1)))/DX
C  ADD D(-P/2)/DY TO V EQUATION:
       FY(I,J)=-0.5*((P(I,J)  +P(I-1,J))  /(PM(I,J)  +PM(I-1,J))
     2              -(P(I,J-1)+P(I-1,J-1))/(PM(I,J-1)+PM(I-1,J-1)))/DY
C-----------------------------------------------------------------------
C  MULTIPLY THE APPROPRIATE COORDINATE TRANSFORMATION TERMS
C-----------------------------------------------------------------------
       TMP(I,J)=0.25*(PM(I,J)*PN(I,J)    +PM(I-1,J)*PN(I-1,J)
     1               +PM(I,J-1)*PN(I,J-1)+PM(I-1,J-1)*PN(I-1,J-1))
       BU(I,J)=BU(I,J)*TMP(I,J)
       BV(I,J)=BV(I,J)*TMP(I,J)
       FX(I,J)=FX(I,J)*TMP(I,J)
       FY(I,J)=FY(I,J)*TMP(I,J)
C-----------------------------------------------------------------------
C  ADD IN THE LOCAL RATE OF CHANGE TO THE SYMMETRIC TERMS
C-----------------------------------------------------------------------
       BU(I,J)=BU(I,J)+AMAS(I,J)/DT
       BV(I,J)=BV(I,J)+AMAS(I,J)/DT
C-----------------------------------------------------------------------
C  SEPARATE THE ICE/OCEAN STRESS TO THE THREE MAJOR TERMS(BU/V,ASY,FX/Y)
C-----------------------------------------------------------------------
       TMP(I,J)=(UWAT(I,J)-U(I,J,LRHS))**2+(VWAT(I,J)-V(I,J,LRHS))**2
C**AVOID DIVISION BY ZERO WHEN TAKING THE RECIPROCAL OF BU AND BV:
       TMP(I,J)=RHOWAT*CDWAT*SQRT(TMP(I,J))+.000001
       BU(I,J)=BU(I,J)+TMP(I,J)*COSWAT
       BV(I,J)=BV(I,J)+TMP(I,J)*COSWAT
       ASY(I,J)=SINWAT*TMP(I,J)
       FX(I,J)=FX(I,J)+TMP(I,J)*(COSWAT*UWAT(I,J)-SINWAT*VWAT(I,J))
       FY(I,J)=FY(I,J)+TMP(I,J)*(SINWAT*UWAT(I,J)+COSWAT*VWAT(I,J))
C-----------------------------------------------------------------------
C  ADD COORDINATE TRANSFORMATION TERM (TANGENT TERMS)
C-----------------------------------------------------------------------
       TMP(I,J)=0.25*((V(I,J,LRHS)  +V(I,J-1,LRHS)
     1                +V(I-1,J,LRHS)+V(I-1,J-1,LRHS))*DNDX(I,J)
     2               -(U(I,J,LRHS)  +U(I-1,J,LRHS)
     3                +U(I-1,J,LRHS)+U(I-1,J-1,LRHS))*DMDY(I,J))
C-----------------------------------------------------------------------
C  ADD COEFFICIENT FOR CORIOLIS FORCE
C-----------------------------------------------------------------------
       ASY(I,J)=ASY(I,J)+AMAS(I,J)*(F(I,J)+TMP(I,J))
C-----------------------------------------------------------------------
C  ADD CONTRIBUTION FROM THE SEA SURFACE TILT
C-----------------------------------------------------------------------
       FX(I,J)=FX(I,J)-VWAT(I,J)*AMAS(I,J)*F(I,J)
       FY(I,J)=FY(I,J)+UWAT(I,J)*AMAS(I,J)*F(I,J)
C-----------------------------------------------------------------------
C  ADD THE AIR/ICE STRESS
C-----------------------------------------------------------------------
       IF (SURTYP .EQ. 0.) THEN
         CDWIN = CDWIN
        ELSE
         CDWIN=.25*(CD(I,J)+CD(I-1,J)+CD(I,J-1)+CD(I-1,J-1))
       ENDIF
       SINWIN=.25*(SINBET(I,J)+SINBET(I-1,J)+SINBET(I,J-1)
     1            +SINBET(I-1,J-1))*(1.-SURFWIN)     +SINWIN*SURFWIN
       COSWIN=.25*(COSBET(I,J)+COSBET(I-1,J)+COSBET(I,J-1)
     1            +COSBET(I-1,J-1))*(1.-SURFWIN)     +COSWIN*SURFWIN
       TMP(I,J)=RHOAIR*CDWIN*SQRT(UWIN(I,J)**2+VWIN(I,J)**2)
       TAUX(I,J)=TMP(I,J)*(COSWIN*UWIN(I,J)-SINWIN*VWIN(I,J))
       TAUY(I,J)=TMP(I,J)*(SINWIN*UWIN(I,J)+COSWIN*VWIN(I,J))
       FX(I,J)=FX(I,J)+TAUX(I,J)
       FY(I,J)=FY(I,J)+TAUY(I,J)
C-----------------------------------------------------------------------
C  ADD IN CONTRIBUTION FROM LOCAL RATE OF CHANGE FROM OLD TIME STEP
C-----------------------------------------------------------------------
       FX(I,J)=FX(I,J)+AMAS(I,J)*U(I,J,LOLD)/DT
       FY(I,J)=FY(I,J)+AMAS(I,J)*V(I,J,LOLD)/DT
C-----------------------------------------------------------------------
C  GET THE RECIPROCALS OF BU AND BV
C-----------------------------------------------------------------------
       IF (BU(I,J) .NE. 0.) BU(I,J)=1./BU(I,J)
       IF (BV(I,J) .NE. 0.) BV(I,J)=1./BV(I,J)
  600 CONTINUE
CD      PRINT *,'Leaving relcon'
      RETURN
      END
      SUBROUTINE STRAIN(LRHS, E11, E12, E22, U, V)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF THE STRAIN RATE TENSOR
C  METHOD:
C     -DETERMINATION OF STRAIN RATES AT GRID CENTER POINTS BY FIRST
C       INTERPOLATING TO 1/2 WAY BETWEEN THE GRID EDGE POINTS, AND
C       THEN DIFFERENTIATING
C     -USE OF EQ.(A5) IN LEPPAERANTA AND HIBLER (85)
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LRHS
CD      COMMON/VEL/U(L,M,3),V(L,M,3)
      REAL U(L, M, 3), V(L, M, 3)
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
      REAL E11(L,M), E12(L,M), E22(L,M)
C=======================================================================
C     -E11:  X-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E22:  Y-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E12:  DEVIATORIC (SHEAR) STRAIN RATE
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  CALCULATE D(U)/DX
C-----------------------------------------------------------------------
      DO 10 J=1,MM
      DO 10 I=1,LM
       E11(I,J)=0.5*PM(I,J)*((U(I+1,J,LRHS)+U(I+1,J+1,LRHS))
     1                      -(U(I,J,LRHS)  +U(I,J+1,LRHS)))/DX
C-----------------------------------------------------------------------
C  CALCULATE D(V)/DY
C-----------------------------------------------------------------------
       E22(I,J)=0.5*PN(I,J)*((V(I,J+1,LRHS)+V(I+1,J+1,LRHS))
     1                      -(V(I,J,LRHS)  +V(I+1,J,LRHS)))/DY
C-----------------------------------------------------------------------
C  CALCULATE 0.5[D(V)/DX+D(U)/DY]
C-----------------------------------------------------------------------
       E12(I,J)=0.25*PM(I,J)*((V(I+1,J,LRHS)+V(I+1,J+1,LRHS))
     1                       -(V(I,J,LRHS)  +V(I,J+1,LRHS)))/DX
     2         +0.25*PN(I,J)*((U(I,J+1,LRHS)+U(I+1,J+1,LRHS))
     3                       -(U(I,J,LRHS)  +U(I+1,J,LRHS)))/DY
   10 CONTINUE
      RETURN
      END
      SUBROUTINE BCSINIT(VM, HM, OM, FLM)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                        AUG  87
C     Robert Grumbine       NMC, Camp Springs                   June 94
C  PURPOSE:
C     -READS DOMAIN MASKS
C     -IDENTIFIES OUTFLOW CELLS
C     -Modified to work with reading in variable grid sizes. 
C         BG 23 June 1994.
C  LAST MODIFIED: 23 June 1994
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/OUTFLOW/NOUT,IOUT(LDO), JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
      INTEGER K1(L,M), K2(0:L,0:M)
C=======================================================================
      INTEGER I, J
      CHARACTER*9 form, formp
C-----------------------------------------------------------------------
C  INITIALIZE THE MASKS
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
          WRITE (form, 9001) L
          WRITE (formp,9001) LP
        READ(10,form) ((VM(I,J), I=1,L), J=1,M)
        READ(10,formp) ((HM(I,J), I=0,L), J=0,M)
        READ(10,formp) ((OM(I,J), I=0,L), J=0,M)
CD        READ(10,801) ((VM(I,J), I=1,L), J=1,M)
CD        READ(10,802) ((HM(I,J), I=0,L), J=0,M)
CD        READ(10,802) ((OM(I,J), I=0,L), J=0,M)
       ELSE IF (PTYPE .EQ. 3) THEN
          WRITE (form, 9001) L
          WRITE (formp,9001) LP
          READ(10,form ) ((VM(I,J), I=1,L), J=1,M)
          READ(10,formp) ((HM(I,J), I=0,L), J=0,M)
          READ(10,formp) ((OM(I,J), I=0,L), J=0,M)
CD          WRITE(*,form) ((INT(VM(I,J)), I=1,L), J=1,M)
CD          WRITE(*,formp) ((INT(HM(I,J)), I=0,L), J=0,M)
        DO 1000 J = 0, M
        DO 1000 I = 0, L
            K2(I,J) = INT(HM(I,J))
 1000   CONTINUE
        DO 1100 J = 1, M
        DO 1100 I = 1, L
            K1(I,J) = INT(VM(I,J))
 1100   CONTINUE

       ELSE
        STOP 'PTYPE OUT OF RANGE IN BCSINIT'
      ENDIF
C-----------------------------------------------------------------------
C  DETEMINE NUMBER OF OUTFLOW CELLS
C-----------------------------------------------------------------------
      NOUT=0
      DO 10 J=0,M
      DO 10 I=0,L
       IF ((OM(I,J).EQ. 0.0).AND.(HM(I,J).NE.0.0)) THEN
        NOUT=NOUT+1
        IOUT(NOUT)=I
        JOUT(NOUT)=J
        IF (NOUT.GT.LDO) THEN
         PRINT 811,LDO
         STOP
        END IF
       END IF
   10 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE MASKS FOR FLUX BC'S
C-----------------------------------------------------------------------
      DO 20 J=1,M
      DO 20 I=1,L
       FLM(I,J,1)=0.5*(OM(I,J)+OM(I-1,J))*(1.-ABS(OM(I,J)-OM(I-1,J)))
       FLM(I,J,2)=0.5*(OM(I,J)+OM(I,J-1))*(1.-ABS(OM(I,J)-OM(I,J-1)))
   20 CONTINUE
      RETURN

  801 FORMAT (127G1.0)
  802 FORMAT (128G1.0)

 9001 FORMAT ('(',I3,'G1.0)')

  811 FORMAT ('NUMBER OF OUTFLOW GRID POINTS EXCEEDS THE DIMENSION ',I5)
      END
      SUBROUTINE DRUCKF(FELD, FLG, FAK, ADD, HEADER, ICOUNT, NX, NY)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     ?                    MPI, HAMBURG
C     Robert Grumbine      NCEP, Camp Springs, MD                1993
C  LAST MODIFIED: 14 January 1993
C  PURPOSE:
C     -PRINTING OF AN ARRAY BY IGNORING "LAND" POINTS
C  METHOD:
C     -ARRAY TAKEN AS AN INTERNAL FILE
C  INTERFACE:
C     -FELD:   FIELD TO BE PRINTED
C     -FLG:    FLAG FIELD OF MASK
C     -FAK:    MULTIPLICATION FACTOR FOR VARIABLE TO BE SHOWN
C     -ADD:    SUMMATION FACTOR FOR VARIABLE TO BE SHOWN
C     -HEADER: HEADER FOR OUTPUT
C     -ICOUNT: RUNNING TIME STEP
C     -NX:     NUMBER OF GRID POINTS IN X-DIRECTION
C     -NY:     NUMBER OF GRID POINTS IN Y-DIRECTION
C=======================================================================
      REAL FELD(NX,NY), FLG(NX,NY), FAK, ADD
      INTEGER ICOUNT, NX, NY
      CHARACTER*3 TEXT(44)
      CHARACTER HEADER*(*)
      INTEGER NXSTEP, NYSTEP, I, J, JJ, INDEX
      REAL XP
C=======================================================================

      NXSTEP=1+(3*(NX-2)-1)/132
      NYSTEP=1+NY/70

      WRITE(16,101)
      WRITE(16,100) ICOUNT,HEADER
      DO 1 J=1,NY-1,NYSTEP
       JJ=NY-J
       INDEX=0
       DO 2 I=3,NX-1,NXSTEP
        INDEX=INDEX+1
        XP=FELD(I,JJ)*FAK+ADD
        IF (XP .LT. 1000. .AND. XP .GT. -99.) THEN 
          WRITE (TEXT(INDEX),200) INT(XP)
        ELSE
CD          WRITE (TEXT(INDEX),205) 
          WRITE (TEXT(INDEX),200) 999
        ENDIF
        IF (FLG(I,JJ).EQ.0.0) THEN
         TEXT(INDEX)='   '
        ELSE IF (XP.LT.0..AND.XP.GT.-1.) THEN
         TEXT(INDEX)=' -0'
        END IF
    2  CONTINUE
       WRITE(16,300) (TEXT(I),I=1,INDEX)
    1 CONTINUE
      RETURN
  100 FORMAT (1H ,5X,' DAY ',I5,'  ARRAY : ',A50)
  101 FORMAT (1H )
  200 FORMAT (I3)
  205 FORMAT ('---')
  300 FORMAT (1X,44A3)
      END
      SUBROUTINE FORFLD(LWDN, SWDN, INTYP, IIC, UWIN, VWIN)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C     Robert Grumbine       NMC, Camp Springs                     Nov 92
C     Robert Grumbine       NMC, Camp Springs                     Jul 93
C     Robert Grumbine       NCEP, Camp Springs                    Feb 97
C  PURPOSE:
C     -READS TEMPORALLY VARYING BOUNDARY CONDITIONS (FORCING FIELDS)
C     -Rewritten to obtain forcing fields from NMC-MRF output
C     -Jul 93 variant to split off interpolations to a separate 
C       program.  This sbr. now just reads in the fields.
C  EXTERNALS:
C     -None
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
CD      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
CD      REAL T
CD      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      INTEGER IIC
CD      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
CD      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN
      REAL UWIN(L,M), VWIN(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1  ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL TAIR, TD, ACL, PA, UG, TA, RPREC
C     LWUP is not used by MPI model.  It is included here for the
C       Navy model, and written out.
      REAL LWUP(0:L,0:M), LWDN(0:L,0:M), SWDN(0:L,0:M)
      REAL ts(0:L, 0:M), mask(0:L, 0:M)
C=======================================================================
      INTEGER I, J, INTYP
C     INTYP is now a dummy
      LOGICAL overload
C-----------------------------------------------------------------------
C    Call routine to get information from the MRF
C-----------------------------------------------------------------------
        IF (MOD(IIC,14) .EQ. 1) REWIND(20)
        READ (20) TAIR
        READ (20) PA
        READ (20) TD
        READ (20) ts
        READ (20) SWDN
        READ (20) LWDN
        READ (20) LWUP
        READ (20) RPREC
        READ (20) mask
        READ (20) UWIN
        READ (20) VWIN
        IF (overload(LWDN, LP, MP, 10000.)) THEN
          PRINT *,'Overloaded LWDN in FORFLD, 10,000 cutoff'
        ENDIF
C-----------------------------------------------------------------------
C  CALCULATION OF WIND SPEED
C-----------------------------------------------------------------------
C     Indices changed by BG to avoid referencing non-extant
C       elements of uwin, vwin.
      DO 6 J = 0, MM
        DO 6 I = 0, LM
          RPREC(I,J)=0.0 !temporary measure until units are checked
       UG(I,J)=.25*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     1             +SQRT(UWIN(I+1,J  )**2+VWIN(I+1,J  )**2)
     2             +SQRT(UWIN(I  ,J+1)**2+VWIN(I  ,J+1)**2)
     3             +SQRT(UWIN(I+1,J+1)**2+VWIN(I+1,J+1)**2))
    6 CONTINUE

      I = L
      DO 10 J = 0, MM
       UG(I,J)=.5*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     2            +SQRT(UWIN(I  ,J+1)**2+VWIN(I  ,J+1)**2) )
  10  CONTINUE

      J = M
      DO 20 I = 0, LM
       UG(I,J)=.5*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     1            +SQRT(UWIN(I+1,J  )**2+VWIN(I+1,J  )**2) )
   20 CONTINUE

      I = L
      J = M
      UG(I,J) = SQRT(UWIN(I,J)**2+VWIN(I,J)**2 )

      RETURN
      END
CD      SUBROUTINE INIT(INTYP, bathy, hmlref, 
      SUBROUTINE INIT(INTYP, bathy, 
     1     DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2     SNOFLG,
     3     TICE, TICM,
     4     U, V, SURTYP, SURFWIN, 
     5     QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO,
     6     HS, HT, QV, QRHO, QW, FW, IEN, MLFIX, 
     7     H, A, HSN)
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C     Robert Grumbine       NMC, Camp Springs, MD                 Dec 92
C     Robert Grumbine       NMC, Camp Springs, MD                 Jul 95
C  PURPOSE:
C     -DEFINES BASIC PARAMETERS FOR THE MODELS
C     -SETS INITIAL FIELDS
C  OPTIONS:
C     -REDUCTION OR INCREASE OF CLOUDINESS
C     -INCLUSION OF SNOW (SNOFLG)
C     -INCLUSION OF OML-MODEL (MLFIX)
C     -INCLUSION OF AN ABL-MODEL (ABLFIX)
C     -CHOICE OF ABL-MODEL (ECMTYP)
C     -ECMTYP and ABLFIX replaced by SURTYP 1/93 jointly.
C     -USE OF SURFACE OR UPPER LAYER WINDS (SURFWIN); THIS OPTION
C       DECIDES ABOUT USING A FIXED OR COMPUTED WIND TURNING AND, IF
C       THE ABL-MODEL IS TURNED ON (ABLFIX=1, ECMTYP=0), SIMULTANEOUSLY
C       ABOUT USING THE FRICTION VELOCITY CALCULATED VIA THE RESISTANCE
C       LAWS OF THE EKMAN-LAYER (SURFWIN=0) OR THE ONE CALCULATED VIA
C       THE MONIN-OBUKHOV THEORY (SURFWIN=1)
C     -CARTESIAN OR SPHERICAL COORDINATES (DX AND PM)
C     -Polar Stereographic coordinates (PTYPE=3) BG.
C  EXTERNALS:
C     -BCSINIT: READS IN MASKS AND REGISTERS OUTFLOW CELLS
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
C     Include physical constants, rheology parameters, and mixed layer
C       parameters
      INCLUDE "physical.inc"
      INCLUDE "rheology.inc"
      INCLUDE "oml.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      REAL FM, F, COSPHI, SINPHI

      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN

      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY

      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA

      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY

CD      COMMON/VEL/U(L,M,3), V(L,M,3)
      REAL U(L, M, 3), V(L, M, 3)
CD      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)

      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN

      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      REAL SINWAT, COSWAT, UWAT, VWAT

      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL TAIR, TD, ACL, PA, UG, TA, RPREC

CD      COMMON/TEMP/TICE(0:L,0:M)
      REAL TICE(0:L, 0:M)
CD      COMMON/TEMPM/TICM(0:L,0:M,NLEVEL)
      REAL TICM(0:L, 0:M, NLEVEL)
C     Commons viscp and relaxp moved to rheology.inc
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM

CD      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
CD     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
CD     2 QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M), 
CD     3 QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M), QHSTO(0:L,0:M)
      REAL HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

CD      COMMON/GEO/PI, RAD
      REAL PI, RAD
CD      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      REAL SURTYP, SURFWIN
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1 BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      REAL CD, SINBET, COSBET, BETA, TAUX, TAUY

CD      COMMON/PMLPARM/DCVM, WUP, COSGAM, RTC, STC, QTOC
      REAL DCVM, WUP, COSGAM, RTC, STC, QTOC
CD      COMMON/SNOFLG/SNOFLG
      REAL SNOFLG
C     Make a bathymetric array, pass as argument
      REAL bathy(0:L, 0:M)
      INTEGER INTYP
C=======================================================================
      REAL TI0, QH0, QS0, QT0, QHB0, QDT0, QDS0
      REAL THWIN, THWAT
      REAL tempor(0:L, 0:M)
      CHARACTER*120 runparms
      REAL PHI, XL, YL, FLAGM, QHSMAX
      INTEGER I, J, K
C-----------------------------------------------------------------------
C  SPECIFY ZONAL AND ANNUAL MEAN OF CLOUDINESS (ACC.TO V.LOON (1972))
C  Replaced with calls to read in from MRF output.  BG 11/92
C-----------------------------------------------------------------------
C     Open input run files
      OPEN ( 9, FILE='RESTARTo', FORM='UNFORMATTED', STATUS='OLD')
      OPEN (10, FILE='MASK', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='tsdeep', FORM='UNFORMATTED', STATUS='OLD')
      OPEN (13, FILE='tsshal', FORM='UNFORMATTED', STATUS='OLD')
      READ (*,9009) runparms
      OPEN (21, FILE=runparms, FORM='FORMATTED', STATUS='OLD')
      OPEN (27, FILE='bathy', FORM='UNFORMATTED', STATUS='OLD')
 9009 FORMAT (A120)
  
C     Open output run files
      OPEN (14, FILE='RESTARTn', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (15, FILE='thick', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (16, FILE='FORT.16', FORM='FORMATTED', STATUS='NEW')
      OPEN (17, FILE='conc', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (18, FILE='FORT.18', FORM='FORMATTED', STATUS='NEW')
      OPEN (19, FILE='vels', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (22, FILE='tml', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (23, FILE='sml', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (24, FILE='hml', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (25, FILE='atm.flux', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (26, FILE='oce.flux', FORM='UNFORMATTED', STATUS='NEW')
C-----------------------------------------------------------------------
C  SET RUN LENGTH and OUTPUT parameters
C-----------------------------------------------------------------------
      READ (21, 9001) INTYP
      IF (INTYP .EQ. 1) THEN
        OPEN (20, FILE='metout', FORM='UNFORMATTED', STATUS='NEW')
       ELSE
        OPEN (20, FILE='metout', FORM='UNFORMATTED', STATUS='OLD')
      ENDIF
      READ (21, 9001) NTMES
      READ (21, 9001) NRST
      READ (21, 9001) NSTAT
      READ (21, 9001) NSTA
      READ (21, 9001) NPLT
      READ (21, 9001) NFLD
      READ (21, 9001) NRREC
C-----------------------------------------------------------------------
C  DECIDE ABOUT INCLUSION OF SNOW
C-----------------------------------------------------------------------
C**REMARK: -SNOFLG=0.: NO SNOW
C          -SNOFLG=1.: INCLUDE SNOW LAYER
      READ (21, 9002) SNOFLG
C-----------------------------------------------------------------------
C  DECIDE ABOUT INCLUDING THE VARIABLE OML MODEL
C-----------------------------------------------------------------------
C**REMARK: -MLFIX=1 : FIXED MIXED LAYER
C          -MLFIX=0 : PROGNOSTIC MIXED LAYER
      READ (21, 9001) MLFIX
C-----------------------------------------------------------------------
C  DECIDE WHICH AIR-ICE INTERACTION TO USE
C-----------------------------------------------------------------------
C          -SURTYP = 0. : BULK AERODYNAMICS
C          -SURTYP = 1. : ASL MODEL (LOUIS, 1979)
C          -SURTYP = 2. : ABL MODEL (KOCH, 1988)
      READ (21, 9002) SURTYP
C-----------------------------------------------------------------------
C  USE OF SURFACE OR UPPER LAYER WINDS
C-----------------------------------------------------------------------
C**REMARK: -SURFWIN=1.: SURFACE WINDS (FIXED WIND TURNING)
C          -SURFWIN=0.: UPPER LAYER WINDS
      READ (21, 9002) SURFWIN
C-----------------------------------------------------------------------
C  SET PARAMETERS FOR SPHERICAL COORDINATES AND CORIOLIS FORCE
C-----------------------------------------------------------------------
      PI=4.*ATAN(1.)
      RAD=PI/180.
C  SET CORIOLIS PARAMETER:
      IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
        DO 1 J=0,M
          DO 2 I = 0, L
            COSPHI(I, J)=COS((LATMIN+DLAT*J)*RAD)
            SINPHI(I, J)=SIN((LATMIN+DLAT*J)*RAD)
            FM(I, J)=F0*SINPHI(I, J)
    2     CONTINUE
    1   CONTINUE
        DO 3 J=1,M
          DO 4 I = 1, L
            F(I,J)=F0*SIN((LATMIN-DLAT/2.+DLAT*J)*RAD)
    4     CONTINUE
    3   CONTINUE
       ELSE IF (PTYPE .EQ. 3) THEN
C       Polar stereographic grid
        DO 5 J = 0, M
          DO 6 I = 0, L
            PHI = SQRT( (DX*(I-POLEI))**2 + (DY*(J-POLEJ))**2 )
     1        / DXDEG - 90.
            PHI = SIGN(PHI,LATMIN) 
            COSPHI(I,J) = COS(RAD*PHI)
            SINPHI(I,J) = SIN(RAD*PHI)
            FM(I,J)     = F0*SINPHI(I,J)
    6     CONTINUE
    5   CONTINUE
        DO 7 J = 1, M
          DO 8 I = 1, L
            PHI = SQRT( (DX*(I-0.5-POLEI))**2 + (DY*(J-0.5-POLEJ))**2 )
     1        / DXDEG - 90.
            PHI = SIGN(PHI,LATMIN) 
            F(I,J)=F0*SIN(PHI*RAD)
    8     CONTINUE
    7   CONTINUE

        ELSE
         STOP 'Grid type out of range'
       ENDIF       
C-----------------------------------------------------------------------
C  DETERMINE GRID SIZE AND TIME STEP
C-----------------------------------------------------------------------
C**REMARK: -DX=5.5560E+05 FOR SPHERICAL COORDINATES
C          -DX=2.3481E+05 FOR CARTESIAN COORDINATES
      XL=DX*FLOAT(LM)
      DXSQ=DX*DX
      YL=DY*FLOAT(MM)
      DYSQ=DY*DY
      SX2=0.5/DXSQ
      SY2=0.5/DYSQ
      SXY=0.25/(DX*DY)
      T=0.0
C-----------------------------------------------------------------------
C  SET COORDINATE MAPPING FACTORS
C-----------------------------------------------------------------------
C**REMARK: -CARTESIAN COORDINATES: PM=1 AND DX=DX AT MEAN LATITUDE
C          -SPHERICAL COORDINATES: PM=1./COSPHI(I,J) AND DX=DX AT LAT=0
C          -Polar Stereographic:   PM=2./(1+ABS(SINPHI)) AND DX=DX AT LAT=90
      IF (PTYPE .EQ. 1) THEN
C       Cartesian Coordinates
        DO 209 J = 0, M
          DO 210 I = 0, L
            PM(I,J) = 1.
            PN(I,J) = 1.
  210     CONTINUE
  209   CONTINUE
       ELSE IF (PTYPE .EQ. 2) THEN
C        Spherical coordinates
         DO 211 J = 0, M
           DO 212 I = 0, L
             PM(I,J) = 1./COSPHI(I,J)
             PN(I,J) = 1.
  212      CONTINUE
  211    CONTINUE
       ELSE IF (PTYPE .EQ. 3) THEN
C        Polar Stereographic
         DO 213 J = 0, M
           DO 214 I = 0, L
             PM(I,J)=2./(1.+ABS(SINPHI(I,J)))  !Polar Stereographic
             PN(I,J)=2./(1.+ABS(SINPHI(I,J)))  !Polar Stereographic
  214      CONTINUE
  213   CONTINUE
       ELSE 
          STOP 'Grid type out of range'
      ENDIF

C     The following terms are the x and y components of the centripetal
C       accelerations.  They are being ignored for now.  They derive
C       from the difference between u.grad(u) and grad(u^2/2)+curl(u)
C       cross u when non-cartesian coordinates are used.  RELCON
C       is the only routine which uses these.
      DO 216 J=1,M
      DO 216 I=1,L
       DNDX(I,J)=0.
       DMDY(I,J)=0.
  216 CONTINUE
C-----------------------------------------------------------------------
C  SET UP ICE AND SNOW PARAMETERS
C-----------------------------------------------------------------------
      READ (21, 9002) H0
      READ (21, 9002) ARMIN
      READ (21, 9002) ARMAX
      READ (21, 9002) HMIN
      READ (21, 9002) HNU
      HNU = HNU * DX
      HNU2=DX**2*HNU
C-----------------------------------------------------------------------
C  SET UP SURFACE HEAT AND MOMENTUM TRANSFER PARAMETERS
C-----------------------------------------------------------------------
C**FOR SURFACE WINDS TURNING ANGLE EQUAL TO ZERO:
C     SINWIN=-0.4226E+00
C     COSWIN=0.9063E+00
      READ (21, 9002) CDWIN
      READ (21, 9002) THWIN
      SINWIN = SIN(THWIN*PI/180.)
      COSWIN = COS(THWIN*PI/180.)
      READ (21, 9002) THWAT
      SINWAT = SIN(THWAT*PI/180.)
      COSWAT = COS(THWAT*PI/180.)
CD      FAKTH=RHOAIR*.4*CPAIR
C-----------------------------------------------------------------------
C  SET OML PARAMETERS
C-----------------------------------------------------------------------
      FLAGM=FLOAT(MLFIX)
      READ (21, 9002) QHSMAX
      DCVM=EXP(-QHSMAX/QHS)
      READ (21, 9002) WUP
      IF (MLFIX.EQ.1) WUP=0.0
      READ (21, 9002) COSGAM
      READ (21, 9002) RTC
      RTC = RTC * 8.64E4/DT
      READ (21, 9002) STC
      STC = STC * 8.64E4/DT
      READ (21, 9002) QTOC
      READ (27) bathy
CD      PRINT *,'Read in bathymetry'
C-----------------------------------------------------------------------
C  SET UP PARAMETERS FOR THE RELAX SUBROUTINE, moved to physical.inc
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  INITIALIZE HORIZONTAL BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      CALL BCSINIT(VM, HM, OM, FLM)
CD      PRINT *,'OM = ',OM
C-----------------------------------------------------------------------
C  Rationalize the boundary condition file versus the bathymetry file.
C  If either says there is water present, put water at the point.
C  Work over 1-(N-1).  Use bathy, HM, OM (HM and OM are identical).
C  Bob Grumbine 10 October 1995.
      DO 200 J = 1, M-1
        DO 101 I = 1, L-1
          IF (OM(i,j) .EQ. 1. .AND. bathy(i,j) .LE. 5.0) THEN
CD            PRINT *,'Mask is water, bathy is land ',i,j
            bathy(i,j) = 1.0
            OM(i,j) = 0.0
            HM(i,j) = 0.0 
          ENDIF
          IF (OM(i,j) .EQ. 0. .AND. bathy(i,j) .GT. 5.0) THEN
CD            PRINT *,'Mask is land, bathy is water ',i,j,bathy(i,j)
            OM(i,j) = 1.
            HM(i,j) = 1.
          ENDIF
  101   CONTINUE
  200 CONTINUE
CD      PRINT *,'OM = ',OM, A, H
C-----------------------------------------------------------------------
C  INITIALIZE THE SI, OML AND ABL VARIABLES (OPT. FROM RESTART TAPE)
C-----------------------------------------------------------------------
      IF (NRREC .NE. 0) THEN
       DO 420 K=1,NRREC
         PRINT *,'Reading record ',K
         READ (9) tempor
         DO 9800 j = 0, M
           DO 9801 i = 0, L
             H(i,j, 1) = tempor(i,j)
             H(i,j, 2) = tempor(i,j)
 9801      CONTINUE
 9800    CONTINUE
         
CD         READ (9) A
         READ (9) tempor
         DO 9802 j = 0, M
           DO 9803 i = 0, L
             A(i,j, 1) = tempor(i,j)
             A(i,j, 2) = tempor(i,j)
 9803      CONTINUE
 9802    CONTINUE
        
CD         READ (9) HSN
         READ (9) tempor
         DO 9804 j = 0, M
           DO 9805 i = 0, L
             HSN(i,j, 1) = tempor(i,j)
             HSN(i,j, 2) = tempor(i,j)
 9805      CONTINUE
 9804    CONTINUE

         READ (9) TICE
         READ (9) QT
         READ (9) QS
         READ (9) QH
         READ (9) QTB
         READ (9) QSB
         READ (9) QHB
         READ (9) QDT
         READ (9) QDS
         READ (9) TICM
         READ (9) U
         READ (9) V
 420   CONTINUE
      ELSE
        READ (21, 9002) TI0
        READ (21, 9002) QH0
        READ (21, 9002) QS0
        READ (21, 9002) QT0
        READ (21, 9002) QHB0
        READ (21, 9002) QDT0
        READ (21, 9002) QDS0
        DO 280 J=1,M
        DO 280 I=1,L
         U(I,J,1)=0.
         V(I,J,1)=0.
  280 CONTINUE
        DO 281 J=0,M
        DO 281 I=0,L
         H(I,J,1)= 0.
         A(I,J,1)= 0.
         HSN(I,J,1)=0.
         TICE(I,J)= TI0
CD         QH(I,J)= QH0
         QH(I,J)= QH0
         QT(I,J)= QT0
         QS(I,J)= QS0
CD         QHB(I,J)= QHB0
         QHB(I,J)= bathy(i,j)
         QDT(I,J)= MIN(QDT0, bathy(i,j)) !Note that QD are thermocline thick-
         QDS(I,J)= MIN(QDS0, bathy(i,j)) ! nesses.  Must be less than bathy.
C      BG ADDITION TO AVOID INITIALIZING NON-EXISTENT ARRAY
C        ELEMENTS.  TAUX, TAUY ARE DIMENSIONED 1-L, 1-M.
         IF (I*J .NE. 0) TAUX(I,J)=0.
         IF (I*J .NE. 0) TAUY(I,J)=0.
  281   CONTINUE
        READ (12) QTB
        READ (12) QSB
        READ (13) QT
        READ (13) QS
      
        IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
C         Old initialization deleted via CD
CD          DO 283 J=0, M/2
          DO 283 J=0, M
          DO 283 I=0,L
CD           H(I,J,1)=2.*(1.-J/FLOAT(M/2))*OM(I,J)
CD           A(I,J,1)=1.*(1.-J/FLOAT(M/2))*OM(I,J)
CD           HSN(I,J,1)=.5*(1.-J/FLOAT(M/2))*OM(I,J)
            H(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            A(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            HSN(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
  283     CONTINUE
         ELSE
          DO 285 J = 0, M
          DO 285 I = 0, L
            H(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            A(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
            HSN(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                         (1. - (J/FLOAT(M/2)-1.)**2)*0.0
  285     CONTINUE
        ENDIF

        DO 282 K=1,NLEVEL
        DO 282 J=0,M
        DO 282 I=0,L
         TICM(I,J,K)=TICE(I,J)
  282   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C  READ IN FIELDS THAT ARE CONSTANT IN TIME
C-----------------------------------------------------------------------
CD        DO 1001 J = 0, M
CD        DO 1002 I = 0, L
CD           ZOW(I,J)=ZOWI
CD 1002   CONTINUE
CD 1001   CONTINUE

        DO 1000 J = 1, M-2
          DO 1100 I = 2, LM
            UWAT(I,J) = 0.0
            VWAT(I,J) = 0.0
 1100     CONTINUE
 1000   CONTINUE

      DO 60 J=1,M
       UWAT(1,J)=UWAT(LM,J)
       VWAT(1,J)=VWAT(LM,J)
       UWAT(L,J)=UWAT(2,J)
       VWAT(L,J)=VWAT(2,J)
   60 CONTINUE
      DO 61 I=1,L
       UWAT(I,M)=UWAT(I,MM)
       VWAT(I,M)=VWAT(I,MM)
   61 CONTINUE
C-----------------------------------------------------------------------
C  DERIVE INITIAL OML VARIABLES
C-----------------------------------------------------------------------
      DO 80 J=0,M
      DO 80 I=0,L
       QDS(I,J)=QDS(I,J)*(1.-FLAGM)
       QDT(I,J)=QDT(I,J)*(1.-FLAGM)
C      Degree content of water column
       HT(I,J)=(QT(I,J)-QTB(I,J))*(QH(I,J)+QDT(I,J))+QTB(I,J)*QHB(I,J)
C      Salt content of water column
       HS(I,J)=(QS(I,J)-QSB(I,J))*(QH(I,J)+QDS(I,J))+QSB(I,J)*QHB(I,J)
   80 CONTINUE
C-----------------------------------------------------------------------
C  WRITE CURRENT CONTROL PARAMETERS
C-----------------------------------------------------------------------
      WRITE(16,403) L, M, MLFIX, SURTYP, SURTYP, SURFWIN, DX, DY, 
     1  DT, H0, PSTAR, WT,
     3  ZOI, SINWIN, COSWIN, QTOC, CDWIN, CDWAT

      RETURN


  100 WRITE(*,400)
  403 FORMAT (
     1 ' L      = ',I12,    ' M      = ',I12,    ' MLFIX  = ',I12,/,
     2 ' SURTYP = ',1PE12.3,' SURTYP = ',1PE12.3,' SURFWIN= ',1PE12.3,/,
     3 ' DX     = ',1PE12.3,' DY     = ',1PE12.3,' DT     = ',1PE12.3,/,
     4 ' H0     = ',1PE12.3,' PSTAR  = ',1PE12.3,' WT     = ',1PE12.3,/,
     5 ' ZOI    = ',1PE12.3,' SINWIN = ',1PE12.3,' COSWIN = ',1PE12.3,/,
     6 ' QTOC   = ',1PE12.3,' CDWIN  = ',1PE12.3,' CDWAT  = ',1PE12.3)
  802 FORMAT (18F6.2)
  901 FORMAT (5E13.4)
  400 FORMAT ('1  READ ERROR IN INIT ')
 9001 FORMAT (I4)
 9002 FORMAT (E13.6)

      STOP
      END
C=======================================================================
      SUBROUTINE output (
     1    FLAGI1, FLAGI2, FLAGI, 
     2    OM, TAUX, TAUY, TA, TICM, QH ,
     3    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     4    CLO, T, 
     5    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS )
C=======================================================================
      IMPLICIT none
C=======================================================================
C  Programmed by:
C     Robert Grumbine       NMC, Camp Springs                     Oct.92
C     Robert Grumbine       NCEP, Camp Springs, MD                Sep 96
C  Purpose:
C     -Localize all model output to a single routine.  Clarify 
C       program logic (previously output was from main program).
C     -Output averaging removed 9/14/96 -- Print out every step.
C     -  Prepare for grib output.  RG.
C  Interface:
C     -INPUT,OUTPUT: standard control output
C     -TAPE15: ice thickness (unformatted)
C     -TAPE16: results printed in domain's shape
C     -TAPE17: ice concentration (unformatted)
C     -TAPE18: accumulated results for summation plots
C     -TAPE19: ice velocity (unformatted)
C     -TAPE20: ice compactness results for selected dates (plots)
C     -TAPE21: Run Parameters
C     -TAPE22: Mixed layer temperature
C     -TAPE23: Mixed layer salinity
C     -TAPE24: Mixed layer depth
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
C  Parameter:
C     -L: number of grid points in X-direction (even number only!)
C     -M: number of grid points in Y-direction (even number only!)
C=======================================================================

C     Arguments
      REAL OM(0:L,0:M), TAUX(L,M), TAUY(L,M), TA(0:L,0:M)
      REAL TICM(0:L,0:M,NLEVEL), QH(0:L,0:M)
      REAL QTM(0:L,0:M), ATMFLX(0:L,0:M)
      REAL A(0:L,0:M,2), H(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL FW(0:L,0:M), U(L,M,3), V(L,M,3)
      REAL PN(0:L,0:M), PM(0:L,0:M)
      REAL QS(0:L,0:M), QT(0:L,0:M), VM(L,M)
      REAL QTB(0:L,0:M), QSB(0:L,0:M), QHB(0:L,0:M)
      REAL QDT(0:L,0:M), QDS(0:L,0:M), TICE(0:L,0:M)
      REAL CLO, T
      INTEGER IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES

C=======================================================================
C     Local variables
      REAL FLAGI1(L,M), FLAGI2(L,M), FLAGI(0:L,0:M)  
      REAL FLAG, FLAG1, HEX, HEX1, HEX2, HEXI
      REAL HOSNSUM, HOTSUM, HSNSUM, HSUM, HU, HV, HWEX, HWEX2
      REAL HOSNTSUM, HOSUM
      REAL Z1, Z2, SHSUM, QTMSUM
      REAL CONC(0:L, 0:M), THICK(0:L, 0:M)
      REAL tempor(0:l, 0:M)
C=======================================================================
C     -FLAGI1:  GRID EDGE MASK FOR PLOTS SHOWING ICE COVERED AREAS ONLY
C     -FLAGI2:  SAME AS FLAGI1, BUT FOR MONTHLY AVERAGED VALUES
C     -FLAGI:   SAME AS FLAGI1, BUT FOR GRID CENTER POINTS
C=======================================================================
      INTEGER I, J
      INTEGER NRREC, NRST
C     ARMAG is the magic number used to determine compact ice conditions
      REAL ARMAG
      PARAMETER (ARMAG = 0.85)
      REAL uout(L, M), vout(L, M)

CD      SAVE
C-----------------------------------------------------------------------
C  STATISTIC FOR OUTFLOW
C-----------------------------------------------------------------------
C  CHANGE OF OUTFLOW IN TERMS OF ICE VOLUME:
       HOSUM=HOSUM*Z1*1.E-5
       HOTSUM=HOTSUM+HOSUM*1.E-1
C  CHANGE OF OUTFLOW IN TERMS OF SNOW VOLUME:
CD       HOSNTSUM=HOSNTSUM+HOSNSUM
C-----------------------------------------------------------------------
C  SUMMATION OF VARIABLES FOR CONTINUOUS STATISTICS
C-----------------------------------------------------------------------
C**NEXT COMPUTATIONS START AT SPECIFIED STATISTICS INTERVAL ONLY:
       IF (MOD(IIC,NSTAT).EQ.0) THEN
C-----------------------------------------------------------------------
C  AVERAGE OCEANIC AND ATMOSPHERIC HEAT FLUXES
C-----------------------------------------------------------------------
        Z1=FLOAT(NSTAT)
C-----------------------------------------------------------------------
C  SET RUNNING SUM VALUES TO 0
C-----------------------------------------------------------------------
        HEX=.0
C  NUMBER OF ICE COVERED GRID CELLS:
        HEXI=.0
C  ICE EXTENT OF HIGH ICE COMPACTNESS:
        HEX1=.0
        HWEX=.0
        HSUM=.0
        HSNSUM=.0
C  AVERAGE OCEANIC HEAT FLUX:
        QTMSUM=.0
C  AVERAGE ATMOSPHERIC HEAT FLUX:
        SHSUM=.0
C  TIME AVERAGED ICE EXTENT OF HIGH ICE COMPACTNESS:
        HEX2=.0
C  TIME AVERAGED ICE AREA OF HIGH ICE COMPACTNESS:
        HWEX2=.0
C  HORIZONTAL GRID CELL AREA IN KM**2 (UNCORR.IF SPHER.COORD.ARE USED):
        Z1=DX*DY*1.0E-6
C-----------------------------------------------------------------------
C  INTEGRATIONS OVER ENTIRE DOMAIN
C-----------------------------------------------------------------------
        DO 150 J=1,MM
        DO 150 I=2,LM
         FLAG=.5*(1.-SIGN(1., 1.-ARMAG-A(I,J,LNEW)))
         FLAG1=.5*(1.-SIGN(1., ARMAG-A(I,J,LNEW)))
         FLAGI(I,J)=.5*(1.-SIGN(1.,.0-H(I,J,LNEW)))
         HEX=HEX+OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))*FLAG
         HEXI=HEXI+FLAG*OM(I,J)
         HEX1=HEX1+OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))*FLAG1
         HWEX=HWEX+OM(I,J)*Z1*1.E-6*A(I,J,LNEW)/(PN(I,J)*PM(I,J))
         HSUM=HSUM+OM(I,J)*Z1*1.E-6*H(I,J,LNEW)/(PN(I,J)*PM(I,J))
         HSNSUM=HSNSUM+OM(I,J)*Z1*1.E-6*HSN(I,J,LNEW)/(PN(I,J)*PM(I,J))
         QTMSUM=QTMSUM+OM(I,J)*FLAGI(I,J)*QTM(I,J)
         SHSUM=SHSUM+OM(I,J)*FLAGI(I,J)*ATMFLX(I,J)
  150   CONTINUE
        Z2=AMAX1(HEXI,1.)
        QTMSUM=QTMSUM/Z2
        SHSUM=SHSUM/Z2*1.E-1
C-----------------------------------------------------------------------
C  WRITE OUT DATA FOR SUMMATION PLOTS (SEASONAL CYCLES)
C-----------------------------------------------------------------------
        WRITE(18,960) HSUM,HEX,HEX1,HWEX
C-----------------------------------------------------------------------
C  WRITE OUT STATISTICS
C-----------------------------------------------------------------------
C        Mixed layer
         WRITE (22) QT
         WRITE (23) QS
         WRITE (24) QH
C        Fluxes 
         WRITE (25) ATMFLX
         WRITE (26) QTM

         WRITE(*, 902) IIC, HSUM, HEX, HWEX, HSNSUM, HOSUM, HOTSUM, 
     1                   QTMSUM, SHSUM
         WRITE(16, 902) IIC, HSUM, HEX, HWEX, HSNSUM, HOSUM, HOTSUM, 
     1                   QTMSUM, SHSUM
         DO 1000 j = 0, M
           DO 1100 i = 0, L
             CONC(i,j) = A(i,j,LNEW)
             THICK(i,j) = H(i,j,LNEW)
 1100      CONTINUE
 1000   CONTINUE
        WRITE (17) CONC
        WRITE (15) THICK

        DO 1200 j = 1, M
          DO 1300 i = 1, L
            IF (CONC(i-1,j-1)+CONC(i-1,j)
     1         +CONC(i,j)+CONC(i,j-1) .NE. 0.) THEN
              uout(i,j) = U(i, j, LNEW)
              vout(i,j) = V(I, j, LNEW)
             ELSE
              uout(i,j) = 0.0
              vout(i,j) = 0.0
             ENDIF
 1300     CONTINUE
 1200   CONTINUE
        WRITE (19) uout
        WRITE (19) vout

CD        PRINT *,'Passed write 19 uout, vout'
C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
C-----------------------------------------------------------------------
C  PRINT OUT GEOGRAPHICAL PATTERNS
C-----------------------------------------------------------------------
         IF (MOD(IIC,NSTA).EQ.0) THEN
          CALL DRUCKF(H(0,0,LNEW),OM,10.,0.,'H*10 [M]',IIC,LP,MP)
          CALL DRUCKF(A(0,0,LNEW),OM,100.,0.,'A [%]',IIC,LP,MP)
          CALL DRUCKF(U(1,1,LNEW),VM,100.,0.,'U [CM/S]',IIC,L,M)
          CALL DRUCKF(V(1,1,LNEW),VM,100.,0.,'V [CM/S]',IIC,L,M)
          CALL DRUCKF(HSN(0,0,LNEW),OM,100.,0.,'HSNOW [CM]',IIC,LP,MP)
          CALL DRUCKF(QT,OM,10.,0.,'M L TEMP*10 [DEG C]',IIC,LP,MP)
          CALL DRUCKF(TICM(0,0,4),OM,1.,0.,'ICE TEMP [DEG C]',IIC,LP,MP)
          CALL DRUCKF(QS,OM,10.,-300.,'SALT M L *10 -300',IIC,LP,MP)
          CALL DRUCKF(QH,OM,1.,0.,'M L DEPTH [M]',IIC,LP,MP)
CD          PRINT *,'Wrote QH'
          CALL DRUCKF(QTM,OM,1.,0.,'OC. HEAT FLUX [W/M**2]',IIC,LP,MP)
CD          PRINT *,'Wrote QTM'
          CALL DRUCKF(ATMFLX,OM,.1,0.,'ATM. HEAT FL./10[W/M**2]',
     1       IIC,LP,MP)
CD          PRINT *,'Wrote ATMFLUX'
          WRITE(16,901)
C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
C**REMARK: MOD. FOR REAL-TIME DAILY FORCING: 6TH YEAR:+5; 5TH YEAR:+20.
         IF (MOD((IIC+5),NPLT).EQ.0) THEN
C-----------------------------------------------------------------------
C  DETERMINATION OF MONTHLY MEAN VALUES
C-----------------------------------------------------------------------
CD          DO 309 J=1,MM
CD          DO 309 I=2,LM
CD           HV=H(I,J,LNEW)+H(I,J-1,LNEW)+H(I-1,J-1,LNEW)+H(I-1,J,LNEW)
CD           FLAGI1(I,J)=.5*(1.-SIGN(1.,.0-HV))
CD           FLAGI2(I,J)=.5*(1.-SIGN(1.,.0-HU))
CD           HEX2=HEX2+FLAG1*OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))
CDCD           HWEX2=HWEX2+OM(I,J)*Z1*1.E-6*BM(I,J)/(PN(I,J)*PM(I,J))
CD  309     CONTINUE
          ENDIF
        ENDIF
       END IF
C-----------------------------------------------------------------------
C  WRITE OUT RESTART DATA
C-----------------------------------------------------------------------
      IF (MOD(IIC, NRST).EQ.0) THEN
        DO 9800 j = 0, M
          DO 9801 i = 0, L
            tempor(i,j) = H(i,j,LNEW)
 9801     CONTINUE
 9800   CONTINUE 
        WRITE (14) tempor

        DO 9802 j = 0, M
          DO 9803 i = 0, L
            tempor(i,j) = A(i,j,LNEW)
 9803     CONTINUE
 9802   CONTINUE 
CD        WRITE (14) A(1,1,LNEW)
        WRITE (14) tempor

        DO 9804 j = 0, M
          DO 9805 i = 0, L
            tempor(i,j) = HSN(i,j,LNEW)
 9805     CONTINUE
 9804   CONTINUE 
CD        WRITE (14) HSN(1,1,LNEW)
        WRITE (14) tempor

        WRITE (14) TICE
        WRITE (14) QT
        WRITE (14) QS
        WRITE (14) QH
        WRITE (14) QTB
        WRITE (14) QSB
        WRITE (14) QHB
        WRITE (14) QDT
        WRITE (14) QDS
        WRITE (14) TICM
        WRITE (14) U
        WRITE (14) V
        NRREC=NRREC+1
      END IF
  
  901 FORMAT (' TIME',3X,' ICE  ',2X,' ICE  ',2X,' ICE ',3X,' SNOW ',2X,
     1       ' ICE OUTFLOW ',2X,'AVG. HEAT FLUXES'/
     2       ' STEP',3X,'VOLUME',2X,'EXTENT',2X,' AREA',3X,'VOLUME',2X,
     3       '        CUMM ',2X,' OCEAN    ATMOS ')
  903 FORMAT (8X,        '10**3 ',2X,'10**6 ',2X,'10**6',3X,'10**3 ',2X,
     1       '10**2   10**3',2X,'            10  '/
     2       8X,        'KM**3 ',2X,'KM**2 ',2X,'KM**2',3X,'KM**3 ',2X,
     3       'KM**3   KM**3',2X,'W/M**2    W/M**2')
  902 FORMAT (1X, I4, 7F8.4, F8.3)
  960 FORMAT (5F8.4)
 9101 FORMAT (5E13.4)

      RETURN

C-----------------------------------------------------------------------
      ENTRY outstr (
     1    FLAGI1, FLAGI2, FLAGI, 
     5    OM, TAUX, TAUY, TA, TICM, QH ,
     6    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     7    CLO, T, 
     8    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS )
C     Initialize output fields and print initial conditions
      NRREC = 0
C-----------------------------------------------------------------------
C  SET RUNNING SUM VALUES TO 0
C-----------------------------------------------------------------------
C  CUMULATIVE OUTFLOW IN TERMS OF ICE THICKNESS:
      HOTSUM=.0
C  CUMULATIVE OUTFLOW IN TERMS OF SNOW THICKNESS:
CD      HOSNTSUM=.0
C  ICE EXTENT:
      HEX=.0
C  ICE AREA:
      HWEX=.0
C  ICE VOLUME:
      HSUM=.0
C  SNOW VOLUME:
      HSNSUM=.0
C  HORIZONTAL GRID CELL AREA IN KM**2 (UNCORR.IF SPHER.COORD.ARE USED):
      Z1=DX*DY*1.0E-6
C-----------------------------------------------------------------------
C  INITIAL INTEGRATIONS OVER ENTIRE DOMAIN
C-----------------------------------------------------------------------
      DO 5 J=1,MM
      DO 5 I=2,LM
       FLAG=.5*(1.-SIGN(1., 1.-ARMAG-A(I,J,1)))
       HEX=HEX+OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))*FLAG
       HWEX=HWEX+OM(I,J)*Z1*1.E-6*A(I,J,1)/(PN(I,J)*PM(I,J))
       HSUM=HSUM+OM(I,J)*Z1*1.E-6*H(I,J,1)/(PN(I,J)*PM(I,J))
       HSNSUM=HSNSUM+OM(I,J)*Z1*1.E-6*HSN(I,J,1)/(PN(I,J)*PM(I,J))
    5 CONTINUE
C-----------------------------------------------------------------------
C  PRINT OUT STATISTICS
C-----------------------------------------------------------------------
      WRITE(*, 901)
      WRITE(*, 903)
      WRITE(16, 901)
      WRITE(16, 903)
      WRITE(*, 902) 0, HSUM, HEX, HWEX, HSNSUM
      WRITE(16, 902) 0, HSUM, HEX, HWEX, HSNSUM

      RETURN
      END
      SUBROUTINE BCSH(H, LNEW, OM)
C=======================================================================
C  PURPOSE:
C     -SETS CYCLIC BOUNDARY COND. FOR VALUES AT GRID CENTER
C     -CUTS OUT DOMAIN
C  METHOD:
C     -OVERLAP OF VARIABLES AT THE SEAM
C  INTERFACE:
C     -H:    VARIABLE TO BE TREATED
C     -LNEW: RUNNING INDEX VALUE
C  MPI Original
C  Robert Grumbine
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      IMPLICIT none

      INCLUDE "icegrid.inc"
C=======================================================================
      REAL OM(0:L,0:M)
      REAL H(0:L,0:M,2)
      INTEGER LNEW
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  SET VARIABLES OUTSIDE DEFINED DOMAIN TO 0
C-----------------------------------------------------------------------
      DO 10 J=0,M
      DO 10 I=0,L
       H(I,J,LNEW)=OM(I,J)*H(I,J,LNEW)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S AS NEEDED
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 3) GO TO 9999
      DO 30 J=2,MM
       H(0,J,LNEW)=H(LM2,J,LNEW)
       H(1,J,LNEW)=H(LM ,J,LNEW)
       H(L,J,LNEW)=H(2  ,J,LNEW)
   30 CONTINUE

 9999 CONTINUE

      RETURN
      END
      SUBROUTINE BCSV(U, V, LNEW, VM)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -SETS CYCLIC BOUNDARY COND. FOR VALUES AT GRID EDGE
C     -CUTS OUT DOMAIN
C  METHOD:
C     -OVERLAP OF VARIABLES AT THE SEAM
C  INTERFACE:
C     -U:    X-COMPONENT OF VARIABLE TO BE TREATED
C     -V:    Y-COMPONENT OF VARIABLE TO BE TREATED
C     -LNEW: RUNNING INDEX VALUE
C  LAST MODIFIED: 5 January 1993
C  MPI Original
C  Robert Grumbine
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL VM(L,M)
      REAL U(L,M,3), V(L,M,3)
C=======================================================================
      INTEGER LNEW, I, J
C-----------------------------------------------------------------------
C  SET VARIABLES OUTSIDE DEFINED DOMAIN TO 0
C-----------------------------------------------------------------------
      DO 10 J=1,M
      DO 10 I=1,L
       U(I,J,LNEW)=VM(I,J)*U(I,J,LNEW)
       V(I,J,LNEW)=VM(I,J)*V(I,J,LNEW)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S AS NEEDED
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 3) GO TO 9999
      DO 30 J=2,MM
       U(1,J,LNEW)=U(LM,J,LNEW)
       V(1,J,LNEW)=V(LM,J,LNEW)
       U(L,J,LNEW)=U(2 ,J,LNEW)
       V(L,J,LNEW)=V(2 ,J,LNEW)
   30 CONTINUE

 9999 CONTINUE

      RETURN
      END
      SUBROUTINE OUTSM(H, LNEW, HOSUM, HOSM,
     1  PN, PM, OM, NOUT, IOUT, JOUT)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  Modified By:
C     Robert Grumbine       NCEP, Camp Springs MD                 Jan 93
C  PURPOSE:
C     -SMOOTH OUTFLOW POINTS TO REDUCE ADVECTION
C     -DETERMINE STATISTICS OF OUTFLOW
C  METHOD:
C     -REPLACES OUTFLOW VALUES WITH AVERAGE OF THEIR NEIGHBORS
C  INTERFACE:
C     -H:     VARIABLE AT GRID CENTER POINT
C     -LNEW:  RUNNING INDEX VALUE
C     -HOSUM: TOTAL OUTFLOW PER TIME STEP
C     -HOSM:  SPATIALLY AVERAGED TOTAL OUTFLOW PER TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL HOSUM, HOSM
      INTEGER LNEW
      REAL PM(0:L,0:M), PN(0:L,0:M)
      REAL OM(0:L,0:M)
      INTEGER NOUT, IOUT(LDO), JOUT(LDO)
      REAL H(0:L,0:M,2), TMP(0:L,0:M)
C=======================================================================
C     -TMP: TEMPORARY REGISTER
C=======================================================================
      INTEGER I, J, N
C=======================================================================
      HOSUM=0.0
      HOSM=0.0
      DO 80 N=1, NOUT
       I=IOUT(N)
       J=JOUT(N)
C-----------------------------------------------------------------------
C  DETERMINE TOTAL OUTFLOW
C-----------------------------------------------------------------------
       HOSUM=HOSUM+H(I,J,LNEW)/(PN(I,J)*PM(I,J))
C-----------------------------------------------------------------------
C  DETERMINE SPATIAL AVERAGE OF OUTFLOW VARIABLE
C-----------------------------------------------------------------------
       TMP(I,J)=(OM(I-1,J-1)+OM(I-1,J)+OM(I-1,J+1)+OM(I,J-1)+OM(I,J+1)+
     1 OM(I+1,J-1)+OM(I+1,J)+OM(I+1,J+1))
       TMP(I,J)=AMAX1(TMP(I,J),0.1)
       H(I,J,LNEW)=( H(I-1,J-1,LNEW)*OM(I-1,J-1)
     1              +H(I-1,J,LNEW)  *OM(I-1,J)
     2              +H(I-1,J+1,LNEW)*OM(I-1,J+1)
     3              +H(I,J-1,LNEW)  *OM(I,J-1)
     4              +H(I,J+1,LNEW)  *OM(I,J+1)
     5              +H(I+1,J-1,LNEW)*OM(I+1,J-1)
     6              +H(I+1,J,LNEW)  *OM(I+1,J)
     7              +H(I+1,J+1,LNEW)*OM(I+1,J+1))/TMP(I,J)
       HOSM=HOSM+H(I,J,LNEW)/(PN(I,J)*PM(I,J))
   80 CONTINUE
      RETURN
      END
      SUBROUTINE SADVECT(RH, H, U, V, LRHS, LADV)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE ADVECT IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     Robert Grumbine          NCEP, Camp Springs, MD          Jan. 1993
C  PURPOSE:
C     -CALCULATION OF ADVECTION OF A SCALAR VARIABLE FOR THE CONTINUITY
C       EQUATIONS (EQ.13 AND 14 IN HIBLER 79 AND EQ.8 IN OWENS AND
C       LEMKE 90)
C  METHOD:
C     -FORWARD-BACKWARD (MATSUNO) SCHEME (SEE MESINGER AND ARAKAWA 76)
C  INTERFACE:
C     -RH:   CHANGE OF SCALAR VARIABLE
C     -H:    SCALAR VARIABLE
C     -U:    X-COMPONENT OF VELOCITY
C     -V:    Y-COMPONENT OF VELOCITY
C     -LRHS: RUNNING INDEX FOR OLD OR NEW TIME STEP
C     -LADV: RUNNING INDEX FOR NEW TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
      REAL RH(0:L,0:M), H(0:L,0:M,2), U(L,M,2), V(L,M,2)
      INTEGER I, J, LADV, LRHS
C=======================================================================
C  ADD -D(UH)/DX TO CONTINUITY EQUATION:
      DO 10 J=1,MM
      DO 10 I=1,LM
       RH(I,J)=-0.5*((U(I+1,J,LADV)+U(I+1,J+1,LADV))
     1          *(H(I+1,J,LRHS)+H(I,J,LRHS))  /(PN(I+1,J)+PN(I,J))
     2              -(U(I,J,LADV)  +U(I,J+1,LADV))
     3          *(H(I,J,LRHS)  +H(I-1,J,LRHS))/(PN(I,J)  +PN(I-1,J)))/DX
C  ADD -D(VH)/DY:
       RH(I,J)=RH(I,J)
     1         -0.5*((V(I,J+1,LADV)+V(I+1,J+1,LADV))
     2          *(H(I,J+1,LRHS)+H(I,J,LRHS))  /(PM(I,J+1)+PM(I,J))
     3              -(V(I,J,LADV)  +V(I+1,J,LADV))
     4          *(H(I,J,LRHS)  +H(I,J-1,LRHS))/(PM(I,J)  +PM(I,J-1)))/DY
       RH(I,J)=PM(I,J)*PN(I,J)*RH(I,J)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE SDIFFUS(RH, H, LRHS)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE DIFFUS IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     A.STOESSEL               MPI, HAMBURG                         1991
C     Robert Grumbine          NCEP Camp Springs, MD                1993
C  PURPOSE:
C     -CALCULATION OF DIFFUSION OF A SCALAR VARIABLE FOR THE CONTINUITY
C       EQUATIONS (EQ.13 AND 14 IN HIBLER 79 AND EQ.8 IN OWENS AND
C       LEMKE 90)
C  METHOD:
C     -FORWARD (EULER) SCHEME FOR TIME INTEGRATION
C     -EMPLOYMENT OF LAPLACIAN AND BIHARMONIC DIFFUSION TERMS (APP.A IN
C       HIBLER 79)
C     -SUBROUTINE BCSFLX WILL BE CALLED WHICH SETS THE DIFFUSIVE FLUXES
C       EQUAL TO ZERO AT THE BOUNDARIES TO INSURE THAT THE TOTAL ICE
C       MASS DOES NOT CHANGE
C  INTERFACE:
C     -RH:   CHANGE OF SCALAR VARIABLE
C     -H:    SCALAR VARIABLE
C     -LRHS: RUNNING INDEX FOR OLD TIME STEP
C  EXTERNALS:
C     -BCSFLX: SETS FLUXES NORMAL TO BOUNDARIES TO 0
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      INTEGER LRHS
      COMMON/IPARM/H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      COMMON/DRV/DXSQ, DYSQ, SX2, SY2, SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DUMMY(2*L*M)
      REAL PM, PN, DUMMY

      REAL TMP(0:L,0:M), DFX(0:L,0:M), DFY(0:L,0:M), RH(0:L,0:M),
     1     H(0:L,0:M,2)
C=======================================================================
C     -TMP: TEMPORARY ARRAY
C     -DFX: FLUX IN X-DIRECTION
C     -DFY: FLUX IN Y-DIRECTION
C     -RH:  CONTRIBUTION TO CONTINUITY EQUATION
C     -H:   SCALAR VARIABLE
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  FIRST CALCULATE THE LAPLACIAN (HARMONIC) DIFFUSION TERMS
C-----------------------------------------------------------------------
C  CALCULATE D(H)/DX:
      DO 110 J=1,M
      DO 110 I=1,L
       DFX(I,J)=PM(I,J)*H(I,J,LRHS)-PM(I-1,J)*H(I-1,J,LRHS)
C  CALCULATE D(H)/DY:
       DFY(I,J)=PN(I,J)*H(I,J,LRHS)-PN(I,J-1)*H(I,J-1,LRHS)
  110 CONTINUE
C  SET THE BOUNDARY FLUXES EQUAL TO ZERO:
      CALL BCSFLX(DFX,DFY)
      DO 130 J=1,MM
      DO 130 I=1,LM
C  ADD D[HNU*D(H)/DX]/DX AND D[HNU*D(H)/DY]/DY TO CONTINUITY EQUATION:
       TMP(I,J)=((DFX(I+1,J)/PN(I+1,J)-DFX(I,J)/PN(I,J))/DXSQ
     1          +(DFY(I,J+1)/PM(I,J+1)-DFY(I,J)/PM(I,J))/DYSQ)
     2         *PM(I,J)*PN(I,J)
       RH(I,J)=RH(I,J)+HNU*TMP(I,J)/(PM(I,J)*PN(I,J))
  130 CONTINUE
C-----------------------------------------------------------------------
C  NOW CALCULATE THE BIHARMONIC DIFFUSION TERM
C-----------------------------------------------------------------------
      DO 210 J=1,M
      DO 210 I=1,L
       DFX(I,J)=PM(I,J)*TMP(I,J)-PM(I-1,J)*TMP(I-1,J)
       DFY(I,J)=PN(I,J)*TMP(I,J)-PN(I,J-1)*TMP(I,J-1)
  210 CONTINUE
      CALL BCSFLX(DFX,DFY)
      DO 230 J=1,MM
      DO 230 I=1,LM
       TMP(I,J)=((DFX(I+1,J)/PN(I+1,J)-DFX(I,J)/PN(I,J))/DXSQ
     1          +(DFY(I,J+1)/PM(I,J+1)-DFY(I,J)/PM(I,J))/DYSQ)
     2         *PM(I,J)*PN(I,J)
       RH(I,J)=RH(I,J)-HNU2*TMP(I,J)/(PM(I,J)*PN(I,J))**3
  230 CONTINUE
      RETURN
      END
      SUBROUTINE SHDEF(LRHS, OPEW, U, V)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CREATION OF EXTRA OPEN WATER DUE TO SHEAR DEFORMATION (HIBLER 84)
C  METHOD:
C     -ADDITIONAL DYNAMIC TERM ENTERING THE COMPACTNESS EQUATION
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR NEW TIME STEP
C     -OPEW: STRAIN RATE PART OF EQ.A7 IN STOESSEL (90)
C  EXTERNALS:
C     -STRAIN: CALCULATES STRAIN RATE TENSOR
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
C     COMMON viscp moved to rheology.inc
      INTEGER LRHS
CD      COMMON/WORK/TMP(1:L,1:M), E11(1:L,1:M), E22(1:L,1:M), E12(1:L,1:M)
CD     1 ,  SPACE(1:L,1:M,8)
CD      REAL TMP, E11, E22, E12, SPACE
      REAL E11(1:L,1:M), E12(1:L,1:M), E22(1:L,1:M)
      REAL U(L,M,3), V(L,M,3)
      REAL OPEW(0:L,0:M)
      INTEGER I, J
      REAL DELT, DELT1
C=======================================================================
      CALL STRAIN(LRHS, E11, E12, E22, U, V)

      DO 1 J = 0, M
      DO 1 I = 0, L
        OPEW(I,J) = 0.0
   1  CONTINUE

      DO 10 J=1,MM
      DO 10 I=1,LM
       DELT=(E11(I,J)**2+E22(I,J)**2)*(1.0+ECM2)+4.0*ECM2*E12(I,J)**2
     1     +2.0*E11(I,J)*E22(I,J)*(1.0-ECM2)
       DELT1=SQRT(DELT)
       OPEW(I,J)=0.5*(DELT1-E11(I,J)-E22(I,J))
   10 CONTINUE

      RETURN
      END
      SUBROUTINE VECMAX(F1, VALUE, F2)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING A CONSTANT
C  LAST MODIFIED: 5 January 1993
C  MPI Original
C  Robert Grumbine
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL F1(0:L,0:M), F2(0:L,0:M), VALUE
      INTEGER I, J
      REAL FLAG
C=======================================================================
      DO 1 J=0, M
      DO 1 I=0, L
       FLAG=SIGN(1.,F1(I,J)-VALUE)
       F2(I,J)=0.5*(F1(I,J)*(1.+FLAG)+VALUE*(1.-FLAG))
    1 CONTINUE
      RETURN
C=======================================================================
      ENTRY VECMIN(F1, VALUE, F2)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MINIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING A CONSTANT
C=======================================================================
      DO 2 J=0, M
      DO 2 I=0, L
       FLAG=SIGN(1.,F1(I,J)-VALUE)
       F2(I,J)=0.5*(F1(I,J)*(1.-FLAG)+VALUE*(1.+FLAG))
    2 CONTINUE
      RETURN
      END
      SUBROUTINE VECMAXC(F1, F2, F3)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING AN ARRAY VARIABLE
C  LAST MODIFIED: 5 January 1993
C  MPI Original
C  Robert Grumbine
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL F1(0:L,0:M), F2(0:L,0:M), F3(0:L,0:M)
      INTEGER I, J
      REAL FLAG
C=======================================================================
      DO 1 J=0,M
      DO 1 I=0,L
       FLAG=SIGN(1.,F1(I,J)-F2(I,J))
       F3(I,J)=0.5*(F1(I,J)*(1.+FLAG)+F2(I,J)*(1.-FLAG))
    1 CONTINUE
      RETURN
C=======================================================================
      ENTRY VECMINC(F1, F2, F3)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING AN ARRAY VARIABLE
C=======================================================================
      DO 2 J=0, M
      DO 2 I=0, L
       FLAG=SIGN(1.,F1(I,J)-F2(I,J))
       F3(I,J)=0.5*(F1(I,J)*(1.-FLAG)+F2(I,J)*(1.+FLAG))
    2 CONTINUE
      RETURN
      END
      REAL FUNCTION albedo(ts, ta, hs, hi)
C     Albedo of possibly snow covered sea ice.
C     Implementation of the algorithm from Ross and Walsh, 1987.
C     ts is surface temperature of the ice/snow in C
C     ta is the atmospheric temperature in C
C     hs is the snow thickness (in meters)
C     hi is the ice thickness (in meters)
C     In this algorithm, the ice thickness is not needed.  The
C       argument is passed for future modifications.  The snow
C       thickness is only needed to determine if there is snow
C       present.  It, too, is included for future experiments.
C     12 September 1994  Robert Grumbine - Original Implementation
C     24 January 1996 BG - Fix initial clause, hi, not hs.

      IMPLICIT none

      REAL ts, ta, hs, hi

      IF (hi .GT. 0.) THEN
        IF (ts .LT. -5.) THEN
          albedo = 0.8
        ELSE IF (ts .LT. 0.) THEN
          albedo = 0.65 - 0.03*ts
        ELSE 
          albedo = 0.65
        ENDIF

       ELSE IF (hs .GT. 0.) THEN
        IF (ts .LT. 0.) THEN
          albedo = 0.65
         ELSE IF (ta .LT. 5.) THEN
          albedo = 0.65 - 0.04 * ta
         ELSE
          albedo = 0.45
        ENDIF

       ELSE
C        Ocean albedo.
         albedo = 0.10
 
      ENDIF
        
      RETURN
      END
      LOGICAL FUNCTION noice(A, nx, ny, ns, lstep)
C     Function returns true if there is some ice on the indicated
C       (lstep) time step, false otherwise.  Useful for avoiding
C       computing ice velocities when there is no ice.
C     Robert Grumbine 15 October 1996.

      IMPLICIT none

      INTEGER nx, ny, ns, lstep
      REAL A(nx, ny, ns)
      INTEGER i, j
      LOGICAL temp

      temp = .TRUE.
      DO 1000 j = 1, ny
        DO 1000 i = 1, nx
          IF (A(i,j,lstep) .NE. 0) THEN
            noice=.FALSE.
            RETURN
          ENDIF
 1000   CONTINUE

      noice = temp
      RETURN
      END
      LOGICAL FUNCTION overload(x, nx, ny, limit)
C     Function to decide if any elements of x exceed (in absolute value)
C       the limit that is passed in.
C     Robert Grumbine 10/17/96

      IMPLICIT none
      REAL limit
      INTEGER nx, ny
      REAL x(nx, ny)

      INTEGER i, j
      LOGICAL temp

CD      PRINT *,'Entering overload'
      temp = .FALSE.
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (ABS(x(i,j)) .GT. limit) THEN
          temp = .TRUE.
          x(i,j) = SIGN(limit, x(i,j) )
        ENDIF
 1000 CONTINUE

      overload = temp

CD      PRINT *,'Leaving overload'
      RETURN
      END
      REAL FUNCTION tfreez(salinity)
C     Constants taken from Gill, 1982.
C     Author: Robert Grumbine
C     LAST MODIFIED: 21 September 1994.

      IMPLICIT none

      REAL salinity
      REAL a1, a2, a3
      PARAMETER (a1 = -0.0575)
      PARAMETER (a2 =  1.710523E-3)
      PARAMETER (a3 = -2.154996E-4)

      salinity = AMAX1(0., salinity)
      tfreez = salinity*(a1+a2*SQRT(salinity)+a3*salinity)

      RETURN
      END
      SUBROUTINE ADJUEX(QHST, SNOW, FLAGI, OM, TMP, TMP2, TMP3,
     1  QS, QSB, QT, QTB, QH, QHB, QDS, QDT, HS, HT, MLFIX)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C     -Robert Grumbine        NMC Camp Springs                      1994
C  PURPOSE:
C     -ADJUSTMENT OF MIXED LAYER VARIABLES WHICH WHERE MODIFIED BY EN-
C       TRAINMENT OR DETRAINMENT AND BY ADVECTION
C  INTERFACE:
C     -QHST:  CHANGE IN ICE THICKNESS OR HEAT STORAGE (IF NEGATIVE)
C     -SNOW:  CURRENT SNOW DEPTH
C     -FLAGI: FLAG FIELD FOR PRESENCE OF ICE AND/OR SNOW
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMIN:  THE THIRD ARGUMENT IS MINIMUM OF THE FIRST TWO ARGUMENTS
C     -VERDIF:  DETERMINES VERTICAL DIFFUSION
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
      INCLUDE "oml.inc"
C=======================================================================
      REAL QS(0:L,0:M), QSB(0:L,0:M), QT(0:L,0:M), QTB(0:L,0:M) 
      REAL QH(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M)
      REAL HS(0:L,0:M), HT(0:L,0:M)
      INTEGER  MLFIX

      REAL OM(0:L, 0:M)

      REAL TMP(0:L, 0:M), TMP2(0:L,0:M), TMP3(0:L, 0:M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C=======================================================================
      REAL QHST(0:L,0:M), SNOW(0:L,0:M), FLAGI(0:L,0:M), TMP4(0:L,0:M)
     1  ,TMP5(0:L,0:M)
      REAL FLAGM, SS, TT, QFMO1, QSFO, QSM, QOC
      REAL tfreez
      INTEGER I, J
C=======================================================================
      FLAGM=1.0-FLOAT(MLFIX)
      CALL VECMAX(QHST,0.,TMP)
C     Ensure that mixed layer thickness is non-zero.  Robert Grumbine 
      CALL VECMAX(QH, minmix, QH)

C Correction - Stoessel - 17 Feb 1993 - QTOC->QOC to avoid modifying
C    QTOC, which is a common block variable.  Has effect if MLFIX=1.
C    Change Made 20 March 1995. Bob Grumbine
      DO 210 J=1,MM
      DO 210 I=0,L
       QOC=( tfreez(QS(I,J)) -QT(I,J) )*QH(I,J)*FLAGI(I,J)*OM(I,J)
       HT(I,J)=HT(I,J)+QOC*FLAGI(I,J)
       QHST(I,J)=QHST(I,J)+CC/CLO*QOC*FLAGI(I,J)
  210 CONTINUE
      CALL VECMAX(QHST,0.,TMP2)
      CALL VECMIN(QHST,0.,TMP3)
      DO 215 J=1,MM
      DO 215 I=0,L
       TMP4(I,J)=SNOW(I,J)
       SNOW(I,J)=SNOW(I,J)+RHOICE/RHOSNO*TMP3(I,J)*OM(I,J)
  215 CONTINUE
      CALL VECMIN(SNOW,0.,TMP3)
      CALL VECMAX(SNOW,0.,TMP5)
      DO 220 J=1,MM
      DO 220 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
       SS=SS*FLAGI(I,J)+1.0-FLAGI(I,J)
       TT=TT*FLAGI(I,J)+1.0-FLAGI(I,J)
       SNOW(I,J)=TMP5(I,J)
       TMP3(I,J)=TMP3(I,J)*RHOSNO/RHOICE
       QFMO1=(TMP(I,J)-TMP2(I,J))*RHOICE/RHOWAT
       QSM=(TMP4(I,J)-SNOW(I,J))*RHOSNO/RHOWAT
       QSFO=-QFMO1*(QS(I,J)-SICE)-QSM*QS(I,J)
       HS(I,J)=HS(I,J)+QSFO*FLAGI(I,J)*OM(I,J)
       HT(I,J)=HT(I,J)-TMP3(I,J)*CLO/CC*FLAGI(I,J)*OM(I,J)
CBG       QT(I,J)=QT(I,J)-(QT(I,J)-TFREZ+TMP3(I,J)*CL/CC/QH(I,J))
C      BG  Original line is above.  CL is unitialized, but CLB and
C        CLO are initialized (COMMON /ABLM/).  They are ice density
C        times latent heat of fusion at bottom and top, respectively
C        of the ice surface.  Insert CLO here, as that is the
C        variable used elsewhere (and CLO and CLB should be
C        fairly close to each other.  BG 4/22/92.
       IF (QH(I,J) .EQ. 0.) STOP "QH = 0"
       QT(I,J)=QT(I,J)- (QT(I,J)- tfreez(QS(I,J)) + TMP3(I,J)
     1         *CLO/CC/QH(I,J)     )
     1         *FLAGI(I,J)*OM(I,J)
       QS(I,J)=QS(I,J)+QSFO/QH(I,J)*FLAGI(I,J)*OM(I,J)
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)

C      Following two lines added by Robert Grumbine to ensure against
C        divisions by zero.  19 September 1994.
       IF (SS .EQ. 0.) SS = 1.E-2
       IF (TT .EQ. 0.) TT = 1.E-2

       SS=SS*FLAGI(I,J)+1.0-FLAGI(I,J)
       TT=TT*FLAGI(I,J)+1.0-FLAGI(I,J)

CD       IF (ss .LT. 0.1) THEN
CD         PRINT *,'ss = ',i,j, ss
CD       ENDIF
CD       IF (tt .LT. 0.1) THEN
CD         PRINT *,'tt = ',i,j, tt
CD       ENDIF

       QDT(I,J)=QDT(I,J)-(QDT(I,J)-(QTB(I,J)*QHB(I,J)-HT(I,J))/TT
     1          +QH(I,J))*FLAGI(I,J)*FLAGM*OM(I,J)
       QDS(I,J)=QDS(I,J)-(QDS(I,J)-(QSB(I,J)*QHB(I,J)-HS(I,J))/SS
     1          +QH(I,J))*FLAGI(I,J)*FLAGM*OM(I,J)
  220 CONTINUE
      IF (MLFIX.EQ.1) GOTO 300
      CALL VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)

  300 CONTINUE

      RETURN
      END
      SUBROUTINE BUDGET(FH,T,LRHS,KG, LWDN, SWDN, QS, IIC,
     1 TAIR, TD, PA, UG, TA, OM, FLSE, FLLA, HICE, ALB, A2, H, A, HSN)
C=======================================================================
C  PROGRAMMED BY:
C     A.STOESSEL               MPI, HAMBURG                         1989
C     R. Grumbine              NMC, Camp Springs                    1992
C  PURPOSE:
C     -CALCULATION OF GROWTH RATES FOR THE ICE COVERED PART OF A GRID
C       CELL WITH STANDARD BULK FORMULAS
C  METHOD:
C     -ICE OR SNOW SURFACE TEMPERATURES, RESPECTIVELY, ARE CALCULATED BY
C       ITERATION (REGULA FALSI)
C  INTERFACE:
C     -FH:   ICE GROWTH RATE
C     -T:    ICE OR SNOW SURFACE TEMPERATURE [CELSIUS]
C     -LRHS: RUNNING INDEX VALUE FOR OLD TIME STEP
C     -KG:   INDEX FOR ICE THICKNESS CATEGORIES
C     -LWDN: Downwelling Longwave Robert Grumbine 1992.
C     -SWDN: Downwelling Shortwave Robert Grumbine 1992.
C     -QS  : Sea water salinity (for tfreez) Robert Grumbine 13 Sep 1994.
C  EXTERNALS:
C     -VAPOR: CALCULATES VAPOR PRESSURE
C     -tfreez: Compute the freezing point of salt water
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      INTEGER LRHS 
C=======================================================================
      INTEGER KG

      INTEGER IIC

      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L, 0:M, 2)

      REAL TAIR(0:L,0:M), TD(0:L,0:M), PA(0:L,0:M), 
     1      UG(0:L,0:M), TA(0:L,0:M)

      REAL OM(0:L, 0:M)

      REAL FLSE(0:L,0:M), FLLA(0:L,0:M)

      REAL HICE(0:L,0:M), ALB(0:L,0:M), A2(0:L,0:M)
C=======================================================================
C     -HICE:  EFFECTIVE ICE THICKNESS (OPTIONALLY FOR SEVEN CATEGORIES)
C     -WRK:   DUMMY ARRAYS
C     -ALB:   ALBEDO
C     -TRK:   DUMMY ARRAY
C     -A2:    FLAG FOR SNOW CONDITIONS
C     -FLAI:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C=======================================================================
      REAL FH(0:L,0:M), T(0:L,0:M), ESTA(LMDP), ESTI(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI: SATURATION VAPOR PRESSURE OVER ICE
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1 FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), T1(LMDP),
     2 ALB1(LMDP), A21(LMDP), HICE1(LMDP), hsnow1(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL QS(0:L, 0:M), TB1(LMDP)
C=======================================================================
C  REMARK: FOLLOWING VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), DIFF(LMDP),
     1  TT(LMDP), TT1(0:L,0:M), TMYUS1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
C  Declare remaining local variables
      REAL fhb, fhi, q1, q2, q3, albedo, tfreez
      REAL a1, flag, fdiff, ea, d1, d2i
      INTEGER n, iter, imax
      INTEGER i, j, k
      REAL limdel, delmax
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
      PARAMETER (IMAX=30)
      PARAMETER (limdel = 0.001)
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
C     IF _either_ H or A is zero, skip the point (after resetting the other
C       to be consistent), BG 10/16/96.
      DO 79 J=1,MM
      DO 79 I=0,L
       TMYUS1(I,J)=OM(I,J)+2.
       IF (A(I,J,LRHS).EQ.0. .OR. HICE(I,J) .EQ. 0.) THEN
         TMYUS1(I,J)=2.
         A(I,J,LRHS) = 0.
         HICE(I,J) = 0.
         HSN(I,J,LRHS) = 0.
       ENDIF
   79 CONTINUE
C-----------------------------------------------------------------------
C  STORE EXTERNAL VARIABLES INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K = 0
      DO 82 J = 1,MM
      DO 82 I = 0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 82
CD       PRINT *,'TMYUS1, OM, HICE, A ',I, J, TMYUS1(I,J), OM(I,J), 
CD     1                   HICE(I,J), A(I,J,LRHS)
       K = K+1
       HICE1(K) = HICE(I,J)
       hsnow1(K) = HSN(I,J, LRHS)
       ALB1(K) = ALB(I,J)
       A21(K) = A2(I,J)
       T1(K) = T(I,J)+TMELT
       TA1(K) = TAIR(I,J)+TMELT
       TD1(K) = MAX(.1,TD(I,J)/100.)
C      Added for tfreez = fn of salinity rather than constant.
       TB1(K) = tfreez(QS(I,J)) + TMELT
       PA1(K) = PA(I,J)
       UG1(K) = MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
CD       SWDN1(K) = SWDN(I,J) * (1. - albedo(T(I,J), TAIR(I,J), 
CD     1                                 HSN(I,J,LRHS), H(I,J,LRHS)) )
C      Albedo computed in GROWTH.  Bob Grumbine 25 October 1994.
       SWDN1(K) = SWDN(I,J) * (1. - ALB1(K))
   82 CONTINUE
CD      PRINT *,'Passed DO 82 in BUDGET'
C     Note that HSNOW is apparently unused in this routine.  BG 10/25/94.
C     Deleted 9/17/96.  BG
      IF (K.EQ.0) GOTO 87
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(T1,ESTI,2,K)
      D1 = RHOAIR*CPAIR*CSENS
      D2I = RHOAIR*SUBL*CLAT
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR SURFACE TEMPERATURE
C-----------------------------------------------------------------------
CD      PRINT *,'Starting DO 33 in BUDGET'
      DO 33 N = 1,K
       STP(N) = T1(N)
       EA = TD1(N)*ESTA(N)
       IF (PA1(N) .EQ. 0.0 .OR. HICE1(N) .EQ. 0.0) THEN
         PRINT *,'n, nmax, pa1, hice1 = ',N, K, PA1(N), HICE1(N)
         STOP 
       ENDIF
       FP(N) = D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1       -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2       EPSI/PA1(N)+(STP(N)-TB1(N) )/HICE1(N)*CON
       T1(N) = T1(N)+1.
       TT(N) = 0.
   33 CONTINUE
CD      PRINT *,'Passed DO 33 in BUDGET'
C-----------------------------------------------------------------------
C  CALCULATE THE SURFACE TEMPERATURE (START OF ITERATION PROCEDURE)
C-----------------------------------------------------------------------
      delmax = 0.
      DO 3 ITER = 1,IMAX
CD       PRINT *,'In budget, iteration = ',ITER, delmax
       CALL VAPOR(T1,ESTI,2,K)
       delmax = 0.
       DO 34 N = 1,K
        STPP(N) = STP(N)
        FPP(N) = FP(N)
        STP(N) = T1(N)
        EA = TD1(N)*ESTA(N)
        FP(N) = D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1        -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2        EPSI/PA1(N)+(STP(N)-TB1(N) )/HICE1(N)*CON
        FDIFF = FP(N)-FPP(N)
        T1(N) = STP(N)-(STP(N)-STPP(N))*FP(N)/
     1        MAX(ABS(FDIFF), 1.E-6)*SIGN(1.,FDIFF)
        DIFF(N) = T1(N)-STP(N)
        TT(N) = SIGN(1.,.01-ABS(DIFF(N)))
CD        delmax = AMAX0(delmax, ABS(DIFF(N)) )
        delmax = MAX(delmax, ABS(DIFF(N)) )
   34  CONTINUE
       IF (delmax .LT. limdel ) GO TO 9999
    3 CONTINUE
C
C  IF TEMPERATURE ITERATION FAILED TO CONVERGE, RESET T1 TO TMELT.
C  Robert Grumbine 21 February 1997: If T1 is negative (T1 is in K!)
C    reset T1 to tmelt and warn
c
      DO 35 N = 1, K
        IF (TT(N) .LE. 0.) T1(N) = TMELT
        IF (T1(N) .LE. 0.) THEN
            T1(N) = TMELT
            PRINT *,'Budget - iterated to a negative K temperature'
        ENDIF
 35   CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED HEAT BALANCE EQUATION
C-----------------------------------------------------------------------
 9999 CONTINUE

      DO 83 N = 1,K
       FLAG = .5*(1.+SIGN(1.,T1(N)-TMELT))
       T1(N) = T1(N)*(1.-FLAG)+TMELT*FLAG
       EA = TD1(N)*ESTA(N)
       A1 = 0.5*(1.+SIGN(1.,T1(N)-TMELT))
CBG       ALB1(N) = A21(N)*ALBSNM+(1.-A21(N))*ALBM
       ALB1(N) = albedo(t1(N), ta1(N), hice1(N), hsnow1(N)) 
       FLSE1(N) = D1*UG1(N)*(TA1(N)-T1(N))
       FLLA1(N) = D2I*UG1(N)*(EA-ESTI(N))*EPSI/PA1(N)
       Q1 = D3*T1(N)**4
       Q2 = SWDN1(N) 
       Q3 = LWDN1(N)
       FHI = A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB1(N) -T1(N))/HICE1(N)
     1                 *CON) /CLO
       FHB = ((TB1(N) - T1(N))/HICE1(N)*CON)/CLB
       FH1(N) = FHI+FHB
   83 CONTINUE
CD       PRINT *,'Passed DO 83 in BUDGET'
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K = 0
      DO 84 J = 1,MM
      DO 84 I = 0,L
       IF (OM(I,J).EQ.0.) GOTO 84
       IF (A(I,J,LRHS).EQ.0.) GOTO 84
       K = K+1
       FH(I,J) = FH1(K)
       T(I,J) = T1(K)-TMELT
       TT1(I,J) = TT(K)
       IF (TT1(I,J).LE.0.) WRITE(16,701) IIC
       IF (KG.NE.4) GOTO 84
       TA(I,J) = TA1(K)
       FLSE(I,J) = FLSE1(K)
       FLLA(I,J) = FLLA1(K)
       IF ( FH(I,J) *CLO .GT. 350.*1.0 .OR. 
     1      FH(I,J) *CLO .LT. -350.*1.0      ) THEN
         WRITE (*,9001) I, J, FH(I,J)*CLO, T1(K), TA(I,J), FLSE(I,J), 
     1     FLLA(I,J), SWDN1(K), LWDN(I,J),
     1     D3*T1(K)**4 - SWDN1(K) -LWDN(I,J)
       ENDIF
   84 CONTINUE

   87 CONTINUE

CD      PRINT *,'Atmflux on leaving budget (mul by clo for flux)',FH

 9001 FORMAT ( 2I4, 8F9.1, 
     1         ' Ice, Net, Ti, Ta, FLSE, FLLA, SWD, LWD, Net rad')

  701 FORMAT (1X,I4,'Budget iteration exceeded')

      RETURN
      END
CD      SUBROUTINE GROWTH(LRHS, LNEW, LWDN, SWDN, bathy, hmlref,
      SUBROUTINE GROWTH(LRHS, LNEW, LWDN, SWDN, bathy, 
     1  DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2  SNOFLG, TICM, U, V,
     3  QTM, ATMFLX, SURTYP, 
     4  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, 
     5  QV, QRHO, QW, FW, IEN, MLFIX,
     6  IIC, H, A, HSN,
     7  H0, ARMIN, ARMAX, HMIN,
     8  TAIR, TD, ACL, PA, UG, TA, RPREC,
     9  OM)
C=======================================================================
C  PROGRAMMED BY:
C     W.D.HIBLER III           CRREL, HANOVER, USA                  1979
C     W.B.OWENS                MPI, HAMBURG                         1987
C     R. W. Grumbine           NMC, Camp Springs, MD                1992
C     R. W. Grumbine           NMC, Camp Springs, MD             Jul. 95
C     R. W. Grumbine           NCEP, Camp Springs, MD            Feb. 97
C  PURPOSE:
C     -CALCULATION OF CHANGES IN ICE THICKNESS, ICE COMPACTNESS AND SNOW
C       THICKNESS DUE TO THERMODYNAMIC EFFECTS
C  METHOD:
C     -CALCULATES HEAT BUDGETS FOR OPEN WATER PART AND ICE COVERED PART
C       OF A GRID CELL, THE LATTER BEING OPTIONAL FOR A SEVEN-LEVEL
C       ICE THICKNESS DISTRIBUTION ACC. TO HIBLER (84)
C     -CALCULATES ADDITIONAL DYNAMIC TERM IN ICE COMPACTNESS EQUATION
C       ACC. TO HIBLER (84)
C     -DETERMINES SNOW THICKNESS ACC. TO OWENS AND LEMKE (90)
C     -TAKES INTO CONSIDERATION THE VERTICAL OCEANIC HEAT FLUX, WHICH
C       IS CALCULATED IN THE OML-ROUTINE
C     -Downwelling radiation accepted as an argument. BG
C     -Ocean bathymetry accepted as an argument. BG
C  OPTIONS:
C     -INCLUSION OF SEVEN-LEVEL ICE THICKNESS DISTRIBUTION
C     -INCLUSION OF Variable number of levels, selected in icegrid.inc
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD TIME STEP
C     -LNEW: RUNNING INDEX FOR NEW TIME STEP
C  EXTERNALS:
C     -SHDEF:   CREATES OPEN WATER DUE TO SHEAR DEFORMATION
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMIN:  THE THIRD ARGUMENT IS MINIMUM OF THE FIRST TWO ARGUMENTS
C     -SHWARA:  CALCULATES SHORT WAVE RADIATION
C     -SHWARA:  Replaced by a read in FORFLD
C     -OBUDGET: CALCULATES OPEN WATER HEAT BUDGET
C     -ECMBDO: CALCULATES OPEN WATER HEAT BUDGET WITH ASL-MODEL
C     -EKMAO:   CALCULATES OPEN WATER HEAT BUDGET WITH ABL-MODEL
C     -BUDGET:  CALCULATES HEAT BUDGET OVER ICE
C     -ECMBDI: CALCULATES HEAT BUDGET OVER ICE WITH ASL-MODEL
C     -EKMAH:   CALCULATES HEAT BUDGET OVER ICE WITH ABL-MODEL
C     -PMLEX:   CALCULATES OML-VARIABLES (OML-MODEL)
C     -VECMINC: SAME AS VECMIN, THE SECOND ARGUMENT BEING AN ARRAY
C     -tfreez:  Compute the freezing point of salt water
C     -albedo:  Compute the albedo of potentially snow covered sea ice
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
CD      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
CD      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      REAL H0, ARMIN, ARMAX, HMIN

CD      COMMON/VEL/U(L,M,3), V(L,M,3)
      REAL U(L, M, 3), V(L, M, 3)

CD      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
CD      REAL T
CD      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      INTEGER IIC

CD      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)

CD      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
CD      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
CD     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
      REAL UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)

CD      COMMON/TEMP/TICE(0:L,0:M)
CD      COMMON/TEMPM/TICM(0:L,0:M,NLEVEL)
      REAL TICM(0:L, 0:M, NLEVEL)

CD      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL OM(0:L,0:M)

CD      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
CD     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
CD     2 QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M),
CD     3 QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M)
      REAL QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

CD      COMMON/SNOFLG/SNOFLG
      REAL SNOFLG

CD      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      REAL SURTYP
CD      COMMON/WORK/TMP(0:L,0:M), RH(0:L,0:M), RA(0:L,0:M), ALB(0:L,0:M),
CD     1 TMP3(0:L,0:M), TMP4(0:L,0:M), QHST(0:L,0:M), QFM(0:L,0:M),
CD     2 PREC(0:L,0:M), SN(0:L,0:M), QTM(0:L,0:M), SH(0:L,0:M)
      REAL TMP(0:L,0:M), RH(0:L,0:M), RA(0:L,0:M), ALB(0:L,0:M),
     1 TMP3(0:L,0:M), QHST(0:L,0:M), QFM(0:L,0:M),
     2 PREC(0:L,0:M), SN(0:L,0:M), QTM(0:L,0:M), ATMFLX(0:L,0:M)

       REAL TMP2(0:L,0:M)
       REAL A2(0:L, 0:M), FLSE(0:L, 0:M), FLLA(0:L, 0:M)

       INTEGER LRHS, LNEW 
       REAL ABLFIX, ECMTYP

       REAL hdraft(0:L, 0:M), bathy(0:L, 0:M), OPEW(0:L, 0:M)
CD       REAL hmlref ! hmlref is in include now
       REAL DCVM, WUP, COSGAM, RTC, STC, QTOC
C=======================================================================
C     -TMP:  TEMP. ARRAY (PASSES THE EFFECTIVE ICE THICKNESS TO BUDGET)
C     -RH:   TEMP. ARRAY (RETURNS GROWTH RATE OF THICK ICE FROM BUDGET)
C     -RA:   TEMP. ARRAY (RETURNS GROWTH RATE OF THIN ICE FROM BUDGET)
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C     -TMP4: TEMPORARY ARRAY
C     -QHST: CHANGE IN ICE THICKNESS OR HEAT STORAGE (IF NEGATIVE)
C     -QFM:  AMOUNT OF ICE MELTED [M]
C     -PREC: FRESH WATER INPUT (SNOW AND ICE MELT + RAIN)
C     -SN:   TEMPORARY ARRAY (EFFECTIVE ICE THICKNESS OR SNOW DEPTH)
C     -QTM:  VERTICAL OCEANIC HEAT FLUX (ON OUTPUT OF PMLEX)
C     -SH:   TEMP.ARR.(7-LEVEL ICE THICKN.AND THERMODYN.THICKN.CHANGE)
C     -SH:   TEMP.ARR.(N-LEVEL ICE THICKN.AND THERMODYN.THICKN.CHANGE)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M)
C     ARMAG is the parameter deciding when ice is compact or not.
      REAL ARMAG
      PARAMETER (ARMAG = 0.85)
      LOGICAL overload
      REAL albedo, tfreez
      INTEGER i, j, k


C-----------------------------------------------------------------------
CD      PRINT *,'Entered GROWTH'
      ABLFIX = 0.
      ECMTYP = 1.
C-----------------------------------------------------------------------
C  CREATE OPEN WATER DUE TO SHEAR DEFORMATION
C-----------------------------------------------------------------------
CD      PRINT *,'Calling SHDEF'
      CALL SHDEF(LNEW, OPEW, U, V)
CD      PRINT *,'Returned from SHDEF'
      CALL VECMAX(A(0,0,LRHS), ARMAG, TMP)
      DO 301 J=1,MM
      DO 301 I=0,L
       TMP2(I,J)=(1.0-TMP(I,J))*(0.5*(1.0+TMP(I,J))-ARMAG)
     1           /((1.-ARMAG)*(0.5*(1.+ARMAG)-ARMAG))
       A(I,J,LNEW)=A(I,J,LNEW)-OPEW(I,J)*(1.0-TMP2(I,J))*DT
  301 CONTINUE
CD      PRINT *,'Past DO 301'
C-----------------------------------------------------------------------
C  CALCULATE COS OF ZENITH DISTANCE AND SOLAR CONSTANT
C   Removed by BG.  Now use externally computed radiation
C-----------------------------------------------------------------------
C  CALCULATE THE GROWTH RATE FOR THIN ICE (OPEN OCEAN)
C-----------------------------------------------------------------------
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP.  According to SURTYP
CD      PRINT *,'about to invoke overload on LWDN'      
      IF (overload(LWDN, LP, MP, 10000.)) THEN
        PRINT *,'Overloaded LWDN in top of growth, 10,000 cutoff'
      ENDIF
CD      PRINT *,'Calling ocean surface budget. surtyp = ', SURTYP
      IF (SURTYP .EQ. 0.) THEN
CBG        CALL OBUDGET(RA, QT, LWDN, SWDN)
CD        PRINT *,'Calling obudget'
        CALL OBUDGET(RA, QT, LWDN, SWDN, TAIR, TD, PA, UG, TA, OM, 
     1                FLSE, FLLA)
       ELSEIF (SURTYP .EQ. 1.) THEN
CBG 24 Feb 1997       CALL ECMBDO(RA, QT, LWDN, SWDN)
           PRINT *,'Surface type 1 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSEIF (SURTYP .EQ. 2.) THEN
CBG 24 Feb 1997        CALL EKMAO(RA, QT, LWDN, SWDN, QS)
           PRINT *,'Surface type 2 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSE
        PRINT *,'Selected surtyp out of range.  surtyp = ',surtyp
        STOP
      ENDIF
C-----------------------------------------------------------------------
C  CALCULATE EFFECTIVE ICE THICKNESS
C-----------------------------------------------------------------------
C  MAKE SURE WE HAVE NON-ZERO COMPACTNESS:
      CALL VECMAX(A(0,0,LRHS), ARMIN,TMP)
      DO 5 J=1,MM
      DO 5 I=0,L
C  INCLUDE SNOW THICKNESS FOR CONDUCTION EFFECT THROUGH SNOW:
       TMP(I,J)=(H(I,J,LRHS)+HSN(I,J,LRHS)*CON/CONSN)/TMP(I,J)
C**FOR 7 LAYER THERMODYNAMICS, INSERT THE FOLLOWING STATEMENT:
C**FOR N LAYER THERMODYNAMICS, INSERT THE FOLLOWING STATEMENT:
       RH(I,J)=0.0
    5 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES FOR THICK ICE
C-----------------------------------------------------------------------
C  MAKE SURE WE HAVE NON-ZERO THICKNESS FOR CONDUCTION TERM IN BUDGET:
      CALL VECMAX(TMP,HMIN,TMP)
C**FOR 7 LEVEL MODEL OF ICE, INSERT THE FOLLOWING 2 STATEMENTS:
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING 2 STATEMENTS:
      CALL VECMAX(TMP,HMIN,SN)

C Set Snow condition flags.  Robert Grumbine 24 February 1997
      DO 8000 J = 0, M
      DO 8000 I = 0, L
        IF (HSN(I,J,LRHS) .GT. 0.) THEN
          A2(I,J) = 1.0
         ELSE
          A2(I,J) = 0.0
        ENDIF
 8000 CONTINUE

      DO 8 K=1, NLEVEL
       DO 6 J=1,MM
       DO 6 I=0,L
C  SET ALBEDO ACC.TO PRESENCE OF SNOW(TMP4) AND MELTING COND.(TMP3):
CBG        TMP4(I,J)=0.5*(1.-SIGN(1.,-HSN(I,J,LRHS)))
CBG        TMP3(I,J)=0.5*(1.+SIGN(1.,TICE(I,J)))
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP3(I,J)=0.5*(1.+SIGN(1.,TICM(I,J,K)))
CBG     Change over to a called albedo routine
CBG     Bob Grumbine 25 October 1994.
        ALB(I,J) = albedo(tmp3(I,j), TAIR(I,J), 
     1                  HSN(i,j,LRHS), H(i,j,LRHS) ) 
CBG        TMP2(I,J)=TMP4(I,J)*(TMP3(I,J)*ALBSNM+(1.-TMP3(I,J))*ALBSN)
CBG     1           +(1.-TMP4(I,J))*(TMP3(I,J)*ALBM+(1.-TMP3(I,J))*ALBI)
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP(I,J)=(2*K-1)*SN(I,J)/FLOAT(NLEVEL)
    6  CONTINUE
C      CALL EKMAH (RH,TICE, LWDN, SWDN)
C**FOR N LEVEL MODEL,INS. THE FOLL.6 STATEM.AND COMM.OUT PREV.STATEM.:
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP:
CD      PRINT *,'Calling ice surface budget'
      IF (SURTYP .EQ. 0.) THEN
CBG        CALL BUDGET(SH, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS)
C       NOTE that tmp holds HICE for budget
        CALL BUDGET(ATMFLX, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS, 
     1      IIC, TAIR, TD, PA, UG, TA, OM, FLSE, FLLA, TMP, ALB, A2,
     2      H, A, HSN)
CD        PRINT *,'Atmflx on return from BUDGET ',ATMFLX
       ELSEIF (SURTYP .EQ. 1.) THEN
CBG 24 Feb 1997        CALL ECMBDI(SH, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS)
           PRINT *,'Surface type 1 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSEIF (SURTYP .EQ. 2.) THEN
CBG 24 Feb 1997        CALL EKMAH(SH, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS)
           PRINT *,'Surface type 2 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSE
        PRINT *,'Selected surtyp out of range.  surtyp = ',surtyp
        STOP
      ENDIF
CD      PRINT *,'Returned from ice surface budget'
C  GET MEAN GROWTH RATE OF THICK ICE:
       DO 7 J=1,MM
       DO 7 I=0,L
        RH(I,J)=RH(I,J)+ATMFLX(I,J)/FLOAT(NLEVEL)
    7  CONTINUE
    8 CONTINUE
CD      PRINT *,'Atmflx prior to do 10 ',ATMFLX
      DO 10 J=1,MM
      DO 10 I=0,L
C-----------------------------------------------------------------------
C  DETERMINE THERMODYNAMIC ICE THICKNESS CHANGE FOR CONTINUITY EQUATION
C-----------------------------------------------------------------------
       RA(I,J)=RA(I,J)*OM(I,J)*DT
       RH(I,J)=RH(I,J)*OM(I,J)*DT
       ATMFLX(I,J)=RH(I,J)*A(I,J,LRHS)+(1.0-A(I,J,LRHS))*RA(I,J)
C-----------------------------------------------------------------------
C  ADD SNOWFALL TO SNOW LAYER, OR ADD PRECIPITATION FOR MIXED LAYER
C-----------------------------------------------------------------------
       TMP3(I,J)=0.5*(1.-SIGN(1.,TAIR(I,J)))*A(I,J,LRHS)*SNOFLG
       TMP(I,J)=RPREC(I,J)*DT*OM(I,J)
       PREC(I,J)=(1.-TMP3(I,J))*TMP(I,J)
       HSN(I,J,LNEW)=HSN(I,J,LNEW)+TMP3(I,J)*TMP(I,J)*RHOWAT/RHOSNO
       TMP(I,J)=RH(I,J)*A(I,J,LRHS)
   10 CONTINUE
CD      PRINT *,'Atmflx after to do 10 ',ATMFLX
CD      PRINT *,'Past do 10 in growth'
C-----------------------------------------------------------------------
C  IF ATMFLX BECOMES NEGATIVE, FIRST MELT ANY SNOW THAT IS PRESENT
C-----------------------------------------------------------------------
C     TMP at this point holds concentration-weighted growth rate (m/step).
      CALL VECMIN(TMP,0.,TMP2)
      DO 12 J=1,MM
      DO 12 I=0,L
        TMP2(I,J)=HSN(I,J,LNEW)+TMP2(I,J)*RHOICE/RHOSNO
   12 CONTINUE
C  MAKE SURE WE DO NOT END UP WITH NEGATIVE SNOW THICKNESS:
      CALL VECMAX(TMP2,0.,SN)
      DO 15 J=1,MM
      DO 15 I=0,L
       TMP2(I,J)=HSN(I,J,LNEW)-SN(I,J)
       HSN(I,J,LNEW)=SN(I,J)
C  MODIFY THE ICE MELT AND PRECIPITATION TO ACCOUNT FOR SNOW MELT:
       RH(I,J)=ATMFLX(I,J)+TMP2(I,J)*RHOSNO/RHOICE
       PREC(I,J)=PREC(I,J)+TMP2(I,J)*RHOSNO/RHOWAT
C-----------------------------------------------------------------------
C  COMPUTE CHANGE IN ICE MASS (OR HEAT STORAGE IN OML) DUE TO ATM.FORC.
C-----------------------------------------------------------------------
       QHST(I,J)=H(I,J,LNEW)-(QT(I,J)-tfreez(QS(I,J)) ) *QH(I,J)*CC/CLO
     1  + RH(I,J)
   15 CONTINUE
CD      PRINT *,'Past do 15 in growth'
C-----------------------------------------------------------------------
C  TAKE CARE OF RESIDUAL SNOW TO BE MELTED WHEN ICE HAS DISAPPEARED
C-----------------------------------------------------------------------
      CALL VECMAX(QHST,0.,TMP)
      CALL VECMIN(QHST,0.,TMP3)
      DO 17 J=1,MM
      DO 17 I=0,L
        SN(I,J)=SN(I,J)+TMP3(I,J)*RHOICE/RHOSNO
   17 CONTINUE
C-----------------------------------------------------------------------
C  UPDATE FRESH WATER INPUT, HEAT STORAGE, HEAT FLUX AND FREEZING RATE
C-----------------------------------------------------------------------
      CALL VECMIN(SN,0.,TMP2)
      CALL VECMAX(SN,0.,SN)
      CALL VECMAX(H(0,0,LNEW), 0.,QFM)
      DO 20 J=1,MM
      DO 20 I=0,L
       PREC(I,J)=PREC(I,J)+(HSN(I,J,LNEW)-SN(I,J))*RHOSNO/RHOWAT
       QHST(I,J)=QHST(I,J)+(HSN(I,J,LNEW)-SN(I,J))*RHOSNO/RHOICE
       TMP2(I,J)=TMP2(I,J)*RHOSNO/RHOICE
CBG       QTM(I,J)=( -SN(I,J)*RHOSNO/RHOICE 
       QTM(I,J)=( - TMP2(I,J)
     1            -(QT(I,J)-tfreez(QS(I,J)) ) *QH(I,J)*CC/CLO )
     1     *OM(I,J)
       IF (ABS(QTM(I,J))*CLO/CC .GE. 35.) THEN
         PRINT *,'QTM overload QTM, SN, QT, tfreez, QH ', I, J, 
     1       QTM(I,J)*CLO/CC, TMP2(I,J),
     2       QT(I,J), tfreez(QS(I,J)), QH(I,J)
         IF (QTM(I,J) .GT. 0.) THEN
           QTM(I,J) = 10.*CC/CLO
          ELSE
           QTM(I,J) = -10.*CC/CLO
         ENDIF
       ENDIF
       QFM(I,J)=(QFM(I,J)-TMP(I,J))*OM(I,J)*RHOICE/RHOWAT
       FW(I,J)=-QFM(I,J)
   20 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE OML VARIABLES
C-----------------------------------------------------------------------
C  CALCULATE KINETIC ENERGY FOR MIXED LAYER
CD      PRINT *,'Starting to compute for OML variables'
      DO 1000 J = 1, MM
        DO 1100 I = 1, LM
          QV(I,J) = (0.25* (SQRT(U(I,J,LNEW)**2+V(I,J,LNEW)**2)
     1                     +SQRT(U(I+1,J,LNEW)**2+V(I+1,J,LNEW)**2)
     2                     +SQRT(U(I,J+1,LNEW)**2+V(I,J+1,LNEW)**2)
     3                     +SQRT(U(I+1,J+1,LNEW)**2+V(I+1,J+1,LNEW)**2))
     4               + 0.01 )**3 * OM(I,J)
 1100   CONTINUE
 1000 CONTINUE

CD      PRINT *,'Calling PMLEX'
CBG      CALL PMLEX(QHST, SN, QFM, PREC, QTM, bathy)
      CALL PMLEX(QHST, SN, QFM, PREC, QTM, bathy,
     1  OM, DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, QV, QRHO,
     3  QW, IEN, FW, MLFIX)
C-----------------------------------------------------------------------
C  UPDATE VARIABLES DUE TO OCEANIC HEAT FLUX
C-----------------------------------------------------------------------
      DO 120 J=1,MM
      DO 120 I=0,L
C  UPDATE ICE THICKNESS OR HEAT STORAGE, RESPECTIVELY:
       QHST(I,J)=QHSTO(I,J)+OM(I,J)*(QHST(I,J)-QHSTO(I,J))
C  UPDATE NET FREEZING RATE:
       FW(I,J)=FW(I,J)-QFM(I,J)
C  UPDATE SNOW THICKNESS: BG: THIS is where we need to add dt*prate
       HSN(I,J,LNEW)=SN(I,J)
C  UPDATE THERMODYNAMIC THICKNESS CHANGE OF THIN AND THICK ICE:
       RH(I,J)=-(RH(I,J)-QTM(I,J)*DT/CLO*OM(I,J))
       RA(I,J)=RA(I,J)-QTM(I,J)*DT/CLO*OM(I,J)
  120 CONTINUE
C-----------------------------------------------------------------------
C  UPDATE THERMODYNAMIC ICE THICKNESS CHANGE (EQ.9 IN OWENS & LEMKE 90)
C-----------------------------------------------------------------------
      CALL VECMAX(QHST,0.,H(0,0,LNEW))
C  MAKE SURE WE DON'T TRY TO MELT MORE ICE THAN IS AVAILABLE:
      CALL VECMINC(RH,H(0,0,LNEW), RH)
      DO 125 J=1,MM
      DO 125 I=0,L
        RH(I,J)=-RH(I,J)
  125 CONTINUE
C-----------------------------------------------------------------------
C  UPDATE THERMODYNAMIC CHANGE IN ICE COMPACTNESS (EQ.16 IN HIBLER 79)
C-----------------------------------------------------------------------
C  MAKE SURE WE DO NOT DIVIDE BY 0 IF H=0:
      CALL VECMAX(H(0,0,LRHS), HMIN,TMP3)
C  IF MELTING THICK ICE, THEN EVALUATE THE MELTING TERM:
      CALL VECMIN(RH,0.,TMP2)
C  IF FREEZING THIN ICE, THEN EVALUATE THE FREEZING TERM:
      CALL VECMAX(RA,0.,TMP)
      DO 130 J=1,MM
      DO 130 I=0,L
       RA(I,J)=0.5*TMP2(I,J)*A(I,J,LRHS)/TMP3(I,J)
     1        +TMP(I,J)*(1.-A(I,J,LRHS))/H0
       A(I,J,LNEW)=A(I,J,LNEW)+RA(I,J)
       TMP(I,J)=H(I,J,LNEW)*1.E+06
  130 CONTINUE
C  SET COMPACTNESS TO 0 WHERE THERE IS NO ICE AND TRUNCATE:
      CALL VECMINC(A(0,0,LNEW), TMP,A(0,0,LNEW))
      CALL VECMIN(A(0,0,LNEW), ARMAX,A(0,0,LNEW))
      CALL VECMAX(A(0,0,LNEW), 0.,A(0,0,LNEW))

C-----------------------------------------------------------------------
C   Determine ice draft and snow to ice conversion.  Stossel, 1992 e-mail
C-----------------------------------------------------------------------
      DO 140 J = 1, MM
      DO 140 I = 0, L
        HDRAFT(i,j) = (RHOSNO*HSN(i,j,lnew)+RHOICE*H(i,j,lnew))/rhowat
 140  CONTINUE
      CALL VECMINC(HDRAFT, H(0,0,lnew), TMP2)
      CALL VECMAXC(HDRAFT, H(0,0,lnew), TMP)
      DO 141 J = 1, MM
      DO 141 I = 0, L
        HSN(i,j,lnew) = HSN(i,j,lnew) - 
     1                 (HDRAFT(i,j) - TMP2(i,j))*RHOICE/RHOSNO
        H(i,j,lnew) = TMP(i,j)
 141  CONTINUE

C-----------------------------------------------------------------------
C   Re-set ATMFLX from meters of ice per delta t to a heat flux
C     Robert Grumbine 28 February 1997.
C-----------------------------------------------------------------------
      DO 150 J = 0, M
      DO 150 I = 0, L
        ATMFLX(I,J) = ATMFLX(I,J)/DT * CLO
  150 CONTINUE

      RETURN
      END
      SUBROUTINE OBUDGET(FH, QT, LWDN, SWDN, TAIR, TD, PA, UG, TA, 
     1  OM, FLSE, FLLA)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1989
C  PURPOSE:
C     -CALCULATES GROWTH RATES OF NEW ICE IN THE ICE FREE PART OF A GRID
C       CELL WITH BULK FORMULAS
C  METHOD:
C     -HEAT BUDGET EQUATION FOR OPEN WATER
C  INTERFACE:
C     -FH: GROWTH RATE IN METERS OF ICE
C     -QT: SEA SURFACE=OML TEMPERATURE IN CELSIUS
C  EXTERNALS:
C     -VAPOR: CALCULATES VAPOR PRESSURE
C  LAST MODIFIED: 13 September 1994.
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      REAL TAIR(0:L,0:M), TD(0:L,0:M), PA(0:L,0:M), UG(0:L,0:M), 
     1  TA(0:L,0:M)

      REAL OM(0:L, 0:M)

      REAL FLSE(0:L, 0:M), FLLA(0:L, 0:M)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C=======================================================================
      REAL FH(0:L,0:M), QT(0:L,0:M), ESTA(LMDP), ESTW(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -QT:   SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW: SATURATION VAPOR PRESSURE OVER WATER
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1  FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), QT1(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL albedo
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      INTEGER I, J, K, N
      REAL D1, D2W, EA, Q1, Q2, Q3
      LOGICAL overload
C-----------------------------------------------------------------------
CD      PRINT *,'Entered obudget'
      IF (overload(LWDN, LP, MP, 10000.)) THEN
        PRINT *,'LWDN overloaded on obudget, 10,000 cutoff'
      ENDIF
C-----------------------------------------------------------------------
C  SELECT THE GRID CELLS AND STORE THEM IN SELECTIVE 1-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
CD      PRINT *,'obudget entering do 93'
      DO 93 J=1,MM
      DO 93 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 93
       K=K+1
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J) * (1. - albedo(0., 0., 0., 0.) )
   93 CONTINUE
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      D1=RHOAIR*CPAIR*CSENS
      D2W=RHOAIR*VAPL*CLAT
C-----------------------------------------------------------------------
C  CALCULATE HEAT FLUXES AND GROWTH RATES
C-----------------------------------------------------------------------
CD      PRINT *,'obudget entering do 25'
      DO 25 N=1,K
       EA=TD1(N)*ESTA(N)
       FLSE1(N)=D1*UG1(N)*(TA1(N)-QT1(N))
       FLLA1(N)=D2W*UG1(N)*(EA-ESTW(N))*EPSI/PA1(N)
       Q1=D3*QT1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       FH1(N)=(Q1-Q2-Q3-FLSE1(N)-FLLA1(N))/CLB
   25 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
CD      PRINT *,'obudget entering do 81'
      DO 81 J=1,MM
      DO 81 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 81
       K=K+1
       FH(I,J)=FH1(K)
       TA(I,J)=TA1(K)
       FLSE(I,J)=FLSE1(K)
       FLLA(I,J)=FLLA1(K)
       IF ( ABS(FH(I,J))*CLO .GT. 1000.) THEN !W/m2
         WRITE (*,9001) I, J, FH(I,J)*CLO, QT1(K), TA(I,J), -FLSE(I,J), 
     1     -FLLA(I,J), -SWDN(I,J), -LWDN(I,J), 
     2     D3*QT1(K)**4 - SWDN(I,J) - LWDN(I,J)
       ENDIF
   81 CONTINUE

CD      PRINT *,'leaving 84, obudget'

 9001 FORMAT (2I4, 8F9.1, 
     1     ' Ocean, Net, TO, TA, FLSE, FLLA, SWD, LWD, Net rad ')

      RETURN
      END
      SUBROUTINE PMLEX(QHST,SNOW,QFM,QPR,QTM, bathy,
     1  OM, DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, QV,
     3  QRHO, QW, IEN, FW, MLFIX)
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C     -R. Grumbine            NMC, Camp Springs                   Jul 95
C  PURPOSE:
C     -PROGNOSTIC CALCULATION OF OCEANIC MIXED LAYER VARIABLES (OML-
C       MODEL) AND VERTICAL OCEANIC HEAT FLUX
C  METHOD:
C     -HEAT AND SALT BUDGET OF WATER COLUMN
C     -KRAUS-TURNER TYPE PARAMETERIZATION FOR ENTRAINMENT HEAT FLUX WITH
C       EXPONENTIALLY SHAPED PYCNOCLINE
C     -ENERGY BALANCE FOR CLOSURE
C     -Limit mixed layer depth to the bathymetry.  BG.
C  INTERFACE:
C     -QHST: CHANGE IN ICE THICKNESS OR HEAT STORAGE (IF NEGATIVE)
C     -SNOW: CURRENT SNOW DEPTH
C     -QFM:  AMOUNT OF MELTED ICE [M] OR SURFACE SALT FLUX
C     -QPR:  FRESH WATER INPUT (MELTED SNOW OR ICE + RAIN)
C     -QTM:  VERTICAL OCEANIC (ENTRAINMENT) HEAT FLUX
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMIN:  THE THIRD ARGUMENT IS MINIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMAXC: SAME AS VECMAX, THE SECOND ARGUMENT BEING AN ARRAY
C     -VECMINC: SAME AS VECMIN, THE SECOND ARGUMENT BEING AN ARRAY
C     -VERDIF:  DETERMINES VERTICAL DIFFUSION
C     -ADJUEX:  ADJUSTS OML VARIABLES
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "oml.inc"
      INCLUDE "physical.inc"
C=======================================================================
      REAL DELTAD, SSMIN, TTMIN, HSTC
      PARAMETER (DELTAD=8.0)
      PARAMETER (SSMIN=0.01)
      PARAMETER (TTMIN=0.8)
      PARAMETER (HSTC=120.0)
C=======================================================================
      REAL bathy(0:L, 0:M)

CD      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
CD     1QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M), 
CD     2QHSTO(0:L,0:M),  HS(0:L,0:M),  HT(0:L,0:M),  QV(0:L,0:M),
CD     3QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M), QHSTO(0:L,0:M)
      REAL HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

      REAL OM(0:L, 0:M)
      REAL DCVM, WUP, COSGAM, RTC, STC, QTOC

C=======================================================================
      REAL TMP(0:L,0:M), TMP2(0:L,0:M), TMP3(0:L,0:M), TMP4(0:L,0:M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C     -TMP4: TEMPORARY ARRAY
C=======================================================================
      REAL DCV(0:L,0:M), QWT(0:L,0:M), RET(0:L,0:M), ENT(0:L,0:M),
     1HOLD(0:L,0:M), QSS(0:L,0:M), FLAGI(0:L,0:M)
C=======================================================================
C     -DCV:   DISSIPATION OF CONVECTIVE ENERGY
C     -QWT:   KINETIC ENERGY * DISSIPATION OF MECHANICAL ENERGY
C     -RET:   AMOUNT OF MIXED LAYER RETREAT
C     -ENT:   ENTRAINMENT VELOCITY
C     -HOLD:  OLD MIXED LAYER DEPTH
C     -QSS:   ADJUSTED MIXED LAYER SALINITY DUE TO ADVECTIVE EFFECTS
C     -FLAGI: FLAG FIELD FOR PRESENCE OF ICE AND/OR SNOW
C=======================================================================
      REAL QHST(0:L,0:M), SNOW(0:L,0:M), QFM(0:L,0:M), QPR(0:L,0:M),
     1QTM(0:L,0:M)
C=======================================================================
C      Local declarations
      INTEGER I, J
      REAL flag, flag1, flag2, flagto, flags, flagtn, flagt
      REAL a1, fnot, f1, ss, tt, ads, adt, sss, ttt, deta, hv
      REAL dh1, shb, thb, stab, sstar, tfreez
      INTEGER iter
      LOGICAL overload
C-----------------------------------------------------------------------
C  ARTIFICIAL RESTRICTIONS FOR OML SIMULATION -- MOVED TO PARAMETERS
C-----------------------------------------------------------------------
C  TURBULENT LENGTH SCALE:
C      DELTAD=8.0
C  PYCNOCLINE LIMITATIONS FOR NEAR NEUTRAL STRATIFICATIONS:
C      SSMIN=0.01
C      TTMIN=0.8
C  MODIFICATION OF TIME CONSTANT FOR NEWTONIAN ADJUSTMENT OF SALINITY:
C      HSTC=120.0
C-----------------------------------------------------------------------
C  DETERMINE INITIAL CONDITIONS AND UPDATE HEAT AND SALT CONTENT
C-----------------------------------------------------------------------
CD      PRINT *,'before do 10 loop '
CD      PRINT *,'bathymetry ',bathy
      DO 10 J=1,MM
      DO 10 I=0,L
       QFM(I,J)=-QFM(I,J)*(QS(I,J)-SICE)-QPR(I,J)*QS(I,J)
       QFM(I,J)=QFM(I,J)*OM(I,J)
       QTM(I,J)= QTM(I,J)*CLO/CC*OM(I,J)
       QRHO(I,J) = (BETAS*QFM(I,J)-BETAT*QTM(I,J) 
     1              + gammat*QTM(I,J)**2)/DT
       QW(I,J)=QV(I,J)*CW*COSGAM
       QWT(I,J)=QW(I,J)*EXP(-QH(I,J)/QHW)*OM(I,J)
       QHSTO(I,J)=QHST(I,J)
       HOLD(I,J)=QH(I,J)
       TMP(I,J)=EXP(-QH(I,J)/QHS)
       TMP4(I,J)=SNOW(I,J)
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
C  PARAMETERIZE ADVECTIVE EFFECTS:
       QFM(I,J)=QFM(I,J)-(QS(I,J)-33.8)*HSTC*OM(I,J)/STC
       HS(I,J)=HS(I,J)+(QFM(I,J)+WUP*DT*(QSB(I,J)-QS(I,J)))*OM(I,J)
       HT(I,J)=HT(I,J)+(QTM(I,J)+WUP*DT*(QTB(I,J)-QT(I,J)))*OM(I,J)
   10 CONTINUE

      IF (overload(QHSTO, lp, mp, 350.)) PRINT *,
     1                             'Overload at 10 in QHSTO'
      IF (overload(HS, lp, mp, 40.*7000.)) THEN
        PRINT *, 'Overload at 10 in HS'
        PRINT *, ' QFM ', QFM
        PRINT *, ' QSB ', QSB
        PRINT *, ' QS ', QS
        PRINT *, ' HS ', HS
      ENDIF
      IF (overload(HT, lp, mp, 40.*7000.)) THEN
        PRINT *, 'Overload at 10 in HT'
        PRINT *, ' QFM ', QFM
        PRINT *, ' QTB ', QTB
        PRINT *, ' QT ', QT
        PRINT *, ' HT ', HT
      ENDIF
      IF (overload(QFM, lp, mp, 350.)) PRINT *,
     1                             'Overload at 10 in QFM'
      IF (overload(QTM, lp, mp, 35.)) PRINT *,
     1             'Overload at 10 in QTM',QTM
      IF (overload(QT, lp, mp, 40.)) PRINT *,
     1             'Overload at 10 in QT', QT
      IF (overload(QS, lp, mp, 40.)) PRINT *,
     1                             'Overload at 10 in QS'


      IF (MLFIX.NE.1) GOTO 30
C-----------------------------------------------------------------------
C  THE NEXT LOOP ENTERS ONLY FOR FIXED MIXED LAYER DEPTH
C-----------------------------------------------------------------------
CD      PRINT *, 'In do 20 loop'
C     Following added by Bob Grumbine to avoid infinitely thin
C       mixed layers.  10 October 1995
      CALL VECMAX (QH, 1.0, QH)
      DO 20 J=1,MM
      DO 20 I=0,L
       IEN(I,J)=1
       HT(I,J)=HT(I,J)+QTOC/CC*DT*OM(I,J)
       QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/QH(I,J)+QTB(I,J)
       QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/QH(I,J)+QSB(I,J)
       QTM(I,J)=QTOC
   20 CONTINUE
      IF (overload(QHSTO, lp, mp, 350.)) PRINT *,
     1                                 'Overload at 20 in QHSTO'
      IF (overload(HS, lp, mp, 40.*7000.)) 
     1                   PRINT *,'Overload at 20 in HS'
      IF (overload(HT, lp, mp, 40.*7000.)) 
     1                   PRINT *,'Overload at 20 in HT'
      IF (overload(QFM, lp, mp, 350.)) PRINT *,'Overload at 20 in QFM'
      IF (overload(QTM, lp, mp, 35.)) PRINT *,'Overload at 20 in QTM'
      IF (overload(QT, lp, mp, 40.)) PRINT *,'Overload at 20 in QT'
      IF (overload(QS, lp, mp, 40.)) PRINT *,'Overload at 20 in QS'

      GOTO 200
C-----------------------------------------------------------------------
C  DETERMINE SIGN OF ENTRAINMENT
C-----------------------------------------------------------------------
   30 CONTINUE
      CALL VECMAX(TMP,DCVM,DCV)
CD      PRINT *,'In do 40 loop'
      DO 40 J=1,MM
      DO 40 I=0,L
       FLAG=(1.+SIGN(1.,QRHO(I,J)))/2.
       DCV(I,J)=1.-FLAG +DCV(I,J)*FLAG
       ENT(I,J)=2.*QWT(I,J)+GRAV*QRHO(I,J)*DCV(I,J)*QH(I,J)
       IEN(I,J)=INT((1.+SIGN(1.,ENT(I,J)))/2.0+0.2)
       TMP(I,J)=QH(I,J)*0.7
   40 CONTINUE
C-----------------------------------------------------------------------
C  DETERMINE MIXED LAYER RETREAT USING THE MONIN-OBUKHOV LENGTH
C-----------------------------------------------------------------------
      CALL VECMIN(TMP,100.0,TMP)
CD      PRINT *,'In do 50 loop'
      DO 50 ITER=1,5
      DO 50 J=1,MM
      DO 50 I=0,L
       FLAG=1.0-FLOAT(IEN(I,J))
       A1=EXP(-TMP(I,J)/QHW)
       FNOT=2.*QW(I,J)*A1+GRAV*QRHO(I,J)*TMP(I,J)
       F1=-2.*QW(I,J)*A1/QHW+GRAV*QRHO(I,J)
       TMP(I,J)=TMP(I,J)-FNOT*FLAG/(F1*FLAG+(1.-FLAG))
   50 CONTINUE
      CALL VECMAX(TMP,10.,TMP2)
      CALL VECMINC(TMP2,QH,TMP3)
CD      PRINT *,'In do 70 loop'
      DO 70 J=1,MM
      DO 70 I=0,L
       RET(I,J)=(TMP3(I,J)-QH(I,J))*(1.-FLOAT(IEN(I,J)))/RTC
   70 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE ENTRAINMENT VELOCITY
C-----------------------------------------------------------------------
CD      PRINT *,'In do 100 loop'
      DO 100 J=1,MM
      DO 100 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
       IF (SS .EQ. 0.) SS = 1.E-3
       IF (TT .EQ. 0.) TT = 1.E-3

       ADS=1.0+(EXP(-DELTAD/QDS(I,J))-1.0)*QDS(I,J)/DELTAD
       ADT=1.0+(EXP(-DELTAD/QDT(I,J))-1.0)*QDT(I,J)/DELTAD
       SSS=SS*ADS
       TTT=TT*ADT
       DETA = GRAV*QH(I,J)*(BETAS*SSS-BETAT*TTT+GAMMAT*TTT*TTT)
       TMP(I,J) = DETA-GRAV*QH(I,J)*TTT*DCV(I,J)
     1   *(BETAT-gammat*TTT
     1          -CC/CLO*BETAS*RHOICE/RHOWAT*(QS(I,J)-SICE))*FLAGI(I,J)
  100 CONTINUE
      CALL VECMAX(TMP,EPSAA,TMP2)
      CALL VECMAX(ENT,0.,TMP)
CD      PRINT *,'In do 110 loop'
      DO 110 J=1,MM
      DO 110 I=0,L
       ENT(I,J)=TMP(I,J)/TMP2(I,J)*DT*FLOAT(IEN(I,J))
  110 CONTINUE
      CALL VECMIN(ENT,ENTMAX,TMP)
C-----------------------------------------------------------------------
C  DETERMINE NEW MIXED LAYER DEPTH
C-----------------------------------------------------------------------
CD      PRINT *,'In do 120 loop'
      DO 120 J=1,MM
      DO 120 I=0,L
       TMP2(I,J)=QH(I,J)-(WUP*DT-TMP(I,J)*FLOAT(IEN(I,J))-
     1           RET(I,J)*(1.0-FLOAT(IEN(I,J))))*OM(I,J)
       TMP3(I,J)= bathy(i,j)
CD Note that the following used to be inside the loop.  That
CD   hardly seems a good idea since QH is being trimmed to fit
CD   physical constraints.  Assign after bounds have been
CD   placed.  BG 10 October 1995
CD       TMP(I,J)=QH(I,J)
  120 CONTINUE
      CALL VECMINC(TMP2,TMP3,QH)
C     Following added by Bob Grumbine to avoid infinitely thin
C       mixed layers.  10 October 1995
      CALL VECMAX (QH, 1.0, QH)
      DO 121 J = 1, MM
      DO 121 I = 0, L
        TMP(i,j) = QH(i,j)
  121 CONTINUE
 
C-----------------------------------------------------------------------
C  DETERMINE MIXED LAYER AND PYCNOCLINE VARIABLES
C-----------------------------------------------------------------------
CD      PRINT *,'In do 140 loop'
      DO 140 J=1,MM
      DO 140 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
CBG    Following added for robustness
       IF (SS .EQ. 0.) SS = 1.E-3
       IF (TT .EQ. 0.) TT = 1.E-3
       IF ( ABS(QDS(I,J)) .LT. 1.E-2) THEN
         PRINT *,'qds ',i,j,qds(i,j)
         QDS(I,J) = SIGN(1.E-2, QDS(I,J) )
       ENDIF
       IF ( ABS(QDT(I,J)) .LT. 1.E-2) THEN
         PRINT *,'qds ',i,j,qdt(I,J)
         QDT(I,J) = SIGN(1.E-2, QDT(I,J) )
       ENDIF

       FLAGTO=(1.+SIGN(1.,ABS(TT)-TTMIN))/2.*SIGN(1.,-TT)
       ADS=1.0+(EXP(-DELTAD/QDS(I,J))-1.0)*QDS(I,J)/DELTAD
       ADT=1.0+(EXP(-DELTAD/QDT(I,J))-1.0)*QDT(I,J)/DELTAD
       SSS=SS*ADS
       TTT=TT*ADT
       HV=TMP(I,J)
       ENT(I,J)=(QH(I,J)-HV+WUP*DT)*FLOAT(IEN(I,J))
       DH1=QH(I,J)-HV
       HV=2.0*QH(I,J)*HV/(QH(I,J)+HV)
       IF (HV .EQ. 0) STOP "HV = 0"

       SHB=QSB(I,J)*QHB(I,J)
       THB=QTB(I,J)*QHB(I,J)
       QS(I,J)=QS(I,J)+(QFM(I,J)/HV+SSS*ENT(I,J)/HV)*OM(I,J)
       SS=QSB(I,J)-QS(I,J)
       FLAGS=(1.+SIGN(1.,ABS(SS)-SSMIN))/2.
       SS=SS*FLAGS+1.0-FLAGS
       IF (SS .EQ. 0) STOP "SS = 0"
       QDS(I,J)=QDS(I,J)-(QDS(I,J)-(SHB-HS(I,J))/SS+QH(I,J))*FLAGS
     1          *OM(I,J)
       QDS(I,J)=QDS(I,J)-DH1*(1.0-FLAGS)*OM(I,J)
CD       PRINT *,'figuring qs( ',i,j
C      BG addition for robustness
       IF (ABS(QH(I,J) + QDS(I,J)) .LT. 1.E-3) THEN
         PRINT *,'resetting QDS to avoid division by zero'
         QDS(I,J) = QH(I,J) - 1.0
       ENDIF
       QS(I,J)=QS(I,J)-(QS(I,J)-(HS(I,J)-SHB)/(QH(I,J)+QDS(I,J))-
     1         QSB(I,J))*(1.0-FLAGS)*OM(I,J)
       QT(I,J)=QT(I,J)+(QTM(I,J)/HV+TTT*ENT(I,J)/HV)*OM(I,J)
       TT=QTB(I,J)-QT(I,J)
       FLAGTN=(1.+SIGN(1.,ABS(TT)-TTMIN))/2.*SIGN(1.,-TT)
       FLAGT=FLAGTO*FLAGTN*(1.+FLAGTO*FLAGTN)/2.
       TT=TT*FLAGT+1.0-FLAGT
       IF (TT .EQ. 0) STOP "TT = 0"
       QDT(I,J)=QDT(I,J)-(QDT(I,J)-(THB-HT(I,J))/TT+QH(I,J))*FLAGT
     1          *OM(I,J)
       QDT(I,J)=QDT(I,J)-DH1*(1.0-FLAGT)*OM(I,J)
CD       PRINT *,'figuring qt( ',i,j
C      BG addition for robustness
       IF (ABS(QH(I,J) + QDT(I,J)) .LT. 1.E-3) THEN
         PRINT *,'resetting QDT to avoid division by zero'
         QDT(I,J) = QH(I,J) - 1.0
       ENDIF
       QT(I,J)=QT(I,J)-(QT(I,J)-(HT(I,J)-THB)/(QH(I,J)+QDT(I,J))-
     1         QTB(I,J))*(1.0-FLAGT)*OM(I,J)
       QTM(I,J)=TTT*ENT(I,J)*FLAGT*OM(I,J)
  140 CONTINUE
      IF (overload(QHSTO, lp, mp, 350.)) PRINT *,
     1                             'Overload at 140 in QHSTO'
      IF (overload(HS, lp, mp, 40.*7000.)) PRINT *,
     1                             'Overload at 140 in HS'
      IF (overload(HT, lp, mp, 40.*7000.)) PRINT *,
     1                             'Overload at 140 in HT'
      IF (overload(QFM, lp, mp, 350.)) PRINT *,
     1                             'Overload at 140 in QFM'
      IF (overload(QTM, lp, mp, 35.)) PRINT *,
     1                             'Overload at 140 in QTM'
      IF (overload(QT, lp, mp, 40.)) PRINT *,
     1                             'Overload at 140 in QT'
      IF (overload(QS, lp, mp, 40.)) PRINT *,
     1                             'Overload at 140 in QS'

CD      PRINT *,'escaped do 140'
C-----------------------------------------------------------------------
C  SIMULATE VERTICAL DIFFUSION
C-----------------------------------------------------------------------
      CALL VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)
C-----------------------------------------------------------------------
C  ADJUST MIXED LAYER VARIABLES AFTER THE PREVIOUS MODIFICATIONS
C-----------------------------------------------------------------------
  200 CONTINUE
      CALL ADJUEX(QHST,SNOW,FLAGI, OM, TMP, TMP2, TMP3,
     1  QS, QSB, QT, QTB, QH, QHB, QDS, QDT, HS, HT, MLFIX)
C-----------------------------------------------------------------------
C  STABILITY ADJUSTMENT (CONVECTION)
C-----------------------------------------------------------------------
      IF (MLFIX.EQ.1) GO TO 300
CD      PRINT *,'In do 270 loop'
      DO 270 J=1,MM
      DO 270 I=0,L
       STAB = BETAS*(QSB(I,J)-QS(I,J))-BETAT*(QTB(I,J)-QT(I,J))
     1       +GAMMAT*(QTB(I,J)**2 - QT(I,J)**2)
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
       FLAGS=(1.0-SIGN(1.,STAB-EPSAA))/2.
       TMP(I,J)=QH(I,J)
       TMP3(I,J)= bathy(i,j) +10.0
C  PARTIAL OVERTURNING IN ICE COVERED OCEAN:
       SSTAR = QSB(I,J)-BETAT/BETAS*(QTB(I,J)-QT(I,J))-0.01
     1       + GAMMAT*(QTB(I,J)**2 - QT(I,J)**2) /BETAS
       QH(I,J) = QH(I,J)+(QS(I,J)-SSTAR)*(QH(I,J)+QDS(I,J)-QHSTO(I,J))
     1         / AMAX1(0.001, (QTB(I,J)-tfreez(QS(I,J)) ) ) 
     2         / (CC/CLO*(SSTAR-SICE)-BETAT/BETAS)
     2         * FLAGI(I,J)*OM(I,J)*FLAGS
C  OVERTURNING IN OPEN OCEAN:
       QH(I,J)=QH(I,J)+(QH(I,J)-TMP3(I,J))*(1.-FLAGI(I,J))*FLAGS*OM(I,J)
       QS(I,J)=QS(I,J)-(QS(I,J)-SSTAR)*(1.-FLAGI(I,J))*FLAGS*OM(I,J)
       HS(I,J)=HS(I,J)-(HS(I,J)-(SSTAR-QSB(I,J))*(QH(I,J)+QDS(I,J))
     1         -QSB(I,J)*QHB(I,J))*(1.0-FLAGI(I,J))*FLAGS*OM(I,J)
  270 CONTINUE
      IF (overload(QHSTO, lp, mp, 350.)) PRINT *,
     1                                'Overload at 270 in QHSTO'
      IF (overload(HS, lp, mp, 40.*7000.)) PRINT *,
     1                                'Overload at 270 in HS'
      IF (overload(HT, lp, mp, 40.*7000.)) PRINT *,
     1                                'Overload at 270 in HT'
      IF (overload(QFM, lp, mp, 350.)) PRINT *,
     1                                'Overload at 270 in QFM'
      IF (overload(QTM, lp, mp, 35.)) PRINT *,
     1                                'Overload at 270 in QTM'
      IF (overload(QT, lp, mp, 40.)) PRINT *,
     1                                'Overload at 270 in QT'
      IF (overload(QS, lp, mp, 40.)) PRINT *,
     1                                'Overload at 270 in QS'

      CALL VECMAXC(QH,TMP,TMP2)
      CALL VECMINC(TMP2,TMP3,QH)
      CALL VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)
      DO 280 J=1,MM
      DO 280 I=0,L
C      'then' clause added by Bob Grumbine 9 July 1995
C      This handles the case where the mixed layer is bottoming out.
CD       PRINT *,'pmlex 280 pt ',i,j
       IF (QH(I,J) -  bathy(i,j)  .GT. -0.1 ) THEN
CD         IF (bathy(i,j) .GT. 1.0 )) PRINT *,'applying hml fix ',i,j
         TMP2(i,j) = QT(i,j)
         QT(I,J)  = HT(i,j) /  bathy(i,j)  - 1.0 
         QH(I,J) = 2.5
         QS(I,J)  = HS(i,j) /  bathy(i,j) * 0.99 
         QTB(I,J) = HT(i,j) /  bathy(i,j) 
         QSB(I,J) = HS(i,j) /  bathy(i,j) 
         IF (QT(i,j) .LT. tfreez(QS(i,j)) ) THEN
           QT(i,j)  = tfreez(QS(I,j))
           HT(i,j)  = QT(I,j)* QH(i,j) 
         ENDIF 
         IF (QTB(i,j) .LT. tfreez(QSB(i,j)) ) THEN
           QTB(i,j) = tfreez(QSB(I,j))
         ENDIF 
        ELSE       
         TMP2(I,J)=QT(I,J)
         QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/(QH(I,J)+QDT(I,J))+QTB(I,J)
         QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/(QH(I,J)+QDS(I,J))+QSB(I,J)
       ENDIF
       QTM(I,J)=QTM(I,J)+(QT(I,J)-TMP2(I,J))*QH(I,J)
       QTM(I,J)=QTM(I,J)*CC/DT*OM(I,J)
  280 CONTINUE

      CALL ADJUEX(QHST,SNOW,FLAGI, OM, TMP, TMP2, TMP3,
     1  QS, QSB, QT, QTB, QH, QHB, QDS, QDT, HS, HT, MLFIX)

C-----------------------------------------------------------------------
C  FINAL OUTPUT FOR BOTH FIXED AND VARIABLE MIXED LAYER
C-----------------------------------------------------------------------
  300 CONTINUE 
      CALL VECMAX(QHSTO,0.,TMP)
      CALL VECMAX(QHST,0.,TMP2)
      DO 310 J=1,MM
      DO 310 I=0,L
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
       FLAGI(I,J)=1.0-FLAGI(I,J)
       QHST(I,J)=QHST(I,J)-(QHST(I,J)+(QT(I,J)-tfreez(QS(I,J)) )
     1           *QH(I,J)*CC/CLO)* FLAGI(I,J)
C  ON OUTPUT: QTM IS ENTRAINMENT HEAT FLUX IN [W/M**2]
       QTM(I,J)=QTM(I,J)*(1.-FLAGI(I,J))*FLOAT(IEN(I,J))
C  ON OUTPUT: QFM = MELTING RATE DUE TO OCEANIC HEAT FLUX IN [M]
       QFM(I,J)=(TMP(I,J)-TMP2(I,J))*OM(I,J)*RHOICE/RHOWAT
       IEN(I,J)=INT(1.0-FLAGI(I,J)+0.3)
  310 CONTINUE
C  PARAMETERIZE ADVECTIVE EFFECTS:
      CALL VECMINC(QDS, bathy ,QDS)
      CALL VECMINC(QDT, bathy ,QDT)
      CALL VECMINC(QH, bathy ,QH)
C  Note: Still apply minimum of HMLREF on ocean grounds, rather than
C    bathymetry grounds.  Definitely need a column ocean rather than
C    present slab ocean.  Bob Grumbine 16 September 1995.
      CALL VECMIN(QDS,HMLREF,QDS)
      CALL VECMIN(QDT,HMLREF,QDT)
      CALL VECMIN(QH,HMLREF,QH)
C  And now require that QDS, QDT be at least 1 meter 10 October 1995
      CALL VECMAX(QDT, 1.0, QDT)
      CALL VECMAX(QDS, 1.0, QDS)

      DO 401 J=1,MM
      DO 401 I=0,L
        QSS(I,J)=QSB(I,J)-(BETAT/BETAS)*(QTB(I,J)-QT(I,J))-.01
     1       + GAMMAT*(QTB(I,J)**2 - QT(I,J)**2) /BETAS
  401 CONTINUE
      CALL VECMINC(QS,QSS,QS)
      DO 400 J=1,MM
      DO 400 I=0,L
       HS(I,J)=(QS(I,J)-QSB(I,J))*(QH(I,J)+QDS(I,J))+QSB(I,J)*QHB(I,J)
       HT(I,J)=(QT(I,J)-QTB(I,J))*(QH(I,J)+QDT(I,J))+QTB(I,J)*QHB(I,J)
  400 CONTINUE

CBG   Clean up after singular variables which seem common in this
CBG     method
      DO 500 J = 1, MM
      DO 500 I = 0, L
        IF (ABS(QHST(I,J)) .GT. 350.  ) THEN
          PRINT *,'qhst, qfm ',i,j,QHST(I,j), QFM(I,J)
          QHST(I,J) = 0.0
        ENDIF
        IF (ABS(QT(I,J)) .GT. 350.  ) THEN
          PRINT *,'QT, QS ',i,j,QT(I,j)
          QT(I,J) = 0.0
        ENDIF
        IF (ABS(QS(I,J)) .GT. 350.  ) THEN
          PRINT *,'QS, QS ',i,j,QS(I,j)
          QS(I,J) = 0.0
        ENDIF
  500 CONTINUE

      RETURN
      END
      SUBROUTINE VAPOR(T,EST,K1,K)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     C.KOCH                  UNI, BONN                             1986
C  MODIFIED BY:
C     A.STOESSEL              MPI, HAMBURG                          1989
C     Robert Grumbine         NCEP, Camp Springs MD            June 1993
C  PURPOSE:
C     -CALCULATION OF SATURATION VAPOR PRESSURE FOR AIR TEMPERATURE
C       (K1=1), OVER ICE (K1=2) AND OVER WATER (K1=3)
C  INTERFACE:
C     -T:   TEMPERATURE OF ATMOSPHERE, ICE OR OCEAN
C     -EST: SATURATION VAPOR PRESSURE
C     -K1:  INDEX FOR CHOICE OF QUANTITY TO BE CALCULATED
C     -K:   INDEX FOR SELECTIVE LOOP
C  LAST MODIFIED: 7 June 1993.
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL T(LMDP),EST(LMDP)
      INTEGER K, K1
      INTEGER N
      INTEGER AIR, ICE, WATER
      PARAMETER (AIR = 1)
      PARAMETER (ICE = 2)
      PARAMETER (WATER = 3)
C=======================================================================

      IF (K1 .EQ. AIR) THEN
      DO 11 N=1,K
        EST(N)=611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),200.)-273.15+257.87))
   11 CONTINUE

       ELSE IF (K1 .EQ. ICE) THEN
      DO 22 N=1,K
        EST(N)=611.15*EXP((23.036-(MIN(T(N),273.15)-273.15)/333.7)*
     1        (MIN(T(N),273.15)-273.15)/(MAX(T(N),200.)-273.15+279.82))
   22 CONTINUE

       ELSE IF (K1 .EQ. WATER) THEN
    3 CONTINUE
      DO 33 N=1,K
        EST(N)=0.9815*611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),260.)-273.15+257.87))
   33 CONTINUE

      ENDIF

      RETURN
      END
      SUBROUTINE VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C  PURPOSE:
C     -SIMULATION OF VERTICAL DIFFUSION
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C  LAST MODIFIED: 5 January 1993.
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
C=======================================================================

        REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2 HS(0:L,0:M), HT(0:L,0:M)

C=======================================================================
      INTEGER I, J
C=======================================================================
      CALL VECMAX(QDS,5.,QDS)
      CALL VECMAX(QDT,5.,QDT)
      DO 150 J=1,MM
      DO 150 I=0,L
       QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/(QH(I,J)+QDS(I,J))+QSB(I,J)
       QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/(QH(I,J)+QDT(I,J))+QTB(I,J)
  150 CONTINUE
      RETURN
      END
