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
      REAL ETA(L,M), ZETA(L,M), ASY(L,M), E11(L,M), E12(L,M), E22(L,M)

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
      PRINT *,'hsn before bcsh zzzzz',HSN
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

CBG       PRINT *, "after forfld"
C-----------------------------------------------------------------------
C      IF there is no ice cover, skip to the thermodynamics
C-----------------------------------------------------------------------
       IF (noice(A, LP, MP, 2, LOLD)) GO TO 9999
CBG       PRINT *, "back from noice"

C-----------------------------------------------------------------------
C  CALCULATE ICE PRESSURE WITH OLD VALUES OF ICE THICKNESSES
C-----------------------------------------------------------------------
CD       PRINT *,'Calling pressb'
CBG       PRINT *, "would call pressb"
CBG      Insert a jump to skip all dynamics as thermo is the problem
CDEBUG GO TO 9999
       CALL PRESSB(LOLD, P, H, A)
CBG       PRINT *, "after pressb"
C-----------------------------------------------------------------------
C  NOW DO VELOCITY INTEGRATION
C**REMARK: IF FIRST TIME, THEN DO INITIAL RELAXATION TO ADJUST VELOCI-
C    TIES TO VISCOUS STRESSES AND FORCING FIELDS.
C-----------------------------------------------------------------------
       IF (T.EQ.DT.AND.NRREC.EQ.0)THEN
CD        PRINT *,'Calling initrel'
CD        CALL INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, DT,
        CALL INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, ASY, 
     1               SURTYP, SURFWIN, IIC)
       END IF

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
CBG       PRINT *, "after plast"
C-----------------------------------------------------------------------
C  CALCULATE THOSE PARTS OF THE EQUATIONS THAT DO NOT DEPEND ON THE NEW
C    VELOCITY, AND STORE THEM IN THE TEMPORARY ARRAYS
C-----------------------------------------------------------------------
CD        PRINT *,'Calling relcon'
        CALL RELCON(LOLD, LRHS, U, V, BU, BV, AMAS, FX, FY, H, 
     1              ETA, ZETA, ASY, SURTYP, SURFWIN)
CBG       PRINT *, "after relcon"
C-----------------------------------------------------------------------
C  NOW SOLVE FOR THE NEW VELOCITIES, USING OVER-RELAXATION
C-----------------------------------------------------------------------
        CALL RELAX(LRHS, LLHS, U, V, BU, BV, AMAS, FX, FY, H, 
     1             ETA, ZETA, ASY, IIC)
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
        CALL SADVECT(RH, H, U, V, PM, PN, LRHS, LNEW)
        CALL SADVECT(RA, A, U, V, PM, PN, LRHS, LNEW)
        CALL SADVECT(RSN, HSN, U, V, PM, PN, LRHS, LNEW)
C-----------------------------------------------------------------------
C  ADD IN HORIZONTAL DIFFUSION
C-----------------------------------------------------------------------
        IF (JJC.EQ.2) THEN
         CALL SDIFFUS(RH, H, LOLD)
         CALL SDIFFUS(RA, A, LOLD)
         CALL SDIFFUS(RSN, HSN, LOLD)
        END IF
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
 9999  CONTINUE
C-----------------------------------------------------------------------
C  KINETIC ENERGY FOR MIXED LAYER, Moved to GROWTH
C-----------------------------------------------------------------------
C  NOW DO THE THERMODYNAMIC GROWTH CALCULATIONS FOR H AND A
C-----------------------------------------------------------------------
CD       PRINT *,'Calling Growth'
CBG       PRINT *, "about to call growth"
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
     1  OM, PM, PN)
CD       PRINT *, "back from growth"
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
CD       PRINT *, "back from bcs"
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
