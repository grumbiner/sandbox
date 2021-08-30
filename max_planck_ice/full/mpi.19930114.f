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
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
      INCLUDE "rheology.inc"
      INCLUDE "oml.inc"
C=======================================================================
C  Parameter:
C     -L: number of grid points in X-direction (even number only!)
C     -M: number of grid points in Y-direction (even number only!)
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
C=======================================================================
C  Common CORR: contains coriolis parameter related variables
C     -FM:      coriolis parameter for grid center [1/s]
C     -F:       coriolis parameter for grid edge [1/s]
C     -COSPHI:  COS of grid center latitude []
C     -SINPHI:  SIN of grid center latitude []
C=======================================================================
      COMMON/IPARM/H0, HNU, HNU2, ARMIN, ARMAX, HMIN
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
C=======================================================================
C  Common COORD: contains metric coefficients
C     -PM:      metric coefficients for X-direction []
C     -PN:      metric coefficients for Y-direction []
C     -DNDX:    metric term for centripetal forces in Y-direction []
C     -DMDX:    metric term for centripetal forces in X-direction []
C=======================================================================
      COMMON/VEL/U(L,M,3), V(L,M,3)
C=======================================================================
C  Common VEL: ice velocity field (defined st grid edge points)
C     -U:       X-component of ice velocity [m/s]
C     -V:       Y-component of ice velocity [m/s]
C=======================================================================
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
C=======================================================================
C  Common THCK: thickness fields (defined on grid center points)
C     -H:       mean ice thickness [m]
C     -A:       ice compactness [100%=1]
C     -HSN:     snow thickness [m]
C=======================================================================
      COMMON/FRWND/CDWIN, SINWIN, COSWIN, UWIN(L,M), VWIN(L,M)
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
C=======================================================================
C  Common PRESS: equation of state for ice pressure
C     -P:       ice strength [N/m]
C=======================================================================
      COMMON/TEMP/TICE(0:L,0:M)
C=======================================================================
C  Common TEMP: surface temperature
C     -TICE:    surface temperature of ice or snow, resp.[C]
C=======================================================================
      COMMON/TEMPM/TICM(0:L,0:M,NLEVEL)
C=======================================================================
C  Common TEMPM: surface temperatures for seven-level heat balance
C     -TICM:    surface temp. for seven ice thickness categories [C]
C               Number of levels made adjustable via icegrid.inc
C=======================================================================
C  Common RELAXP: parameters for overrelaxation routine, moved to 
C    rheology.inc
C     -MMAX:    maximum iteration steps []
C     -VRMAX:   cut off velocity difference between iteration steps[m/s]
C     -WT:      relaxation factor []
C=======================================================================
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
C=======================================================================
C  Common MASK: contains definition points for specific domain
C     -VM:      mask for grid edge points []
C     -HM:      mask for grid center points []
C     -OM:      mask to separate outflow grid points []
C     -FLM:     mask for fluxes normal to boundaries []
C=======================================================================
      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1  QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2  QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M), 
     3  QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
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
C=======================================================================
      COMMON/GEO/PI, RAD
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M)
C=======================================================================
C  Common GEO: mathematical parameters
C     -PI:      circle number []
C     -RAD:     factor for conversion of degree into radiant (PI/180)[]
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, ABLFIX, SURFWIN, ECMTYP
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
C=======================================================================
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
C=======================================================================
C  Common TAU: variables of ABL model derived dynamic forcing of sea ice
C     -CD:      drag coefficient []
C     -SINBET:  SIN of wind turning angle []
C     -COSBET:  COS of wind turning angle []
C     -BETA:    wind turning angle [deg]
C     -TAUX:    X-component of wind stress [N/m**2]
C     -TAUY:    Y-component of wind stress [N/m**2]
C=======================================================================
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1  ,UST1(0:L,0:M), TMPL1(0:L,0:M)
C=======================================================================
C  Common FLUX: heat flux and ABL stability related variables
C     -FLSE:    sensible heat flux [W/m**2]
C     -FLLA:    latent heat flux [W/m**2]
C     -WMUE1:   stability parameter []
C     -UST1:    friction velocity [m/s]
C     -TMPL1:   Richardson number or mod.Monin-Obukhov length []or[1/m]
C=======================================================================
      COMMON/OUTFLOW/NOUT, IOUT(LDO), JOUT(LDO)
C=======================================================================
C  Common OUTFLOW: contains outflow cells
C     -NOUT:    number of outflow cells []
C     -IOUT:    X-coordinate of outflow cell []
C     -JOUT:    Y-coordinate of outflow cell []
C=======================================================================
      COMMON/PMLPARM/DCVM, WUP, COSGAM, RTC, STC, QTOC
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
      COMMON/SNOFLG/SNOFLG
C=======================================================================
C  Common SNOFLG: contains switch for inclusion of snow
C=======================================================================
C=======================================================================
      COMMON/WORK/TMP(0:L,0:M), RH(0:L,0:M), RA(0:L,0:M), DFX(0:L,0:M),
     1  DFY(0:L,0:M), RSN(0:L,0:M), WRK(0:L,0:M,4), QTM(0:L,0:M), 
     2  SH(0:L,0:M)
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
C     -SH:      ATMOSPHERIC HEAT FLUX (WEIGHTED WITH ICE COMPACTNESS)
C=======================================================================
C     Output fields.  BG
      REAL HOSM(2), HOSNSM(2), FLAGI1(L,M), FLAGI2(L,M),
     1 FLAGI(0:L,0:M), SHA(0:L,0:M), QTMA(0:L,0:M), FRS(0:L,0:M),
     2 SB(0:L,0:M), BM(0:L,0:M), SHM(0:L,0:M), HMM(0:L,0:M), STAUX(L,M),
     3 STAUY(L,M), STAUM(0:L,0:M), TAUM(0:L,0:M), SU(L,M), UMM(L,M), 
     4 SV(L,M), VMM(L,M), STA(0:L,0:M), SQTM(0:L,0:M), SFLSE(0:L,0:M),
     5 STICM(0:L,0:M)
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
C     -VMM:     TIME AVERAGE OF Y-COMPONENT OF ICE VELOCITY
C     -STA:     SUMMATION OVER TIME OF AIR TEMPERATURE
C     -SQTM:    SUMMATION OVER TIME OF OCEANIC HEAT FLUX
C     -SFLSE:   SUMMATION OVER TIME OF SENSIBLE HEAT FLUX
C     -STICM:   SUMMATION OVER TIME OF ICE/SNOW SURFACE TEMPERATURE
C=======================================================================
C-----------------------------------------------------------------------
C  SET INITIAL VALUES OF RUNNING INDICES
C-----------------------------------------------------------------------
      LOLD=1
      LNEW=2
C-----------------------------------------------------------------------
C  CALL SUBROUTINE INIT TO SET UP PARAMETERS OF THE MODEL
C-----------------------------------------------------------------------
      CALL INIT
C-----------------------------------------------------------------------
C  MAKE SURE THE OUTFLOW GRID POINTS ARE SMOOTHED
C-----------------------------------------------------------------------
      CALL OUTSM(H, 1, HOSUM, HOSM(1), 
     1 PN, PM, OM, NOUT, IOUT, JOUT)
      CALL OUTSM(A, 1, SCR1, SCR2, 
     1 PN, PM, OM, NOUT, IOUT, JOUT)
      CALL OUTSM(HSN, 1, HSNOSUM, HOSNSM(1), 
     1 PN, PM, OM, NOUT, IOUT, JOUT)
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
      CALL outstr(
     1    HOSM, HOSNSM, FLAGI1, FLAGI2, 
     2    FLAGI, SHA, QTMA, FRS, 
     3    SB, BM, SHM, HMM, STAUX, 
     4    STAUY, STAUM, TAUM, SU, UMM, SV  , 
     5    OM, TAUX, TAUY, TA, FLSE, TICM, QH ,
     6    QTM, SH, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     7    CLO, T, 
     8    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC)
C-----------------------------------------------------------------------
C----------------------- MAIN COMPUTATIONAL LOOP -----------------------
C-----------------------------------------------------------------------
      DO 400 IIC=1, NTMES
       T=T+DT
C-----------------------------------------------------------------------
C  FIRST GET THE FORCING FIELDS FOR THIS TIME STEP
C-----------------------------------------------------------------------
       CALL FORFLD(LWDN, SWDN)
C-----------------------------------------------------------------------
C  CALCULATE ICE PRESSURE WITH OLD VALUES OF ICE THICKNESSES
C-----------------------------------------------------------------------
       CALL PRESSB(LOLD, P, H, A)
C-----------------------------------------------------------------------
C  NOW DO VELOCITY INTEGRATION
C**REMARK: IF FIRST TIME, THEN DO INITIAL RELAXATION TO ADJUST VELOCI-
C    TIES TO VISCOUS STRESSES AND FORCING FIELDS.
C-----------------------------------------------------------------------
       IF (T.EQ.DT.AND.NRREC.EQ.0)THEN
        CALL INITREL
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
        CALL PLAST(LRHS)
C-----------------------------------------------------------------------
C  CALCULATE THOSE PARTS OF THE EQUATIONS THAT DO NOT DEPEND ON THE NEW
C    VELOCITY, AND STORE THEM IN THE TEMPORARY ARRAYS
C-----------------------------------------------------------------------
        CALL RELCON(LOLD, LRHS)
C-----------------------------------------------------------------------
C  NOW SOLVE FOR THE NEW VELOCITIES, USING OVER-RELAXATION
C-----------------------------------------------------------------------
        CALL RELAX(LRHS, LLHS)
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
C-----------------------------------------------------------------------
C  KINETIC ENERGY FOR MIXED LAYER, Moved to GROWTH
C-----------------------------------------------------------------------
C  NOW DO THE THERMODYNAMIC GROWTH CALCULATIONS FOR H AND A
C-----------------------------------------------------------------------
       CALL GROWTH(LOLD, LNEW, LWDN, SWDN)
C-----------------------------------------------------------------------
C  SMOOTH OUTFLOW THICKNESSES AND COMPACTNESSES
C-----------------------------------------------------------------------
       CALL OUTSM(H, LNEW, HOSUM, HOSM(LNEW), 
     1 PN, PM, OM, NOUT, IOUT, JOUT)
       CALL OUTSM(A, LNEW, SCR1, SCR2, 
     1 PN, PM, OM, NOUT, IOUT, JOUT)
       CALL OUTSM(HSN, LNEW, HOSNSUM, HOSNSM(LNEW), 
     1 PN, PM, OM, NOUT, IOUT, JOUT)
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
     1    HOSM, HOSNSM, FLAGI1, FLAGI2, 
     2    FLAGI, SHA, QTMA, FRS, 
     3    SB, BM, SHM, HMM, STAUX, 
     4    STAUY, STAUM, TAUM, SU, UMM, SV  , 
     5    OM, TAUX, TAUY, TA, FLSE, TICM, QH ,
     6    QTM, SH, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     7    CLO, T, 
     8    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC)
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
      SUBROUTINE BCSINIT(VM, HM, OM, FLM)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  PURPOSE:
C     -READS DOMAIN MASKS
C     -IDENTIFIES OUTFLOW CELLS
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/OUTFLOW/NOUT,IOUT(LDO), JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
      INTEGER K1(0:L,0:M), K2(0:L,0:M)
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  INITIALIZE THE MASKS
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
        READ(10,801) ((VM(I,J), I=1,L), J=1,M)
        READ(10,802) ((HM(I,J), I=0,L), J=0,M)
        READ(10,802) ((OM(I,J), I=0,L), J=0,M)
       ELSE IF (PTYPE .EQ. 3) THEN
        IF (LATMIN .LE. 0.) THEN
          READ(10,321) ((VM(I,J), I=1,L), J=1,M)
          READ(10,322) ((HM(I,J), I=0,L), J=0,M)
          READ(10,322) ((OM(I,J), I=0,L), J=0,M)
CD        DO 1000 J = 0, M
CD          DO 1100 I = 0, L
CD            K1(I,J) = INT(OM(I,J))
CD            K2(I,J) = INT(HM(I,J))
CD 1100     CONTINUE
CD 1000   CONTINUE
CD        WRITE(20,323) ((K1(I,J), I=0,L), J=0,M)
CD        WRITE(20,323) ((K2(I,J), I=0,L), J=0,M)
CD        PRINT *, OM
         ELSE
          READ(10,311) ((VM(I,J), I=1,L), J=1,M)
          READ(10,312) ((HM(I,J), I=0,L), J=0,M)
          READ(10,312) ((OM(I,J), I=0,L), J=0,M)
        DO 1000 J = 0, M
          DO 1100 I = 0, L
            K1(I,J) = INT(OM(I,J))
            K2(I,J) = INT(HM(I,J))
 1100     CONTINUE
 1000   CONTINUE
CD        WRITE(20,313) ((K1(I,J), I=0,L), J=0,M)
CD        WRITE(20,313) ((K2(I,J), I=0,L), J=0,M)
CD        PRINT *, OM
        ENDIF      
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

  801 FORMAT (74G1.0)
  802 FORMAT (75G1.0)

  321 FORMAT (56G1.0)
  322 FORMAT (57G1.0)
  323 FORMAT (57I1)

  311 FORMAT (68G1.0)
  312 FORMAT (69G1.0)
  313 FORMAT (69I1)

  811 FORMAT ('NUMBER OF OUTFLOW GRID POINTS EXCEEDS THE DIMENSION ',I5)
      END
      SUBROUTINE DRUCKF(FELD, FLG, FAK, ADD, HEADER, ICOUNT, NX, NY)
      IMPLICIT none
C=======================================================================
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
        WRITE (TEXT(INDEX),200) INT(XP)
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
  300 FORMAT (1X,44A3)
      END
      SUBROUTINE FORFLD(LWDN, SWDN)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C     Robert Grumbine       NMC, Camp Springs                     Nov 92
C  PURPOSE:
C     -READS TEMPORALLY VARYING BOUNDARY CONDITIONS (FORCING FIELDS)
C     -Rewritten to obtain forcing fields from NMC-MRF output
C  EXTERNALS:
C     -BCSQ: SETS FORCING DATA AT CYCLIC BOUNDARIES
C     -stosselget: Manage the interaction with the MRF
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN
      COMMON/THFOR/TAIR(0:L,0:M),TD(0:L,0:M),ACL(0:L,0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      REAL TAIR, TD, ACL, PA, UG, TA, RPREC
      REAL LWDN(0:L,0:M), SWDN(0:L,0:M)
C=======================================================================
      INTEGER I, J, POLE
C-----------------------------------------------------------------------
C    Call routine to get information from the MRF
C-----------------------------------------------------------------------
      IF (LATMIN .GE. 0) THEN
        POLE = 1
       ELSE
        POLE = 2
      ENDIF

      IF (PTYPE .EQ. 2 .OR. PTYPE .EQ. 1) THEN
        CALL twoget(POLE, 1.0, UWIN, VWIN, TAIR, TD, PA, RPREC, ACL,
     1     LWDN, SWDN, IIC)

        ELSE IF (PTYPE .EQ. 3) THEN
CD        PRINT *,'Calling thregt '
        CALL thregt(POLE, 1.0, UWIN, VWIN, TAIR, TD, PA, RPREC, ACL, 
     1       LWDN, SWDN, IIC)

      ENDIF
C-----------------------------------------------------------------------
C  CALCULATION OF WIND SPEED
C-----------------------------------------------------------------------
C     Indices changed by BG to avoid referencing non-extant
C       elements of uwin, vwin.
      DO 6 J = 1, MM
        DO 6 I = 1, LM
       UG(I,J)=.25*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     1             +SQRT(UWIN(I+1,J  )**2+VWIN(I+1,J  )**2)
     2             +SQRT(UWIN(I  ,J+1)**2+VWIN(I  ,J+1)**2)
     3             +SQRT(UWIN(I+1,J+1)**2+VWIN(I+1,J+1)**2))
    6 CONTINUE

      I = L
      DO 10 J = 1, MM
       UG(I,J)=.5*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     2            +SQRT(UWIN(I  ,J+1)**2+VWIN(I  ,J+1)**2) )
  10  CONTINUE

      J = M
      DO 20 I = 1, LM
       UG(I,J)=.5*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     1            +SQRT(UWIN(I+1,J  )**2+VWIN(I+1,J  )**2) )
   20 CONTINUE

      I = L
      J = M
      UG(I,J) = SQRT(UWIN(I,J)**2+VWIN(I,J)**2 )

      RETURN
      END
      SUBROUTINE INIT
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C     Robert Grumbine       NMC, Camp Springs, MD                 Dec 92
C  PURPOSE:
C     -DEFINES BASIC PARAMETERS FOR THE MODELS
C     -SETS INITIAL FIELDS
C  OPTIONS:
C     -REDUCTION OR INCREASE OF CLOUDINESS
C     -INCLUSION OF SNOW (SNOFLG)
C     -INCLUSION OF OML-MODEL (MLFIX)
C     -INCLUSION OF AN ABL-MODEL (ABLFIX)
C     -CHOICE OF ABL-MODEL (ECMTYP)
C     -USE OF SURFACE OR UPPER LAYER WINDS (SURFWIN); THIS OPTION
C       DECIDES ABOUT USING A FIXED OR COMPUTED WIND TURNING AND, IF
C       THE ABL-MODEL IS TURNED ON (ABLFIX=1, ECMTYP=0), SIMULTANEOUSLY
C       ABOUT USING THE FRICTION VELOCITY CALCULATED VIA THE RESISTANCE
C       LAWS OF THE EKMAN-LAYER (SURFWIN=0) OR THE ONE CALCULATED VIA
C       THE MONIN-OBUKHOV THEORY (SURFWIN=1)
C     -CARTESIAN OR SPHERICAL COORDINATES (DX AND PM)
C  EXTERNALS:
C     -BCSINIT: READS IN MASKS AND REGISTERS OUTFLOW CELLS
C=======================================================================
      INCLUDE "icegrid.inc"
C     Include physical constants, rheology parameters, and mixed layer
C       parameters
      INCLUDE "physical.inc"
      INCLUDE "rheology.inc"
      INCLUDE "oml.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      COMMON/VEL/U(L,M,3), V(L,M,3)
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/TEMP/TICE(0:L,0:M)
      COMMON/TEMPM/TICM(0:L,0:M,NLEVEL)
C     Commons viscp and relaxp moved to rheology.inc
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2 QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M), 
     3 QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      COMMON/GEO/PI, RAD
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1 BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/PMLPARM/DCVM, WUP, COSGAM, RTC, STC, QTOC
      COMMON/SNOFLG/SNOFLG
C=======================================================================
C-----------------------------------------------------------------------
C  SPECIFY ZONAL AND ANNUAL MEAN OF CLOUDINESS (ACC.TO V.LOON (1972))
C  Replaced with calls to read in from MRF output.  BG 11/92
C-----------------------------------------------------------------------
C      Open run files
      OPEN (10, FILE='MASK', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='TSUVWG', FORM='FORMATTED', STATUS='OLD')
      OPEN (20, FILE='FORT.20', FORM='FORMATTED', STATUS='NEW')
      OPEN (14, FILE='RESTART', FORM='FORMATTED', STATUS='NEW')
      OPEN (15, FILE='thick', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (16, FILE='FORT.16', FORM='FORMATTED', STATUS='NEW')
      OPEN (17, FILE='conc', FORM='UNFORMATTED', STATUS='NEW')
      OPEN (18, FILE='FORT.18', FORM='FORMATTED', STATUS='NEW')
C-----------------------------------------------------------------------
C  SET RUN LENGTH and OUTPUT parameters
C-----------------------------------------------------------------------
      NTMES=14
      NRST= 14
      NSTAT=2
      NSTA= 4
      NPLT=400
      NFLD=0
      NRREC=0
C-----------------------------------------------------------------------
C  DECIDE ABOUT INCLUSION OF SNOW
C-----------------------------------------------------------------------
C**REMARK: -SNOFLG=0.: NO SNOW
C          -SNOFLG=1.: INCLUDE SNOW LAYER
      SNOFLG=1.
C-----------------------------------------------------------------------
C  DECIDE ABOUT INCLUDING THE VARIABLE OML MODEL
C-----------------------------------------------------------------------
C**REMARK: -MLFIX=1 : FIXED MIXED LAYER
C          -MLFIX=0 : PROGNOSTIC MIXED LAYER
      MLFIX=0
C-----------------------------------------------------------------------
C  DECIDE ABOUT INCLUDING AN ABL MODEL
C-----------------------------------------------------------------------
C**REMARK: -ABLFIX=1.: NO ATMOSPHERIC BOUNDARY LAYER
C          -ABLFIX=0.: WITH ATMOSPHERIC BOUNDARY LAYER
      ABLFIX=0.
C-----------------------------------------------------------------------
C  DECIDE ABOUT THE KIND OF ABL MODEL TO USE
C-----------------------------------------------------------------------
C**REMARK: -ECMTYP=1.: ASL MODEL (LOUIS, 1979)
C          -ECMTYP=0.: ABL MODEL (KOCH, 1988)
      ECMTYP=1.
C-----------------------------------------------------------------------
C  USE OF SURFACE OR UPPER LAYER WINDS
C-----------------------------------------------------------------------
C**REMARK: -SURFWIN=1.: SURFACE WINDS (FIXED WIND TURNING)
C          -SURFWIN=0.: UPPER LAYER WINDS
      SURFWIN=1.
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
          STOP 'GRID TYPE OUT OF RANGE'
      ENDIF

      DO 216 J=1,M
      DO 216 I=1,L
       DNDX(I,J)=0.
       DMDY(I,J)=0.
  216 CONTINUE
C-----------------------------------------------------------------------
C  SET UP ICE AND SNOW PARAMETERS
C-----------------------------------------------------------------------
      H0=0.5
      ARMIN=0.15
      ARMAX=1.0
      HMIN=0.05
      HNU=0.004*DX
      HNU2=DX**2*HNU
C-----------------------------------------------------------------------
C  SET UP SURFACE HEAT AND MOMENTUM TRANSFER PARAMETERS
C-----------------------------------------------------------------------
      CDWIN=1.2E-03
C**FOR SURFACE WINDS TURNING ANGLE EQUAL TO ZERO:
C     SINWIN=-0.4226E+00
C     COSWIN=0.9063E+00
      SINWIN=0.
      COSWIN=1.
      SINWAT=-0.4226E+00
      COSWAT=0.9063E+00
      FAKTH=RHOAIR*.4*CPAIR
C-----------------------------------------------------------------------
C  SET OML PARAMETERS
C-----------------------------------------------------------------------
      FLAGM=FLOAT(MLFIX)
      QHSMAX=50.0
      DCVM=EXP(-QHSMAX/QHS)
      WUP=3.0E-7
      IF (MLFIX.EQ.1) WUP=0.0
      COSGAM=.9135
      RTC=10.0*86400.0/DT
      STC=300.0*86400.0/DT
      QTOC=2.00
C-----------------------------------------------------------------------
C  SET UP PARAMETERS FOR THE RELAX SUBROUTINE, moved to physical.inc
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  INITIALIZE HORIZONTAL BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      CALL BCSINIT(VM, HM, OM, FLM)
C-----------------------------------------------------------------------
C  INITIALIZE THE SI, OML AND ABL VARIABLES (OPT. FROM RESTART TAPE)
C-----------------------------------------------------------------------
C     IF (NRREC.NE.0) THEN
C      DO 420 K=1,NRREC
C       READ(9,901,ERR=100)((U(I,J,1), V(I,J,1), I=1,L), J=1,M)
C       READ(9,901,ERR=100)((H(I,J,1), A(I,J,1), I=0,L), J=0,M)
C       READ(9,901,ERR=100)((HSN(I,J,1), TICE(I,J), I=0,L), J=0,M)
C       READ(9,901,ERR=100)((QT(I,J), QS(I,J), QH(I,J), I=0,L), J=0,M)
C       READ(9,901,ERR=100)((QTB(I,J), QSB(I,J), QHB(I,J), I=0,L), J=0,M)
C       READ(9,901,ERR=100)((QDT(I,J), QDS(I,J), I=0,L), J=0,M)
C       READ(9,901,ERR=100)(((TICM(I,J,N), I=0,L), J=0,M), N=1,NLEVEL)
C 420  CONTINUE
C     ELSE
      DO 280 J=1,M
      DO 280 I=1,L
       U(I,J,1)=0.
       V(I,J,1)=0.
  280 CONTINUE
      DO 281 J=0,M
      DO 281 I=0,L
       ZOW(I,J)=ZOWI
       H(I,J,1)=0.
       A(I,J,1)=0.
       HSN(I,J,1)=0.
       TICE(I,J)=-.16
       QH(I,J)=60.0
       QS(I,J)=33.3
       QT(I,J)=TFREZ
       QHB(I,J)=3000.
       QDT(I,J)=70.
       QDS(I,J)=20.
C      BG ADDITION TO AVOID INITIALIZING NON-EXISTENT ARRAY
C        ELEMENTS.  TAUX, TAUY ARE DIMENSIONED 1-L, 1-M.
       IF (I*J .NE. 0) TAUX(I,J)=0.
       IF (I*J .NE. 0) TAUY(I,J)=0.
  281 CONTINUE
      
      IF (PTYPE .EQ. 1 .OR. PTYPE .EQ. 2) THEN
        DO 283 J=0, M/2
        DO 283 I=0,L
         H(I,J,1)=2.*(1.-J/FLOAT(M/2))*OM(I,J)
         A(I,J,1)=1.*(1.-J/FLOAT(M/2))*OM(I,J)
         HSN(I,J,1)=.5*(1.-J/FLOAT(M/2))*OM(I,J)
  283   CONTINUE
       ELSE
        DO 285 J = 0, M
        DO 285 I = 0, L
          H(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                       (1. - (J/FLOAT(M/2)-1.)**2)*0.0
          A(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                       (1. - (J/FLOAT(M/2)-1.)**2)*0.0
          HSN(I,J,1) = OM(I,J)*(1. - (I/FLOAT(L/2)-1.)**2)*
     1                       (1. - (J/FLOAT(M/2)-1.)**2)*0.0
  285   CONTINUE
      ENDIF

      DO 282 K=1,NLEVEL
      DO 282 J=0,M
      DO 282 I=0,L
       TICM(I,J,K)=TICE(I,J)
  282 CONTINUE
C-----------------------------------------------------------------------
C  READ IN FIELDS THAT ARE CONSTANT IN TIME
C-----------------------------------------------------------------------
CD      READ(12,802,ERR=100)((QTB(I,J), I=2,LM), J=1,M-2)
CD      READ(12,802,ERR=100)((QSB(I,J), I=2,LM), J=1,M-2)
CD      READ(12,802,ERR=100)((UWAT(I,J), VWAT(I,J), I=2,LM), J=1,MM)
      DO 1000 J = 1, M-2
        DO 1100 I = 2, LM
          QTB(I,J) = -1.5
          QSB(I,J) = 34.7
          UWAT(I,J) = 0.0
          VWAT(I,J) = 0.0
 1100   CONTINUE
 1000 CONTINUE

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
       HT(I,J)=(QT(I,J)-QTB(I,J))*(QH(I,J)+QDT(I,J))+QTB(I,J)*QHB(I,J)
       HS(I,J)=(QS(I,J)-QSB(I,J))*(QH(I,J)+QDS(I,J))+QSB(I,J)*QHB(I,J)
   80 CONTINUE
C-----------------------------------------------------------------------
C  WRITE CURRENT CONTROL PARAMETERS
C-----------------------------------------------------------------------
      WRITE(16,403) L, M, MLFIX, ABLFIX, ECMTYP, SURFWIN, DX, DY, 
     1  DT, H0, PSTAR, WT,
     3  ZOI, SINWIN, COSWIN, QTOC, CDWIN, CDWAT

      RETURN


  100 WRITE(*,400)
  403 FORMAT (
     1 ' L      = ',I12,    ' M      = ',I12,    ' MLFIX  = ',I12,/,
     2 ' ABLFIX = ',1PE12.3,' ECMTYP = ',1PE12.3,' SURFWIN= ',1PE12.3,/,
     3 ' DX     = ',1PE12.3,' DY     = ',1PE12.3,' DT     = ',1PE12.3,/,
     4 ' H0     = ',1PE12.3,' PSTAR  = ',1PE12.3,' WT     = ',1PE12.3,/,
     5 ' ZOI    = ',1PE12.3,' SINWIN = ',1PE12.3,' COSWIN = ',1PE12.3,/,
     6 ' QTOC   = ',1PE12.3,' CDWIN  = ',1PE12.3,' CDWAT  = ',1PE12.3)
  802 FORMAT (18F6.2)
C 901 FORMAT (5E13.4)
  400 FORMAT ('1  READ ERROR IN INIT ')
      STOP
      END
      SUBROUTINE INITREL
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
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
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      COMMON/VEL/U(L,M,3),V(L,M,3)
      REAL U, V
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      REAL H, A, HSN
      COMMON/WORK/TRK(1:L,1:M,4),AMAS(1:L,1:M),BU(1:L,1:M),BV(1:L,1:M),
     1FX(1:L,1:M),FY(1:L,1:M),
     1ASY(1:L,1:M),ZETA(1:L,1:M),ETA(1:L,1:M)
      REAL TRK, AMAS, BU, BV, FX, FY, ASY, ZETA, ETA
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
      CALL OUTBCS
C-----------------------------------------------------------------------
C  SET UP DIAGNOSTIC PART OF THE MOMENTUM EQUATION
C-----------------------------------------------------------------------
      CALL RELCON(1,1)
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
      CALL RELAX(1,2)
      DO 30 J=1,M
      DO 30 I=1,L
       U(I,J,1)=U(I,J,2)
       V(I,J,1)=V(I,J,2)
   30 CONTINUE
      RETURN
      END
      SUBROUTINE getflx(swdn, lwdn, t2, q2, uten, vten, 
     1    qsen, qlat, cloud, mask, iunit )
C=====================================================================--
C  Programmed by:
C     Robert W. Grumbine        NMC, Camp Springs, MD            Dec '92
C  Purpose:
C     Read selected fields from MRF flux files, which are in GRIB
C       format.  
C  Externals:
C     W3AI08:    NMC W3LIB routine to unpack GRIB data
C     GAU2L:     Jordan Alpert to convert gaussian latitude data
C                  to regular latitude-longitude grids.
C     BUFFERIN:  Cray routine to manage the reading of the GRIB
C                  data fields.  Will run only on Cray.
C=====================================================================--
      IMPLICIT none
      INCLUDE "mgrid.inc"
C=====================================================================--

      INTEGER iunit
      INTEGER nlong, nlat
      PARAMETER (nlong = 360./dlonm)
      PARAMETER (nlat  = 180./dlatm + 1)

      REAL swdn(nlong, nlat), lwdn(nlong, nlat)
      REAL t2(nlong, nlat), q2(nlong, nlat)
      REAL uten(nlong, nlat), vten(nlong, nlat)
      REAL qsen(nlong, nlat), qlat(nlong, nlat)
      REAL cloud(nlong, nlat), mask(nlong, nlat)

C=====================================================================--
C     Variables for de-gribbing.
      INTEGER idim, jdim, ijdim
      INTEGER mxbits, maxgrb
C     Grid parameters.
      PARAMETER (idim = 3*nwave+6     )
      PARAMETER (jdim = (3*nwave+2)/2 )
      PARAMETER (ijdim = idim*jdim)
C     Parameters for GRIB extraction
      PARAMETER (mxbits = 16)
      PARAMETER (maxgrb = 82 + ((ijdim+15)/16)*2
     1                       + ((ijdim*mxbits+7)/16)*2 )

C     Arguments to the GRIB extraction routine W3AI08.
      INTEGER kgds(13), kpds(17), kptr(10), kret
      LOGICAL lbms(ijdim)
      REAL    grid(ijdim)
      CHARACTER*1 pack(maxgrb)
C     Variables which may be used to detect proper reading in
C       the BUFFERIN process which obtains the data from the
C       GRIB packed file.
      REAL rcrgrb    !-1. for a success, 0. for failure (EOF)
      INTEGER lengrb !record length in bytes
C=====================================================================--

C     Pointers to the fields of the surface flux files (s2d files).
      INTEGER utau, vtau, qsense, qlaten, tsa, soilw, sndep
      INTEGER lwdns, lwups, lwupt, swupt, swups, swdns
      PARAMETER (utau   =  3) !N/m2, 7 bits
      PARAMETER (vtau   =  4) !N/m2, 7 bits
      PARAMETER (qsense =  5) !W/m2, 0 bits
      PARAMETER (qlaten =  6) !W/m2, 0 bits
      PARAMETER (tsa    =  7) !K, surface air temperature, 3 bits
      PARAMETER (soilw  =  8) 
      PARAMETER (sndep  =  9)
      PARAMETER (lwdns  = 10) !w/m2, 0 bits
      PARAMETER (lwups  = 11) !w/m2, 0 bits
      PARAMETER (lwupt  = 12) !w/m2, 0 bits
      PARAMETER (swupt  = 13) !w/m2, 0 bits
      PARAMETER (swups  = 14) !w/m2, 0 bits
      PARAMETER (swdns  = 15) !w/m2, 0 bits

      INTEGER hcf, hcbot, hctop, hctemp, mcf, mcbot, mctop, mctemp
      INTEGER lcf, lcbot, lctop, lctemp
      PARAMETER (hcf    = 16)
      PARAMETER (hcbot  = 17)
      PARAMETER (hctop  = 18)
      PARAMETER (hctemp = 19)
      PARAMETER (mcf    = 20)
      PARAMETER (mcbot  = 21)
      PARAMETER (mctop  = 22)
      PARAMETER (mctemp = 23)
      PARAMETER (lcf    = 24)
      PARAMETER (lcbot  = 25)
      PARAMETER (lctop  = 26)
      PARAMETER (lctemp = 27)

      INTEGER rain, crain, ghflux, slimsk, uwind, vwind
      INTEGER t2m, q2m, pslp
      PARAMETER (rain   = 28) !mm,   3 bits
      PARAMETER (crain  = 29) !mm,   3 bits
      PARAMETER (ghflux = 30) !w/m2, 0 bits
      PARAMETER (slimsk = 31) ! ?,   0 bits
      PARAMETER (uwind  = 32) !m/s,  3 bits
      PARAMETER (vwind  = 33) !m/s,  3 bits
      PARAMETER (t2m    = 34) !K,    3 bits
      PARAMETER (q2m    = 35) !.1g/kg, 0 bits
      PARAMETER (pslp   = 36) !hPa (mb), 3 bits, surface level pressure.
      INTEGER nfield
      PARAMETER (nfield = 36)
C=====================================================================--

C     Local variables. 
      INTEGER i, j, k 
      REAL tempor(idim, jdim)
C=====================================================================--

C     Loop over all fields.  Copy grid into array for selected
C       arrays.
      DO 1000 k = 1, nfield

C     Call bufferin to get the data to be unpacked into the
C       unpacking array.
       BUFFERIN (iunit, 0) (pack(1), pack(maxgrb*8))
       rcrgrb = UNIT(iunit)
       lengrb = LENGTH(iunit)
        IF (rcrgrb .EQ. -1.) THEN
         ELSE
          PRINT *,'Read failed, at EOF with field# ',k
          STOP
        ENDIF

C       Call w3ai08 to extract the data from grib field into
C         data field.  First two calls will fail.
        CALL W3AI08(pack, kpds, kgds, lbms, grid, kptr, kret)
        IF ( kret .NE. 0) THEN
          IF (k .NE. 1 .AND. k .NE. 2) THEN
            PRINT *,'Error in the GRIB extraction process.'
            PRINT *,'Attempting to extract field # ',k
            PRINT *,'Return code ',kret
            PRINT *,'kpds ',kpds
            PRINT *,'kgds ',kgds
            PRINT *,'kptr ',kptr
          ENDIF
        ENDIF

C       Now have data in the field 'grid'.  Logical flags in
C         field lbms tag whether ...? WHAT

C       Now put data field into useable data field for desired fields.

        IF (k .EQ. t2m) THEN
          DO 2000 j = 1, jdim
            DO 2001 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2001       CONTINUE
 2000     CONTINUE
C         Convert data to regular lat-long grid
          CALL GAU2L(tempor, idim, jdim, t2, nlong, nlat)
        ENDIF

        IF (k .EQ. q2m) THEN
          DO 2002 j = 1, jdim
            DO 2003 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2003       CONTINUE
 2002     CONTINUE
          CALL GAU2L(tempor, idim, jdim, q2, nlong, nlat)
        ENDIF

        IF (k .EQ. qsense) THEN
          DO 2004 j = 1, jdim
            DO 2005 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2005       CONTINUE
 2004     CONTINUE
          CALL GAU2L(tempor, idim, jdim, qsen, nlong, nlat)
        ENDIF

        IF (k .EQ. qlaten) THEN
          DO 2006 j = 1, jdim
            DO 2007 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2007       CONTINUE
 2006     CONTINUE
          CALL GAU2L(tempor, idim, jdim, qlat, nlong, nlat)
        ENDIF

        IF (k .EQ. lcf) THEN
          DO 2016 j = 1, jdim
            DO 2017 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2017       CONTINUE
 2016     CONTINUE
          CALL GAU2L(tempor, idim, jdim, cloud, nlong, nlat)
        ENDIF

        IF (k .EQ. slimsk) THEN
          DO 2018 j = 1, jdim
            DO 2019 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2019       CONTINUE
 2018     CONTINUE
          CALL GAU2L(tempor, idim, jdim, mask, nlong, nlat)
        ENDIF

        IF (k .EQ. uwind) THEN
          DO 2008 j = 1, jdim
            DO 2009 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2009       CONTINUE
 2008     CONTINUE
          CALL GAU2L(tempor, idim, jdim, uten, nlong, nlat)
        ENDIF

        IF (k .EQ. vwind) THEN
          DO 2010 j = 1, jdim
            DO 2011 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2011       CONTINUE
 2010     CONTINUE
          CALL GAU2L(tempor, idim, jdim, vten, nlong, nlat)
        ENDIF

        IF (k .EQ. lwdns) THEN
          DO 2012 j = 1, jdim
            DO 2013 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2013       CONTINUE
 2012     CONTINUE
          CALL GAU2L(tempor, idim, jdim, lwdn, nlong, nlat)
        ENDIF

        IF (k .EQ. swdns) THEN
          DO 2014 j = 1, jdim
            DO 2015 i = 1, idim
              tempor(i,j) = grid((j-1)*idim+i)
 2015       CONTINUE
 2014     CONTINUE
          CALL GAU2L(tempor, idim, jdim, swdn, nlong, nlat)
        ENDIF

C       Now finished de-gribbing.
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE getsig(slp, temp, ztopo, qsig, iunit, sigma)
C=======================================================================
C  Programmed by:
C     Robert W. Grumbine          NMC  Camp Springs, MD          Dec '92
C  Purpose:
C     Read in MRF forecat sigma fields.
C  EXTERNALS:
C     None
C=======================================================================
      IMPLICIT none
      INCLUDE "mgrid.inc"
C=======================================================================

      INTEGER iunit
      REAL slp(mwave), temp(mwave), ztopo(mwave), qsig(mwave)
      REAL sigma, dummy(mwave)

C     Grid parameters.
      INTEGER idim, jdim, ijdim
      PARAMETER (idim =  3*nwave+6   )
      PARAMETER (jdim = (3*nwave+2)/2)
      PARAMETER (ijdim = idim*jdim)

C     Header info for sigma files
      CHARACTER*8 lab(4)
      REAL fhour, dphi(kdim+1), dlam(kdim)
      INTEGER*4 idate(4)
      
C     Local variables. 
      INTEGER i, k, klev
      REAL delsig, olddel
C=======================================================================

C     Program version for using sigma files from forecast.
C     Read in from sigma files
      READ (iunit) lab
      READ (iunit) fhour, (idate(i),i=1,4), (dphi(k),k=1,kdim+1),
     1                    (dlam(k),k=1,kdim)

C     Determine which level is nearest the desired sigma level.
C     Assume that the levels are arranged in monotonically in/de-
C       creasing order.
      k = 1
      olddel = ABS(sigma-dlam(1))
 1000 CONTINUE
        delsig = ABS(sigma-dlam(k+1))
        IF (delsig .GT. olddel) THEN
          klev = k
          GO TO 2000
         ELSE
          olddel = delsig
          k = k + 1
          GO TO 1000
        ENDIF 
 2000 CONTINUE

      READ (iunit) (ztopo(i),i=1,mwave)
      READ (iunit) (slp(i),i=1,mwave)
C     Note that the slp field is actually LN(Ps), where Ps in in
C       kPa.

      DO 3000 k = 1, klev
        READ (iunit) (temp(i),i=1,mwave)
 3000 CONTINUE
      DO 3100 k = klev+1, kdim
        READ (iunit) (dummy(i),i=1,mwave)
 3100 CONTINUE

C     Dummy reads through the divergence fields
      DO 4000 k = 1, kdim
        READ (iunit) (dummy(i),i=1,mwave)
 4000 CONTINUE
C     Dummy reads through the vorticity fields
      DO 4100 k = 1, kdim
        READ (iunit) (dummy(i),i=1,mwave)
 4100 CONTINUE
C     Read to the q field
      DO 4200 k = 1, klev
        READ (iunit) (qsig(i),i=1,mwave)
 4200 CONTINUE
CD      DO 4300 k = klev+1, kdim
CD        READ (iunit) (dummy(i),i=1,mwave)
CD 4300 CONTINUE
C     Now in a position to read the rain field.      
  
      RETURN
      END
C=======================================================================
      SUBROUTINE output (
     1    HOSM, HOSNSM, FLAGI1, FLAGI2, 
     2    FLAGI, SHA, QTMA, FRS, 
     3    SB, BM, SHM, HMM, STAUX, 
     4    STAUY, STAUM, TAUM, SU, UMM, SV  , 
     5    OM, TAUX, TAUY, TA, FLSE, TICM, QH ,
     6    QTM, SH, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     7    CLO, T, 
     8    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC)
C=======================================================================
      IMPLICIT none
C=======================================================================
C  Programmed by:
C     Robert Grumbine       NMC, Camp Springs                     Oct.92
C  Purpose:
C     -Localize all model output to a single routine.  Clarify 
C       program logic (previously output was from main program).
C  Interface:
C     -INPUT,OUTPUT: standard control output
C     -TAPE15: ice velocity results for plots
C     -TAPE16: results printed in domain's shape
C     -TAPE17: various results for plots
C     -TAPE18: accumulated results for summation plots
C     -TAPE20: ice compactness results for selected dates (plots)
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
C  Parameter:
C     -L: number of grid points in X-direction (even number only!)
C     -M: number of grid points in Y-direction (even number only!)
C=======================================================================

C     Arguments
      REAL OM(0:L,0:M), TAUX(L,M), TAUY(L,M), TA(0:L,0:M)
      REAL FLSE(0:L,0:M), TICM(0:L,0:M,NLEVEL), QH(0:L,0:M)
      REAL QTM(0:L,0:M), SH(0:L,0:M)
      REAL A(0:L,0:M,2), H(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL FW(0:L,0:M), U(0:L,0:M,3), V(0:L,0:M,3)
      REAL PN(0:L,0:M), PM(0:L,0:M)
      REAL QS(0:L,0:M), QT(0:L,0:M), VM(L,M)
      REAL CLO, T
      INTEGER IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES

C=======================================================================
C     Local variables
      REAL HOSM(2), HOSNSM(2), FLAGI1(L,M), FLAGI2(L,M),
     1 FLAGI(0:L,0:M), SHA(0:L,0:M), QTMA(0:L,0:M), FRS(0:L,0:M),
     2 SB(0:L,0:M), BM(0:L,0:M), SHM(0:L,0:M), HMM(0:L,0:M), 
     3 STAUX(L,M), STAUY(L,M), STAUM(0:L,0:M), TAUM(0:L,0:M), 
     4 SU(L,M), UMM(L,M), SV(L,M),
     5 VMM(L,M), STA(0:L,0:M), SQTM(0:L,0:M), SFLSE(0:L,0:M), 
     6 STICM(0:L,0:M)
      REAL FLAG, FLAG1, HEX, HEX1, HEX2, HEXI
      REAL HOSNSUM, HOTSUM, HSNSUM, HSUM, HU, HV, HWEX, HWEX2
      REAL HOSNTSUM, HOSUM
      REAL Z1, Z2, SHSUM, QTMSUM
      REAL CONC(0:L, 0:M), THICK(0:L, 0:M)

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
C     -VMM:     TIME AVERAGE OF Y-COMPONENT OF ICE VELOCITY
C     -STA:     SUMMATION OVER TIME OF AIR TEMPERATURE
C     -SQTM:    SUMMATION OVER TIME OF OCEANIC HEAT FLUX
C     -SFLSE:   SUMMATION OVER TIME OF SENSIBLE HEAT FLUX
C     -STICM:   SUMMATION OVER TIME OF ICE/SNOW SURFACE TEMPERATURE
C=======================================================================
      INTEGER I, J, K
      INTEGER NRREC, NRST
C     ARMAG is the magic number used to determine compact ice conditions
      REAL ARMAG
      PARAMETER (ARMAG = 0.85)

      SAVE
C-----------------------------------------------------------------------
C  STATISTIC FOR OUTFLOW
C-----------------------------------------------------------------------
C  CHANGE OF OUTFLOW IN TERMS OF ICE VOLUME:
       HOSUM=HOSUM-HOSM(LOLD)
       HOSUM=HOSUM*Z1*1.E-5
       HOTSUM=HOTSUM+HOSUM*1.E-1
C  CHANGE OF OUTFLOW IN TERMS OF SNOW VOLUME:
       HOSNSUM=HOSNSUM-HOSNSM(LOLD)
       HOSNTSUM=HOSNTSUM+HOSNSUM
C-----------------------------------------------------------------------
C  SUMMATION OF VARIABLES FOR CONTINUOUS STATISTICS
C-----------------------------------------------------------------------
       DO 130 J=0,M
       DO 130 I=0,L
        QTMA(I,J)=QTMA(I,J)+QTM(I,J)
        SHA(I,J)=SHA(I,J)+SH(I,J)*CLO/DT
  130  CONTINUE
C**NEXT COMPUTATIONS START AT SPECIFIED STATISTICS INTERVAL ONLY:
       IF (MOD(IIC,NSTAT).EQ.0) THEN
C-----------------------------------------------------------------------
C  AVERAGE OCEANIC AND ATMOSPHERIC HEAT FLUXES
C-----------------------------------------------------------------------
        Z1=FLOAT(NSTAT)
        DO 135 J=0,M
        DO 135 I=0,L
         QTMA(I,J)=QTMA(I,J)/Z1
         SHA(I,J)=SHA(I,J)/Z1
  135   CONTINUE
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
         QTMSUM=QTMSUM+OM(I,J)*FLAGI(I,J)*QTMA(I,J)
         SHSUM=SHSUM+OM(I,J)*FLAGI(I,J)*SHA(I,J)
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

C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
C-----------------------------------------------------------------------
C  PRINT OUT GEOGRAPHICAL PATTERNS
C-----------------------------------------------------------------------
         IF (MOD(IIC,NSTA).EQ.0) THEN
          CALL DRUCKF(H(0,0,LNEW),OM,10.,0.,'H*10 [M]',IIC,LP,MP)
          CALL DRUCKF(A(0,0,LNEW),OM,100.,0.,'A [%]',IIC,LP,MP)
CD          CALL DRUCKF(U(1,1,LNEW),VM,100.,0.,'U [CM/S]',IIC,L,M)
CD          CALL DRUCKF(V(1,1,LNEW),VM,100.,0.,'V [CM/S]',IIC,L,M)
          CALL DRUCKF(HSN(0,0,LNEW),OM,100.,0.,'HSNOW [CM]',IIC,LP,MP)
          CALL DRUCKF(QT,OM,10.,0.,'M L TEMP*10 [DEG C]',IIC,LP,MP)
          CALL DRUCKF(TICM(0,0,4),OM,1.,0.,'ICE TEMP [DEG C]',IIC,LP,MP)
          CALL DRUCKF(QS,OM,10.,-300.,'SALT M L *10 -300',IIC,LP,MP)
          CALL DRUCKF(QH,OM,1.,0.,'M L DEPTH [M]',IIC,LP,MP)
          CALL DRUCKF(QTMA,OM,1.,0.,'OC. HEAT FLUX [W/M**2]',IIC,LP,MP)
          CALL DRUCKF(SHA,OM,.1,0.,'ATM. HEAT FL./10[W/M**2]',IIC,LP,MP)
          WRITE(16,901)
C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
C**REMARK: MOD. FOR REAL-TIME DAILY FORCING: 6TH YEAR:+5; 5TH YEAR:+20.
         IF (MOD((IIC+5),NPLT).EQ.0) THEN
C-----------------------------------------------------------------------
C  DETERMINATION OF MONTHLY MEAN VALUES
C-----------------------------------------------------------------------
          DO 309 J=1,MM
          DO 309 I=2,LM
           HV=H(I,J,LNEW)+H(I,J-1,LNEW)+H(I-1,J-1,LNEW)+H(I-1,J,LNEW)
           FLAGI1(I,J)=.5*(1.-SIGN(1.,.0-HV))
           BM(I,J)=SB(I,J)/NPLT
           HMM(I,J)=SHM(I,J)/NPLT
           TAUX(I,J)=STAUX(I,J)/NPLT
           TAUY(I,J)=STAUY(I,J)/NPLT
           TAUM(I,J)=STAUM(I,J)/NPLT
           UMM(I,J)=SU(I,J)/NPLT
           VMM(I,J)=SV(I,J)/NPLT
           TA(I,J)=STA(I,J)/NPLT
           QTMA(I,J)=SQTM(I,J)/NPLT
           FLSE(I,J)=SFLSE(I,J)/NPLT
           TICM(I,J,4)=STICM(I,J)/NPLT
           SB(I,J)=0.
           STAUX(I,J)=0.
           STAUY(I,J)=0.
           STAUM(I,J)=0.
           SU(I,J)=0.
           SV(I,J)=0.
           STA(I,J)=0.
           SQTM(I,J)=0.
           SFLSE(I,J)=0.
           STICM(I,J)=0.
           HU=HMM(I,J)+HMM(I,J-1)+HMM(I-1,J-1)+HMM(I-1,J)
           FLAGI2(I,J)=.5*(1.-SIGN(1.,.0-HU))
           FLAG1=.5*(1.-SIGN(1., ARMAG-BM(I,J)))
           HEX2=HEX2+FLAG1*OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))
           HWEX2=HWEX2+OM(I,J)*Z1*1.E-6*BM(I,J)/(PN(I,J)*PM(I,J))
  309     CONTINUE
C-----------------------------------------------------------------------
C  WRITE MODEL OUTPUT FOR PLOTTING
C-----------------------------------------------------------------------
          WRITE(15, 974) 'U', IIC, T
          DO 310 J=1, MM
  310     WRITE(15, 973) (U(I, J, LNEW)*FLAGI1(I, J), I=2, LM)
          WRITE(15, 974) 'V', IIC, T
          DO 312 J=1, MM
  312     WRITE(15, 973) (V(I, J, LNEW)*FLAGI1(I, J), I=2, LM)
          WRITE(15, 974) 'UM', IIC, T
          DO 311 J=1, MM
  311     WRITE(15, 973) (UMM(I, J)*FLAGI2(I, J), I=2, LM)
          WRITE(15, 974) 'VM', IIC, T
          DO 313 J=1, MM
  313     WRITE(15, 973) (VMM(I, J)*FLAGI2(I, J), I=2, LM)
          WRITE(15, 974) 'TAUX', IIC, T
          DO 315 J=1, MM
  315     WRITE(15, 973) (TAUX(I, J), I=2, LM)
          WRITE(15, 974) 'TAUY', IIC, T
          DO 317 J=1, MM
  317     WRITE(15, 973) (TAUY(I, J), I=2, LM)
          WRITE(17, 974) 'H', IIC, T
          DO 314 J=1, MM
  314     WRITE(17, 973) (H(I, J, LNEW), I=2, LM)
          WRITE(17, 974) 'A', IIC, HEX2
          DO 316 J=1, MM
  316     WRITE(17, 973) (A(I, J, LNEW), I=2, LM)
          WRITE (17, 974) 'TICM', IIC, T
          DO 318 J=1, MM
  318     WRITE(17, 973) (TICM(I, J, 4)*FLAGI(I, J), I=2,LM)
          WRITE(17, 974) 'TA', IIC, T
          DO 320 J=1, MM
  320     WRITE(17, 973) (TA(I, J), I=2, LM)
          WRITE(17, 974) 'FLSE', IIC, T
          DO 322 J=1, MM
  322     WRITE(17, 973) (FLSE(I, J), I=2, LM)
          WRITE(17, 974) 'QH', IIC, T
          DO 324 J=1, MM
            WRITE(17, 973) (QH(I, J), I=2, LM)
  324     CONTINUE
          WRITE(17, 974) 'QTMA', IIC, T
          DO 326 J=1, MM
  326     WRITE(17, 973) (QTMA(I, J)*FLAGI2(I, J), I=2, LM)
          WRITE(17, 974) 'TAUM', IIC, T
          DO 328 J=1, MM
  328     WRITE(17, 973) (TAUM(I, J), I=2, LM)
          IF (MOD(IIC, NTMES).EQ.0) THEN
           WRITE(17, 974) 'FRS', IIC, T
           DO 329 J=1, MM
  329      WRITE(17, 973) (FRS(I, J), I=2, LM)
          END IF
         END IF
        END IF
C-----------------------------------------------------------------------
C  ZERO OUT AVERAGING ARRAYS
C-----------------------------------------------------------------------
        DO 390 J=0, M
        DO 390 I=0, L
         QTMA(I, J)=.0
         SHA(I, J)=.0
  390   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  WRITE OUT RESTART DATA
C-----------------------------------------------------------------------
      IF (MOD(IIC, NRST).EQ.0) THEN
       WRITE(14, 9101) ((U(I, J, LNEW), V(I, J, LNEW), I=1, L), J=1, M)
       WRITE(14, 9101) ((H(I, J, LNEW), A(I, J, LNEW), I=0, L), J=0, M)
CD       WRITE(14, 9101) ((HSN(I, J, LNEW), TICE(I, J), I=0, L), J=0, M)
CD       WRITE(14, 9101) ((QT(I, J), QS(I, J), QH(I, J), I=0, L), J=0, M)
CD       WRITE(14, 9101) ((QTB(I, J), QSB(I, J), QHB(I, J), I=0, L), J=0, M)
CD       WRITE(14, 9101) ((QDT(I, J), QDS(I, J), I=0, L), J=0, M)
       WRITE(14, 9101) (((TICM(I, J, K), I=0, L), J=0, M), K=1, NLEVEL)
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
  973 FORMAT (13E10.3)
  974 FORMAT (2X, A5, I8, E12.5)
  960 FORMAT (5F8.4)
 9101 FORMAT (5E13.4)
  980 FORMAT (' ',I5,' RESTART RECORDS WRITTEN')

CD      PRINT *,'Leaving output '
      RETURN

C-----------------------------------------------------------------------
      ENTRY outstr (
     1    HOSM, HOSNSM, FLAGI1, FLAGI2, 
     2    FLAGI, SHA, QTMA, FRS, 
     3    SB, BM, SHM, HMM, STAUX, 
     4    STAUY, STAUM, TAUM, SU, UMM, SV, 
     5    OM, TAUX, TAUY, TA, FLSE, TICM, QH ,
     6    QTM, SH, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     7    CLO, T, 
     8    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC)
C     Initialize output fields and print initial conditions
      NRREC = 0
C-----------------------------------------------------------------------
C  SET RUNNING SUM VALUES TO 0
C-----------------------------------------------------------------------
C  CUMULATIVE OUTFLOW IN TERMS OF ICE THICKNESS:
      HOTSUM=.0
C  CUMULATIVE OUTFLOW IN TERMS OF SNOW THICKNESS:
      HOSNTSUM=.0
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
C  SET RUNNING SUM ARRAY VALUES TO 0
C-----------------------------------------------------------------------
      DO 4 J=1,MM
      DO 4 I=2,LM
       QTMA(I,J)=.0
       FRS(I,J)=.0
       SHM(I,J)=.0
       SHA(I,J)=.0
       STAUX(I,J)=.0
       STAUY(I,J)=.0
       STA(I,J)=.0
       SU(I,J)=.0
       SV(I,J)=.0
       SQTM(I,J)=.0
       SFLSE(I,J)=.0
       STAUM(I,J)=.0
       STICM(I,J)=.0
    4 CONTINUE
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
      SUBROUTINE terp(globe, polar, pole, proj)
C=======================================================================
C  Programmed by:
C     Robert W. Grumbine          NMC  Camp Springs, MD         Dec. '92
C  Purpose:
C     Interpolate from a spherical grid derived from the MRF to the
C       polar stereographic grid used by the ice model.
C  EXTERNALS:
C     W3FT32 -  NMC W3LIB routine to conduct interpolations between
C                 grids.  Variant modified by BG must be used.
C=======================================================================
C     IMPLICIT none is a non-standard feature.  It compels 
C       all variables to be typed, on systems which recognize it.
      IMPLICIT none

      INCLUDE "icegrid.inc"
      INCLUDE "mgrid.inc"
C=======================================================================

      INTEGER idim, jdim
C     Grid parameters.
      PARAMETER (idim = 3*nwave+6     )
      PARAMETER (jdim = (3*nwave+2)/2 )

C     Interpolation parameters
      INTEGER tlat, tlong, nx, ny
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm +1)
      PARAMETER (nx    =   L)
      PARAMETER (ny    =   M)

      REAL field(tlong+1, tlat/2+1), tempor(nx, ny), polar(0:nx, 0:ny)
      REAL globe(tlong, tlat)
      INTEGER mapin, mapout, interp, ier, pole, proj
      REAL sigma

C     Local variables. 
      INTEGER i, j
C=======================================================================

      DO 100 j = 1, tlat
        DO 110 i = 1, tlong
          field(i,j/2+1) = 0.0
  110   CONTINUE
  100 CONTINUE

C     Section off the polar domain (select the polar region)
 9001 FORMAT (I3)
      DO 2000 j = 1, tlat/2+1
        DO 2100 i = 1, tlong
          IF (pole .EQ. 1) THEN
            field(i,tlat/2+2-j) = globe(i,j)
           ELSE
            field(i,j) = globe(i,tlat+1-j)
          ENDIF
 2100   CONTINUE
          IF (pole .EQ. 1) THEN
            field(tlong+1,tlat/2+2-j) = field(1,tlat/2+2-j)
           ELSE
            field(tlong+1,j) = field(1,j)
          ENDIF
 2000 CONTINUE

C     Interpolate to the polar stereographic grid
      mapin = 34+pole
      mapout = proj+pole
      interp = 1
CD      PRINT *,'params ',mapin, mapout, interp, proj, pole
      CALL w3ft32(field, mapin, tempor, mapout, interp, ier)
      IF (ier .NE. 0) STOP 'ier .ne. 0'

C     For Stossel model, assign i=0, j=0 points
      DO 3001 j = 1, M
        DO 3002 i = 1, L
          polar(i,j) = tempor(i,j)
 3002   CONTINUE
 3001 CONTINUE

      DO 3000 i = 1, L
        polar(i,0) = tempor(i,1)
 3000 CONTINUE
      DO 3100 j = 1, M
        polar(0,j) = tempor(1,j)
 3100 CONTINUE
      polar(0,0) = tempor(1,1)

      RETURN
      END
      SUBROUTINE thregt(pole, siglev, u850, v850, t850, rh850,
     1  ps, precip, acl, lwic, swic, step)
C=====================================================================--
C  Programmed by:
C     Robert W. Grumbine         NMC  Camp Springs, MD          Dec. '92
C  Purpose:
C     Obtain meteorological forcing data from the MRF forecasts in the
C       case that the ice model grid is polar stereographic (PTYPE=3).
C       Argument siglev selects the sigma level to attempt to 
C       get the data from.  (typically surface or 850 mb, hene the 850
C       in variable names.)
C  EXTERNALS:
C     getsig   Read in the MRF sigma files
C     getflx   Read in the MRF flux files
C     SPHERT   Convert sigma file data to regular spherical grid
C     terp     Convert regular spherical grid to polar stereographic
C=====================================================================--
C     IMPLICIT none is a non-standard feature.  It compels 
C       all variables to be typed, on systems which recognize it.
      IMPLICIT none
C=====================================================================--
      INCLUDE "icegrid.inc"
      INCLUDE "mgrid.inc"
      INCLUDE "physical.inc"
C=====================================================================--

      INTEGER step
      REAL siglev

      INTEGER idim, jdim, ijdim
C     Grid parameters.
      PARAMETER (idim = 3*nwave+6     )
      PARAMETER (jdim = (3*nwave+2)/2 )
      PARAMETER (ijdim = idim*jdim)

C     Interpolation parameters
      INTEGER tlat, tlong
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm +1)

      REAL field(tlong+1, tlat/2+1), fac(mwave/2)
      INTEGER pole, type, proj
C     Sigma file data
      REAL slp(mwave),   slpf (tlong, tlat)
      REAL ztopo(mwave), ztopof(tlong, tlat)
      REAL temp(mwave),  tempf(tlong, tlat)
      REAL qsig(mwave),  qsigf(tlong, tlat)

C     Flux file data 
      REAL qsen(tlong, tlat), qlat(tlong, tlat)
      REAL lwdn(tlong, tlat), swdn(tlong, tlat)
      REAL uten(tlong, tlat), vten(tlong, tlat)
      REAL t2(tlong, tlat)  , q2(tlong, tlat)
      REAL mask(tlong, tlat), cloud(tlong, tlat)

C     Arguments to return
      REAL u850(L,M), v850(L,M), rh850(0:L, 0:M)
      REAL ps(0:L, 0:M), acl(0:L, 0:M), precip(0:L, 0:M)
      REAL t850(0:L, 0:M), lwic(0:L, 0:M), swic(0:L, 0:M)

C=====================================================================--
C     Local variables. 
      INTEGER i, j, iunit
      REAL tempor(tlong, tlat)
      DOUBLE PRECISION W3FA09
      REAL tempa(0:L, 0:M), tempb(0:L, 0:M)

C=====================================================================--
C     Begin the routine 
C     Read in data here

      iunit = 31
      REWIND (31)
      CALL getsig(slp, temp, ztopo, qsig, iunit, siglev)
      iunit = 50+step
      CALL getflx(swdn, lwdn, t2, q2, uten, vten, 
     1            qsen, qlat, cloud, mask, iunit)

C     Convert the sigma files to lat-long grid
      type = -101
      CALL SPHERT(type, slpf, slp, 0, fac, tlong, tlat, nwave, 0)
      CALL SPHERT(type, tempf, temp, 0, fac, tlong, tlat, nwave, 0)
      CALL SPHERT(type, qsigf, qsig, 0, fac, tlong, tlat, nwave, 0)

C     Zero out the temporary data file used for hemispheric values
      DO 100 j = 1, tlat
        DO 110 i = 1, tlong
          field(i,j/2+1) = 0.0
  110   CONTINUE
  100 CONTINUE

C     Select base map projection
      proj = 202

C     Select the data field
      DO 1000 j = 1, tlat
        DO 1100 i = 1, tlong
          tempor(i,j) = 10.*EXP(slpf(i,j))
 1100   CONTINUE
 1000 CONTINUE
      CALL terp(tempor, ps, pole, proj)
 
      CALL terp(tempf, t850, pole, proj)

      CALL terp(swdn, swic, pole, proj)

      CALL terp(lwdn, lwic, pole, proj)
      
  
      CALL terp(q2, rh850, pole, proj)
      DO 1200 j = 1, M
        DO 1300 i = 1, L
          rh850(i,j) = rh850(i,j)/1.E3*ps(i,j)*EPSI*100.
     1                          /W3FA09(t850(i,j))
 1300   CONTINUE
 1200 CONTINUE

      CALL terp(cloud, acl, pole, proj)

      CALL terp(uten, tempa, pole, proj)

      CALL terp(vten, tempb, pole, proj)

C     Average u, v onto the half-grid points.
      DO 2000 j = 1, M
        DO 2100 i = 1, L
          u850(i,j) = 0.25*(tempa(i,j)+tempa(i-1,j)+tempa(i,j-1)+
     1                      tempa(i-1,j-1)   )
          v850(i,j) = 0.25*(tempb(i,j)+tempb(i-1,j)+tempb(i,j-1)+
     1                      tempb(i-1,j-1)   )
 2100   CONTINUE
 2000 CONTINUE

      DO 3000 j = 0, M
        DO 3100 i = 0, L
          precip(i,j) = 0.0
 3100   CONTINUE
 3000 CONTINUE

      RETURN
      END
      SUBROUTINE twoget(pole, siglev, u850, v850, t850, rh850,
     1   ps, precip, cloud, lwic, swic, step)
C=====================================================================--
C  Programmed by:
C     Robert W. Grumbine          NMC  Camp Springs, MD         Dec. '92
C  Purpose:
C     Get data from MRF forecasts for ice model, in the case that the
C       ice model grid is a 2.5*5.0 lat-long spherical grid.  (PTYPE=2)
C  EXTERNALS:
C     getsig  -  Get sigma file data from MRF
C     getflx  -  Get flux file data from MRF
C     SPHERT  -  Convert sigma file data to regular spherical grid
C=====================================================================--
      IMPLICIT none
      INCLUDE "mgrid.inc"
C=====================================================================--
      INTEGER idim, jdim
C     Meteorological Grid parameters.
      PARAMETER (idim = 3*nwave+6     )
      PARAMETER (jdim = (3*nwave+2)/2 )

      INCLUDE "icegrid.inc"
C     Interpolation parameters
      INTEGER tlat, tlong
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm +1)

C     Physical Parameters
      INCLUDE "physical.inc"

      REAL fac(mwave/2)
      INTEGER pole, type
C     Sigma file data
      REAL slp(mwave),   slpf (tlong, tlat)
      REAL ztopo(mwave), ztopof(tlong, tlat)
      REAL temp(mwave),  tempf(tlong, tlat)
      REAL qsig(mwave),  qsigf(tlong, tlat)
      REAL siglev

C     Flux file data 
      REAL qsen(tlong, tlat), qlat(tlong, tlat)
      REAL lwdn(tlong, tlat), swdn(tlong, tlat)
      REAL uten(tlong, tlat), vten(tlong, tlat)
      REAL t2(tlong, tlat)  , q2(tlong, tlat)
      REAL mask(tlong, tlat), clouda(tlong, tlat)

C     Arguments to return
      REAL u850(L, M), v850(L, M)
      REAL rh850(0:L, 0:M), ps(0:L, 0:M)
      REAL t850(0:L, 0:M), precip(0:L, 0:M), cloud(0:L, 0:M)
      REAL lwic(0:L, 0:M), swic(0:L, 0:M)
      INTEGER step
C=====================================================================--

C     Local variables. 
      INTEGER i, j, iunit
      REAL tempor(tlong, tlat)
      REAL*8 W3FA09
C=====================================================================--

C     For conversion between spherical meteo grid coords and
C       spherical ice coords
      INTEGER imet, jmet, im, jm
      REAL longm, latm
      imet(longm) = 1 + INT(longm/dlonm)
      jmet(latm)  = 1 + INT((90.-latm)/dlatm)

C=====================================================================--
C     Begin the routine 

      iunit = 31
      REWIND (31)
      CALL getsig(slp, temp, ztopo, qsig, iunit, siglev)
      iunit = 50+step
      CALL getflx(swdn, lwdn, t2, q2, uten, vten, 
     1            qsen, qlat, clouda, mask, iunit)

C     Convert the sigma files to lat-long grid
      type = -101
      CALL SPHERT(type, slpf, slp, 0, fac, tlong, tlat, nwave, 0)
      CALL SPHERT(type, tempf, temp, 0, fac, tlong, tlat, nwave, 0)
      CALL SPHERT(type, qsigf, qsig, 0, fac, tlong, tlat, nwave, 0)

C     Transfer the data to the stossel grid
      DO 1000 j = 1, tlat
        DO 1100 i = 1, tlong
          tempor(i,j) = 1.E3*EXP(slpf(i,j))
 1100   CONTINUE
 1000 CONTINUE

      DO 2000 j = 0, MM
        DO 2100 i = 2, LM
          latm = latmin + (j-0.5)*dlat
          longm = -10.+i*dlon
          im = imet(longm)
          jm = jmet(latm)
 
          precip(i,j) = 0.0
          ps(i,j) = (tempor(im, jm)+tempor(im,jm+1))/2.
          t850(i,j) = (tempf(im, jm)+tempf(im,jm+1))/2.
          cloud(i,j) = (clouda(im, jm)+clouda(im,jm+1))/2.
          lwic(i,j)  = (lwdn(im, jm) + lwdn(im,jm+1))/2.
          swic(i,j)  = (swdn(im, jm) + swdn(im,jm+1))/2.

          rh850(i,j) = (qsigf(im, jm)+qsigf(im, jm+1))/2./1.E3
     1               * ps(i, j) / 
     2   W3FA09(t850(i,j))   * EPSI * 100.
          IF (rh850(i,j) .LT. 0.) rh850(i,j) = 0.
C         Stossel reads in degrees C rather than K
          t850(i,j) = t850(i,j) - TMELT

 2100   CONTINUE
 2000 CONTINUE
 9001 FORMAT (4F12.5)

      DO 2200 j = 1, MM
        DO 2300 i = 2, LM
          latm = latmin + (j-0.5)*dlat
          longm = -10.+i*dlon
          im = imet(longm)
          jm = jmet(latm)
          u850(i,j) = (uten(im, jm)+uten(im, jm+1))/2.
          v850(i,j) = (vten(im, jm)+vten(im, jm+1))/2.
 2300   CONTINUE
 2200 CONTINUE

C     Carry out the wrap-around
      DO 3000 j = 1, M
        ps(0,j)   = ps(LM2,j)
        t850(0,j) = t850(LM2,j)
        rh850(0,j) = rh850(LM2,j)
        precip(0,j) = precip(LM2,j)
        cloud(0,j) = cloud(LM2,j)

        ps(1,j)   = ps(LM,j)
        t850(1,j) = t850(LM,j)
        rh850(1,j) = rh850(LM,j)
        precip(1,j) = precip(LM,j)
        cloud(1,j) = cloud(LM,j)
        u850(1,j) = u850(LM,j)
        v850(1,j) = v850(LM,j)

        ps(L,j)     = ps(2,j)
        t850(L,j)   = t850(2,j)
        rh850(L,j)  = rh850(2,j)
        precip(L,j) = precip(2,j)
        cloud(L,j)  = cloud(2,j)
        u850(L,j)   = u850(2,j)
        v850(L,j)   = v850(2,j)
 3000 CONTINUE

      DO 30 J=1,M
       U850(1,J)=U850(LM,J)
       V850(1,J)=V850(LM,J)
       U850(L,J)=U850(2 ,J)
       V850(L,J)=V850(2 ,J)
   30 CONTINUE
      DO 31 I=1,L
       U850(I,M)=U850(I,MM)
       V850(I,M)=V850(I,MM)
   31 CONTINUE
C-----------------------------------------------------------------------
      DO 12 I=0,L
       T850(I,MM)=T850(I,MM2)
       T850(I,0)=T850(I,1)
       T850(I,M)=T850(I,MM)
   12 CONTINUE
C-----------------------------------------------------------------------
      DO 41 I=0,L
       rh850(I,MM)=rh850(I,MM2)
       rh850(I,0)=rh850(I,1)
       rh850(I,M)=rh850(I,MM)
   41 CONTINUE
C-----------------------------------------------------------------------
      DO 13 I=0,L
       ps(I,MM)=ps(I,MM2)
       ps(I,0)=ps(I,1)
       ps(I,M)=ps(I,MM)
   13 CONTINUE
C-----------------------------------------------------------------------
      DO 14 I=0,L
       cloud(I,MM)=cloud(I,MM2)
       cloud(I,0)=cloud(I,1)
       cloud(I,M)=cloud(I,MM)
   14 CONTINUE
C-----------------------------------------------------------------------
      DO 42 I=0,L
       precip(I,MM)=precip(I,MM2)
       precip(I,0)=precip(I,1)
       precip(I,M)=precip(I,MM)
   42 CONTINUE

      RETURN
      END
                SUBROUTINE W3FT32
     &          (FIELD, MAPIN, DATA, MAPOUT, INTERP, IER)
C$$$  SUBROUTINE DOCUMENTATION BLOCK  ***
C
C SUBROUTINE: W3FT32        GENERAL INTERPOLATOR BETWEEN NMC FIELDS
C   AUTHOR: B. CAVANAUGH E     ORG: W324          DATE: 87-07-15
C
C ABSTRACT:  INTERPOLATE SCALAR QUANTITY FROM ANY GIVEN NMC
C   FIELD (IN OFFICE NOTE 84) TO ANY OTHER FIELD. CAN DO BILINEARLY
C   OR BIQUADRATICALLY.  WILL NOT ROTATE WIND COMPONENTS.
C   INPUT AND OUTPUT FIELDS ARE REAL*4 UNPACKED
C
C PROGRAM HISTORY LOG:
C   74-06-15  JOHN STACKPOLE
C   87-07-15  B. CAVANAUGH     ADD GRID TYPE 100, 101 TO TABLES.
C   89-02-02  R.E.JONES        CHANGE TO MICROSOFT FORTRAN 4.10
C   90-09-18  R.E.JONES        CHANGE NAME FROM POLATE TO W3FT32
C
C USAGE:  CALL W3FT32(FIELD, MAPIN, DATA, MAPOUT, INTERP, IER)
C   INPUT ARGUMENTS:
C       FIELD  - REAL*4    - TWO DIMENSIONAL ARRAY.
C       MAPIN  - INTEGER*4 - NMC MAP NUMBER (K) FOR GIVEN INPUT FIELD.
C       MAPOUT - INTEGER*4 - NMC MAP NUMBER (K) FOR WANTED OUTPUT FIELD.
C       INTERP - INTEGER*4 - SET INTERPOLATION METHOD:
C                  EQ 1 - LINEAR
C                  NE 1 - BIQUADRATIC
C   INPUT FILES:  NONE
C
C   OUTPUT ARGUMENTS:
C        DATA - REAL*4 - ARRAY TO HOLD OUTPUT MAP (UNPACKED).
C         IER - INTEGER*4 - COMPLETION CONDITION FLAG
C
C   OUTPUT FILES: NONE
C
C
C   RETURN CONDITIONS:
C     IER = 0  -  NO DIFFICULTIES
C           1  -  MAPIN NOT RECOGNIZED
C           2  -  MAPOUT NOT RECOGNIZED
C           3  -  PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           4  -  PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           5  -  PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           6  -  PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           7  -  PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           8  -  PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           THESE FLAGS ARE SET AT VARIOUS TEST LOCATIONS
C           PLEASE REFER TO THE CODE LISTING FOR DETAILS
C
C   SUBPROGRAMS CALLED:
C     UNIQUE :  NONE
C
C     LIBRARY:  W3FB01, W3FB02, W3FB03, W3FB04, W3FT00, W3FT01
C
C ATTRIBUTES:
C   LANGUAGE: SVS 386 FORTRAN 2.8a
C   MACHINE:  IBM PC, AT, PS/2, 386, 486, CLONES.
C
C   INFORMATION:  SEE COMMENT CARDS FOLLOWING FOR MORE DETAIL
C                 INCLUDING RECIPES FOR ADDING MORE INPUT AND
C                 OUTPUT MAPS AS THE NEED ARISES.
C$$$
C
C     INTERPOLATE INFORMATION FROM FIELD (MAP TYPE K = MAPIN)
C     TO DATA  (MAP TYPE K = MAPOUT)
C        INTERP SETS INTERPOLATION METHOD
C               = 1 BILINEAR, OTHERWISE BIQUADRATIC
C
      REAL      DATA(*), FIELD(*)
C
C     RESTRICTION AND RULES:
C
C          AT PRESENT W3FT32 WILL ACCEPT  ONLY THE FOLLOWING TYPES
C          POLAR STEREOGRAPHIC
C          K = 5 & 26 (LFM ANL & FCST RESPECTIVELY)
C              27 & 28  (65X65)
C              25 (53X57 SOUTHERN HEMISPHERE)
C              49 (129X129 NH; 190.5 KM)
C              50 (129X129 SH; 190.5 KM)
C              55 (87X71 NH; LFM ORIENT; 254 KM)
C              56 (87X71 NA; LFM ORIENT; 174 KM)
C              60 (57X57 ENLARGED LFM 'VLFM')
C             100 (83X83 NGM C-GRID; 91.452)
C             101 (113X91 NGM BIG C-GRID; 91.452)
C
C          LONGITUDE/LATITUDE: ('LOLA')
C          K =  29 & 30  (145X37)
C               33 & 34  (181X46)
C               35 & 36  (361X91)  ! Added by BG 11/3/92
C               45 & 46  (97X25 - 3.75 DEG LOLA)
C               21 & 22 (73X19 - 5 DEG LOLA)
C
C     WILL OUTPUT:
C     POLAR STEREO:
C          K =  5  (53X57)  LFM
C               25  (53X57  SOUTH HEMISPHERE)
C               26  (53X45)  LFM
C               27 & 28  (65X65)
C               49  (129X129 NH POLA) (1/2 BEDIENT MESH;ORIENTED 80W)
C               50  (129X129 SH POLA) (1/2 BEDIENT MESH;ORINETED 80W)
C               51  (129X129 NH POLA) (SAME MESHL; ORIENTED AT 105W)
C               55 (NH 87X71 254 KM, LFM ORIENT)
C               56 (NA 87X71 127 KM, LFM ORIENT)
C               60 (57X57 ENLARGED LFM 'VLFM')
C              100 (83X83 NGM C-GRID)
C              101 (113X91 NGM BIG C-GRID)
C         20x series added by BG
C              201 (129X129 NH POLA) (1/2 BEDIENT MESH; ORIENTED 80 W)
C              202 (129X129 SH POLA) (1/2 BEDIENT MESH; ORIENTED 80 W)
C              203 (193X193 NH POLA) (1/3 BEDIENT MESH; ORIENTED 80 W)
C              204 (193X193 SH POLA) (1/3 BEDIENT MESH; ORIENTED 80 W)
C              205 (961X961 NH POLA) (1/15 BEDIENT MESH; ORIENTED 80 W)
C              206 (961X961 SH POLA) (1/15 BEDIENT MESH; ORIENTED 80 W)
C              400 (39X39 1:40MIL 80 DEG VERTICAL POLA)
C              401 (25X35 1:20MIL U.S. SECTION ROTATED)
C              402 (97X97 1-20MIL N.H. POLA ROTATED TO 105W VERT)
C              403 (97X97 1-20MIL S.H. POLA UNROTATED 80W TOP VERT)
C     LOLA:
C          K =  29 & 30  (145X37)
C               33 & 34  (181X46)
C               35 & 36  (361X91)  !1deg lola BG
C               45 & 46  (97X25 - 3.75 DEG LOLA)
C               500 & 501 US SECTIONAL NEP 36 & 45
C
C     FEEL FREE, GENTLE READER, TO AUGMENT THE LIST AS YOU WISH
C     AND HERE IS A RECIPE FOR ADDING A  NEW OUTPUT GRID
C     (POLA IN THIS CASE, BUT I AM SURE YOU CAN DRAW THE ANALOGY)
C     STEP1
C          PUT NEW NUMBER IN COMMENT ABOVE
C     STEP 2
C          ADD IT TO MAPOUT LIST NEAR STMT 30
C     STEP 3
C          ADD SET OF PARAMETERS AT STMT 2000 (FOR POLA)
C     STEP4
C          ADD SET OF PARAMETERS AT STMT 6000 (FOR POLA)
C
C     HERE TOO IS A RECIPE FOR ADDING A NEW (POLA) INPUT GRID
C
C     STEP 1:
C          PUT NEW NUMBER IN COMMENT ABOVE
C     STEP2:
C          ADD NUMBER TO IF(MAPIN.. ) TEST BELOW
C     STEP 3:
C          ADD INPUT MAP CHARACTERISTICS AT STMT 1000
C     STEP 4:
C          DITTO AT STMT 3000
C
      LOGICAL LOLAIN, POLAIN, LOLAOU, POLAOU
C     Variable xmesho added by BG for polar to polar conversions
C              plength added by BG for polar ice grid sizing
      REAL xmesho, plength, lenl, lenr, lenu, lend
      REAL lenls, lenrs, lenus, lends
C
C     BEGIN HERE  -  SET ERROR RETURN TO O.K.
C
      IER = 0
C
C     Set up the polar grid.  Adapt first for NH
      lenl = 36.*127.
      lenr = 31.*127.
      lend = 43.*127.
      lenu = 32.*127.

C     Set up for SH
      lenls = 27.*127.
      lenrs = 28.*127.
      lends = 25.*127.
      lenus = 34.*127.

      plength = 12.2E3 - 28.*127. 
C     DETERMINE WHETHER INPUT GRID  IS LOLA OR POLA
C
C     THIS LIST CAN BE AUGMENTED  ONLY AT THE COST OF A LOT OF
C     WORK ELSEWHERE IN THE PROGRAM
C     HAVE AT IT IF YOU WANT OTHER MAPS
C
C        POLA MAPS
C
      IF (MAPIN.EQ. 5)  GO TO 10
      IF (MAPIN.EQ.25)  GO TO 10
      IF (MAPIN.EQ.26)  GO TO 10
      IF (MAPIN.EQ.27)  GO TO 10
      IF (MAPIN.EQ.28)  GO TO 10
      IF (MAPIN.EQ.49)  GO TO 10
      IF (MAPIN.EQ.50)  GO TO 10
      IF (MAPIN.EQ.51)  GO TO 10
      IF (MAPIN.EQ.55)  GO TO 10
      IF (MAPIN.EQ.56)  GO TO 10
      IF (MAPIN.EQ.60)  GO TO 10
      IF (MAPIN.EQ.100) GO TO 10
      IF (MAPIN.EQ.101) GO TO 10
      IF (MAPIN.EQ.201) GO TO 10
      IF (MAPIN.EQ.202) GO TO 10
      IF (MAPIN.EQ.203) GO TO 10
      IF (MAPIN.EQ.204) GO TO 10
      IF (MAPIN.EQ.205) GO TO 10
      IF (MAPIN.EQ.206) GO TO 10
C
C        LOLA MAPS
C
      IF (MAPIN.EQ.21)  GO TO 20
      IF (MAPIN.EQ.22)  GO TO 20
      IF (MAPIN.EQ.29)  GO TO 20
      IF (MAPIN.EQ.30)  GO TO 20
      IF (MAPIN.EQ.33)  GO TO 20
      IF (MAPIN.EQ.34)  GO TO 20
      IF (MAPIN.EQ.35)  GO TO 20  !Added by BG
      IF (MAPIN.EQ.36)  GO TO 20  !Added by BG
      IF (MAPIN.EQ.45)  GO TO 20
      IF (MAPIN.EQ.46)  GO TO 20
C
C     IF NO MATCH - ERROR
C
      IER = 1
      RETURN
C
C     SET LOGICAL FLAGS
C
   10 LOLAIN = .FALSE.
      POLAIN = .TRUE.
      GO TO 30
C
   20 LOLAIN = .TRUE.
      POLAIN = .FALSE.
C
C     DITTO FOR OUTPUT MAP TYPE
C
C        POLA MAPS
C
   30 CONTINUE
      IF (MAPOUT.EQ. 5)  GO TO 40
      IF (MAPOUT.EQ.25)  GO TO 40
      IF (MAPOUT.EQ.26)  GO TO 40
      IF (MAPOUT.EQ.27)  GO TO 40
      IF (MAPOUT.EQ.28)  GO TO 40
      IF (MAPOUT.EQ.49)  GO TO 40
      IF (MAPOUT.EQ.50)  GO TO 40
      IF (MAPOUT.EQ.51)  GO TO 40
      IF (MAPOUT.EQ.55)  GO TO 40
      IF (MAPOUT.EQ.56)  GO TO 40
      IF (MAPOUT.EQ.60)  GO TO 40
      IF (MAPOUT.EQ.100) GO TO 40
      IF (MAPOUT.EQ.101) GO TO 40
      IF (MAPOUT.EQ.201) GO TO 40  !20x series added by Bob Grumbine
      IF (MAPOUT.EQ.202) GO TO 40
      IF (MAPOUT.EQ.203) GO TO 40
      IF (MAPOUT.EQ.204) GO TO 40
      IF (MAPOUT.EQ.205) GO TO 40
      IF (MAPOUT.EQ.206) GO TO 40
      IF (MAPOUT.EQ.400) GO TO 40
      IF (MAPOUT.EQ.401) GO TO 40
      IF (MAPOUT.EQ.402) GO TO 40
      IF (MAPOUT.EQ.403) GO TO 40
C
C        LOLA MAPS
C
      IF (MAPOUT.EQ.21)  GO TO 50
      IF (MAPOUT.EQ.22)  GO TO 50
      IF (MAPOUT.EQ.29)  GO TO 50
      IF (MAPOUT.EQ.30)  GO TO 50
      IF (MAPOUT.EQ.33)  GO TO 50
      IF (MAPOUT.EQ.34)  GO TO 50
      IF (MAPOUT.EQ.35)  GO TO 50
      IF (MAPOUT.EQ.36)  GO TO 50
      IF (MAPOUT.EQ.45)  GO TO 50
      IF (MAPOUT.EQ.46)  GO TO 50
      IF (MAPOUT.EQ.500) GO TO 50
      IF (MAPOUT.EQ.501) GO TO 50
C
C     NO MATCH - ERROR
C
      IER = 2
      RETURN
C
C     SET LOGICAL FLAGS
C
   40 LOLAOU = .FALSE.
      POLAOU = .TRUE.
      GO TO 60
C
   50 LOLAOU = .TRUE.
      POLAOU = .FALSE.
C
C     GO TO DIFFERENT SECTIONS FOR IN/OUT OPTIONS
C
   60 IF (POLAIN)  GO TO 1000
      IF (LOLAIN)  GO TO 5000
C
C     ##################################################################
C     ##################################################################
C
C     THIS SECTION FOR POLAR STEREOGRAPHIC INPUT MAPS
C
C     SUBDIVIDED FOR OUTPUT TYPE
C
 1000 IF (LOLAOU)  GO TO 3000
C
C     POLAR STEREO TO POLAR STEREO
C        USE HOWCROFTS FIELD TRANSFORMER
C     ORIENT IS DEGREES OF ROTATION FROM NMC STANDARD
C      (80 DEG CENTER VERTIVAL) TO INPUT GRID (POSITIVE ANTICLOCKWISE)
C
      IF (MAPIN.EQ. 5)  GO TO 1005
      IF (MAPIN.EQ.25)  GO TO 1025
      IF (MAPIN.EQ.26)  GO TO 1026
      IF (MAPIN.EQ.27)  GO TO 1027
      IF (MAPIN.EQ.28)  GO TO 1027
      IF (MAPIN.EQ.49)  GO TO 1049
      IF (MAPIN.EQ.50)  GO TO 1049
      IF (MAPIN.EQ.51)  GO TO 1051
      IF (MAPIN.EQ.55)  GO TO 1055
      IF (MAPIN.EQ.56)  GO TO 1056
      IF (MAPIN.EQ.60)  GO TO 1060
      IF (MAPIN.EQ.100) GO TO 1100
      IF (MAPIN.EQ.101) GO TO 1101
      IF (MAPIN.EQ.201) GO TO 1201  ! 20x series added by Bob Grumbine
      IF (MAPIN.EQ.202) GO TO 1202
      IF (MAPIN.EQ.203) GO TO 1203
      IF (MAPIN.EQ.204) GO TO 1204
      IF (MAPIN.EQ.205) GO TO 1205
      IF (MAPIN.EQ.206) GO TO 1206
      IER = 1
      RETURN
C
 1005 IMAXIN =53
      JMAXIN = 57
      COMIIN = 27.
      COMJIN = 49.
      ORIENT = -25.
      XMESH  = 190.5
      GO TO 2000
C
 1025 IMAXIN = 53
      JMAXIN = 57
      COMIIN = 27.
      COMJIN = 29.
      ORIENT = 0.
      XMESH  = 381.
      GO TO 2000
C
 1026 IMAXIN = 53
      JMAXIN = 45
      COMIIN = 27.
      COMJIN = 49.
      ORIENT = -25.
      XMESH  = 190.5
      GO TO 2000
C
 1027 IMAXIN = 65
      JMAXIN = 65
      COMIIN = 33.
      COMJIN = 33.
      ORIENT = 0.
      XMESH  = 381.
      GO TO 2000
C
 1049 IMAXIN = 129
      JMAXIN = 129
      COMIIN = 65.
      COMJIN = 65.
      ORIENT = 0.
      XMESH  = 190.5
      GOTO 2000
C
 1051 IMAXIN = 129
      JMAXIN = 129
      COMIIN = 65.
      COMJIN = 65.
      ORIENT = -25.
      XMESH  = 190.5
      GOTO 2000
C
 1055 IMAXIN = 87
      JMAXIN = 71
      COMIIN = 44.
      COMJIN = 38.
      ORIENT = -25.
      XMESH  = 254.
      GOTO 2000
C
 1056 IMAXIN = 87
      JMAXIN = 71
      COMIIN = 40.
      COMJIN = 73.
      ORIENT = -25.
      XMESH  = 127.
      GOTO 2000
C
 1060 IMAXIN=  57
      JMAXIN = 57
      COMIIN = 29.
      COMJIN = 49.
      ORIENT = -25.
      XMESH  = 190.5
      GO TO 2000
C
 1100 IMAXIN = 83
      JMAXIN = 83
      COMIIN = 40.5
      COMJIN = 88.5
      ORIENT = -25.
      XMESH  = 91.452
      GO TO 2000
C
 1101 IMAXIN = 113
      JMAXIN = 91
      COMIIN = 58.5
      COMJIN = 92.5
      ORIENT = -25.
      XMESH  = 91.452
      GO TO 2000
C
 1201 CONTINUE
      XMESH  = 381./2.
      IMAXIN = INT((lenl+lenr)/XMESH)+1
      JMAXIN = INT((lenu+lend)/XMESH)+1
      COMIIN = FLOAT ( INT(lenl/XMESH +1) )
      COMJIN = FLOAT ( INT(lend/XMESH +1) )
      ORIENT = 0.
      GO TO 2000
C
 1202 CONTINUE
      XMESH  = 381./2.
      IMAXIN = INT(plength/XMESH)+1
      JMAXIN = INT(plength/XMESH)+1
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = -90.
      GO TO 2000
C
 1203 CONTINUE
      XMESH  = 381./3.
      IMAXIN = INT(plength/XMESH)+1
      JMAXIN = INT(plength/XMESH)+1
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = 0.
      GO TO 2000
C
 1204 CONTINUE
      XMESH  = 381./3.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = -90.
      GO TO 2000
C
 1205 CONTINUE
      XMESH  = 381./15.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = 0.
      GO TO 2000
C
 1206 CONTINUE
      XMESH  = 381./15.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = -90.
      GO TO 2000
C
C
C     SELECT I, J, DILATION, ROTATION, AND COMMON POINT (POLE) OUTPUT
C     DILATE = XMESHOUT / XMESHIN
C
 2000 IF (MAPOUT.EQ. 5)  GO TO 2005
      IF (MAPOUT.EQ.25)  GO TO 2025
      IF (MAPOUT.EQ.26)  GO TO 2026
      IF (MAPOUT.EQ.27)  GO TO 2027
      IF (MAPOUT.EQ.28)  GO TO 2027
      IF (MAPOUT.EQ.49)  GO TO 2049
      IF (MAPOUT.EQ.50)  GO TO 2049
      IF (MAPOUT.EQ.51)  GO TO 2051
      IF (MAPOUT.EQ.55)  GO TO 2055
      IF (MAPOUT.EQ.56)  GO TO 2056
      IF (MAPOUT.EQ.60)  GO TO 2060
      IF (MAPOUT.EQ.100) GO TO 2100
      IF (MAPOUT.EQ.101) GO TO 2101
      IF (MAPOUT.EQ.201) GO TO 2201  !20x series added by Bob Grumbine
      IF (MAPOUT.EQ.202) GO TO 2202
      IF (MAPOUT.EQ.203) GO TO 2203
      IF (MAPOUT.EQ.204) GO TO 2204
      IF (MAPOUT.EQ.205) GO TO 2205
      IF (MAPOUT.EQ.206) GO TO 2206
      IF (MAPOUT.EQ.400) GO TO 2400
      IF (MAPOUT.EQ.401) GO TO 2401
      IF (MAPOUT.EQ.402) GO TO 2402
      IF (MAPOUT.EQ.403) GO TO 2403
      IER = 3
      RETURN
C
 2005 IMAXOU = 53
      JMAXOU = 57
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 27.
      COMJOU = 49.
      GO TO  2700
C
 2025 IMAXOU = 53
      JMAXOU = 57
      DILAT  = 381./XMESH
      ROT    = 0. - ORIENT
      COMIOU = 27.
      COMJOU = 29.
      GO TO 2700
C
 2026 IMAXOU = 53
      JMAXOU = 45
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 27.
      COMJOU = 49.
      GO TO 2700
C
 2027 IMAXOU = 65
      JMAXOU = 65
      DILAT  = 381./XMESH
      ROT    = 0. - ORIENT
      COMIOU = 33.
      COMJOU = 33.
      GO TO 2700
C
 2049 IMAXOU = 129
      JMAXOU = 129
      DILAT  = 190.5/XMESH
      ROT    = 0. - ORIENT
      COMIOU = 65.
      COMJOU = 65.
      GOTO 2700
C
 2051 IMAXOU = 129
      JMAXOU = 129
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 65.
      COMJOU = 65.
      GOTO 2700
C
 2055 IMAXOU = 87
      JMAXOU = 71
      DILAT  = 254./XMESH
      ROT    = -25. - ORIENT
      COMIOU = 44.
      COMJOU = 38.
      GOTO 2700
C
 2056 IMAXOU = 87
      JMAXOU = 71
      DILAT  = 127./XMESH
      ROT    = -25. - ORIENT
      COMIOU = 40.
      COMJOU = 73.
      GOTO 2700
C
 2060 IMAXOU = 57
      JMAXOU = 57
      DILAT  = 190.5/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 29.
      COMJOU = 49.
      GO TO  2700
C
 2100 IMAXOU = 83
      JMAXOU = 83
      DILAT  = 91.452/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 40.5
      COMJOU = 88.5
      GO TO  2700
C
 2101 IMAXOU = 113
      JMAXOU = 91
      DILAT  = 91.452/XMESH
      ROT    = -25. - ORIENT
      COMIOU = 58.5
      COMJOU = 92.5
      GO TO  2700
C
 2201 CONTINUE 
      XMESHO = 381./2.
      IMAXIN = INT((lenl+lenr)/XMESH) + 1
      JMAXIN = INT((lenu+lend)/XMESH) + 1
      DILAT  = XMESHO/XMESH
      ROT    = -25. - ORIENT
      COMIIN = FLOAT ( INT(lenl/XMESH +1) )
      COMJIN = FLOAT ( INT(lend/XMESH +1) )
      GO TO  2700
C
 2202 CONTINUE 
      XMESHO = 381./2.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      DILAT  = XMESHO/XMESH
      ROT    = -25. - ORIENT
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      GO TO  2700
C
 2203 CONTINUE 
      XMESHO = 381./3.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      DILAT  = XMESHO/XMESH
      ROT    = -25. - ORIENT
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      GO TO  2700
C
 2204 CONTINUE 
      XMESHO = 381./3.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      DILAT  = XMESHO/XMESH
      ROT    = -25. - ORIENT
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      GO TO  2700
C
 2205 CONTINUE 
      XMESHO = 381./15.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      DILAT  = XMESHO/XMESH
      ROT    = -25. - ORIENT
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      GO TO  2700
C
 2206 CONTINUE 
      XMESHO = 381./15.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      DILAT  = XMESHO/XMESH
      ROT    = -25. - ORIENT
      COMIIN = FLOAT ( INT(IMAXIN/2 +1) )
      COMJIN = FLOAT ( INT(JMAXIN/2 +1) )
      GO TO  2700
C
 2400 IMAXOU = 39
      JMAXOU = 39
      DILAT  = 508./ XMESH
      ROT    = 0. - ORIENT
      COMIOU = 20.
      COMJOU = 20.
      GO TO 2700
C
 2401 IMAXOU = 25
      JMAXOU = 35
      DILAT  = 254./XMESH
      ROT    = -25. + 90. - ORIENT
      COMIOU =31.75
      COMJOU = 18.
      GO TO 2700
C
 2402 IMAXOU = 97
      JMAXOU = 97
      DILAT  = 254./XMESH
      ROT    = -25. - ORIENT
      COMIOU = 49.
      COMJOU = 49.
      GOTO 2700
C
 2403 IMAXOU = 97
      JMAXOU = 97
      DILAT  = 254./XMESH
      ROT    =   0. - ORIENT
      COMIOU = 49.
      COMJOU = 49.
      GOTO 2700
C
 2700   CALL W3FT00
     1     (FIELD, DATA, IMAXIN, JMAXIN, IMAXOU, JMAXOU,
     2                   COMIIN, COMJIN, COMIOU, COMJOU,
     3                   DILAT, ROT, INTERP)
      RETURN
C
C     ##################################################################
C
C     HERE FOR POLAR STEREO TO LO/LA
C
 3000 IF (MAPIN.EQ. 5)  GO TO 3005
      IF (MAPIN.EQ.25)  GO TO 3025
      IF (MAPIN.EQ.26)  GO TO 3026
      IF (MAPIN.EQ.27)  GO TO 3027
      IF (MAPIN.EQ.28)  GO TO 3027
      IF (MAPIN.EQ.49)  GO TO 3049
      IF (MAPIN.EQ.50)  GO TO 3049
      IF (MAPIN.EQ.51)  GO TO 3051
      IF (MAPIN.EQ.55)  GO TO 3055
      IF (MAPIN.EQ.56)  GO TO 3056
      IF (MAPIN.EQ.60)  GO TO 3060
      IF (MAPIN.EQ.100) GO TO 3100
      IF (MAPIN.EQ.101) GO TO 3101
      IF (MAPIN.EQ.201) GO TO 3201
      IF (MAPIN.EQ.202) GO TO 3202
      IF (MAPIN.EQ.203) GO TO 3203
      IF (MAPIN.EQ.204) GO TO 3204
      IF (MAPIN.EQ.205) GO TO 3205
      IF (MAPIN.EQ.206) GO TO 3206
C
 3005 XMESH  = 190.5
      IMAXIN = 53
      JMAXIN = 57
      NTHSTH = 1
      POLEI  = 27.
      POLEJ  = 49.
      ORIENT = 105.
      GO TO 4000
C
 3025 XMESH  = 381.
      IMAXIN = 53
      JMAXIN = 57
      NTHSTH = 2
      POLEI  = 27.
      POLEJ  = 29.
      GO TO 4000
C
 3026 XMESH  = 190.5
      IMAXIN = 53
      JMAXIN = 45
      NTHSTH = 1
      POLEI  = 27.
      POLEJ  = 49.
      ORIENT = 105.
      GO TO 4000
C
 3027 XMESH  = 381.
      IMAXIN = 65
      JMAXIN = 65
      NTHSTH = 1
      IF (MAPIN.EQ.28)  NTHSTH = 2
      POLEI  = 33.
      POLEJ  = 33.
      ORIENT = 80.
      GO TO 4000
C
 3049 XMESH  = 190.5
      IMAXIN = 129
      JMAXIN = 129
      NTHSTH = 1
      IF (MAPIN.EQ.50) NTHSTH=2
      POLEI  = 65.
      POLEJ  = 65.
      ORIENT = 80.
      GOTO 4000
C
 3051 XMESH  = 190.5
      IMAXIN = 129
      JMAXIN = 129
      NTHSTH = 1
      POLEI  = 65.
      POLEJ  = 65.
      ORIENT = 105.
      GOTO 4000
C
 3055 XMESH  = 254.
      IMAXIN = 87
      JMAXIN = 71
      NTHSTH = 1
      POLEI  = 44.
      POLEJ  = 38.
      ORIENT = 105.
      GOTO 4000
C
 3056 XMESH  = 127.
      IMAXIN = 87
      JMAXIN = 71
      NTHSTH = 1
      POLEI  = 40.
      POLEJ  = 73.
      ORIENT = 105.
      GOTO 4000
C
 3060 XMESH  = 190.5
      IMAXIN = 57
      JMAXIN = 57
      NTHSTH = 1
      POLEI  = 29.
      POLEJ  = 49.
      ORIENT = 105.
      GO TO 4000
C
 3100 XMESH  = 91.452
      IMAXIN = 83
      JMAXIN = 83
      NTHSTH = 1
      POLEI  = 40.5
      POLEJ  = 88.5
      ORIENT = 105.
      GO TO 4000
C
 3101 XMESH  = 91.452
      IMAXIN = 113
      JMAXIN = 91
      NTHSTH = 1
      POLEI  = 58.5
      POLEJ  = 92.5
      ORIENT = 105.
      GO TO 4000
C
 3201 CONTINUE
      XMESH  = 381./2.
      IMAXIN = INT((lenl+lenr)/XMESH) + 1
      JMAXIN = INT((lenu+lend)/XMESH) + 1
      NTHSTH = 1
      POLEI  = FLOAT ( INT(lenl/XMESH +1) )
      POLEJ  = FLOAT ( INT(lend/XMESH +1) )
      ORIENT = 105.
      GO TO 4000
C
 3202 CONTINUE
      XMESH  = 381./2.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      NTHSTH = 2
      POLEI  = FLOAT ( INT(IMAXIN/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = 105.
      GO TO 4000
C
 3203 CONTINUE
      XMESH  = 381./3.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      NTHSTH = 1
      POLEI  = FLOAT ( INT(IMAXIN/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = 105.
      GO TO 4000
C
 3204 CONTINUE
      XMESH  = 381./3.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      NTHSTH = 2
      POLEI  = FLOAT ( INT(IMAXIN/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = 105.
      GO TO 4000
C
 3205 CONTINUE
      XMESH  = 381./15.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      NTHSTH = 1
      POLEI  = FLOAT ( INT(IMAXIN/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = 105.
      GO TO 4000
C
 3206 CONTINUE
      XMESH  = 381./15.
      IMAXIN = INT(plength/XMESH) + 1
      JMAXIN = INT(plength/XMESH) + 1
      NTHSTH = 2
      POLEI  = FLOAT ( INT(IMAXIN/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXIN/2 +1) )
      ORIENT = 105.
      GO TO 4000
C
C     SELECT OUTPUT LO/LA     VARIATIONS
C
 4000 IF (MAPOUT.EQ.21)  GO TO 4021
      IF (MAPOUT.EQ.22)  GO TO 4021
      IF (MAPOUT.EQ.29)  GO TO 4029
      IF (MAPOUT.EQ.30)  GO TO 4029
      IF (MAPOUT.EQ.33)  GO TO 4033
      IF (MAPOUT.EQ.34)  GO TO 4033
      IF (MAPOUT.EQ.35)  GO TO 4035
      IF (MAPOUT.EQ.36)  GO TO 4035
      IF (MAPOUT.EQ.45)  GO TO 4045
      IF (MAPOUT.EQ.46)  GO TO 4045
      IF (MAPOUT.EQ.500) GO TO 4500
      IF (MAPOUT.EQ.501) GO TO 4501
      IER = 4
      RETURN
C
 4021 IMINOU = 1
      JMINOU = 1
      IMAXOU = 73
      JMAXOU = 19
      DEG    = 5.0
      GO TO 4700
C
 4029 IMINOU = 1
      IMAXOU = 145
      JMINOU = 1
      JMAXOU = 37
      DEG    = 2.5
      GO TO 4700
C
 4033 IMINOU = 1
      IMAXOU = 181
      JMINOU = 1
      JMAXOU = 46
      DEG    = 2.0
      GO TO 4700
C     Added by Bob Grumbine 11/3/92
 4035 IMINOU = 1
      IMAXOU = 361
      JMINOU = 1
      JMAXOU = 91
      DEG    = 1.0
      GO TO 4700
C
 4045 IMINOU = 1
      IMAXOU = 97
      JMINOU = 1
      JMAXOU = 25
      DEG    = 3.75
      GOTO 4700
C
 4500 IMINOU = 93
      IMAXOU = 117
      JMINOU = 1
      JMAXOU = 37
      DEG    = 2.5
      GO TO 4700
C
 4501 IMINOU = 116
      IMAXOU = 140
      JMINOU = 1
      JMAXOU = 46
      DEG    = 2.0
      GO TO 4700
C
C     FIND INPUT POLA I,J FOR DESIRED LOLA OUTPUT POINTS
C
 4700 IJOUT  = 0
      DO 4740 J = JMINOU, JMAXOU
          XLAT = (J-1) * DEG
          IF (NTHSTH.EQ.2)  XLAT = XLAT - 90.
          DO 4740 I = IMINOU, IMAXOU
              ELON = (I-1) * DEG
              WLON = AMOD(360. - ELON, 360.)
              GO TO (4710, 4720), NTHSTH
 4710         CALL W3FB04(XLAT, WLON, XMESH, ORIENT, XI, XJ)
              GO TO 4730
 4720         CALL W3FB02(XLAT, WLON, XMESH, XI, XJ)
 4730         XIIN = XI + POLEI
              XJIN = XJ + POLEJ
C
C         MACDONALDS SUPER GENERAL INTERPOLATOR
C                  IN WHICH  D = FIELD(XIIN, XJIN)
C
              CALL W3FT01
     1        (XIIN, XJIN,  FIELD, D, IMAXIN, JMAXIN, 0, INTERP)
              IJOUT = IJOUT + 1
              DATA(IJOUT) = D
 4740 CONTINUE
      RETURN
C
C     ##################################################################
C     ##################################################################
C
C     THIS SECTION FOR LOLA INPUT MAP
C
C     SELCT OUTPUT TYPE
C
 5000 IF (LOLAOU)  GO TO 7000
C
C     LOLA TO POLA
C     SELECT INPUT INFO
C     (THIS PATTERN CAN BE USED WITH POLA INPUT, TOO  -  TRY IT
C
      IF (MAPIN.EQ.21)  GO TO 5021
      IF (MAPIN.EQ.22)  GO TO 5021
      IF (MAPIN.EQ.29)  GO TO 5029
      IF (MAPIN.EQ.30)  GO TO 5029
      IF (MAPIN.EQ.33)  GO TO 5033
      IF (MAPIN.EQ.34)  GO TO 5033
      IF (MAPIN.EQ.35)  GO TO 5035
      IF (MAPIN.EQ.36)  GO TO 5035
      IF (MAPIN.EQ.45)  GO TO 5045
      IF (MAPIN.EQ.45)  GO TO 5045
      IF (MAPIN.EQ.46)  GO TO 5045
      IER = 5
      RETURN
C
 5021 IMAXIN = 73
      JMAXIN = 19
      DEG    = 5.0
      NTHSTH = 1
      IF (MAPIN.EQ.22)  NTHSTH = 2
      GO TO 6000
C
 5029 IMAXIN = 145
      JMAXIN = 37
      DEG    = 2.5
      NTHSTH = 1
      IF (MAPIN.EQ.30)  NTHSTH = 2
      GO TO 6000
C
 5033 IMAXIN = 181
      JMAXIN = 46
      DEG    = 2.0
      NTHSTH = 1
      IF (MAPIN.EQ.34)  NTHSTH = 2
      GO TO 6000
C
 5035 CONTINUE
      IMAXIN = 361
      JMAXIN = 91
      DEG    = 1.0
      NTHSTH = 1
      IF (MAPIN.EQ.36)  NTHSTH = 2
      GO TO 6000
C
 5045 IMAXIN = 97
      JMAXIN = 25
      DEG    = 3.75
      NTHSTH = 1
      IF (MAPIN.EQ.46)  NTHSTH = 2
       GOTO 6000
C
C     SELECT OUTPUT POLA VARIETY
C     ROT INDICATES HOW MANY DEGREES THE POLA GRID IS TO BE ROTATED
C     (POSITIVE COUNTER-CLOCKWISE)   FROM THE NMC 'STANDARD'
C     OF 80 DEG WEST AT THE BOTTOM (OR TOP IF SOUTHERN HEMISPHERE)
C
 6000 CONTINUE 
      IF (MAPOUT.EQ. 5)  GO TO 6005
      IF (MAPOUT.EQ.25)  GO TO 6025
      IF (MAPOUT.EQ.26)  GO TO 6026
      IF (MAPOUT.EQ.27)  GO TO 6027
      IF (MAPOUT.EQ.28)  GO TO 6027
      IF (MAPOUT.EQ.49)  GO TO 6049
      IF (MAPOUT.EQ.50)  GO TO 6049
      IF (MAPOUT.EQ.51)  GO TO 6051
      IF (MAPOUT.EQ.55)  GO TO 6055
      IF (MAPOUT.EQ.56)  GO TO 6056
      IF (MAPOUT.EQ.60)  GO TO 6060
      IF (MAPOUT.EQ.100) GO TO 6100
      IF (MAPOUT.EQ.101) GO TO 6101
      IF (MAPOUT.EQ.201) GO TO 6201
      IF (MAPOUT.EQ.202) GO TO 6202
      IF (MAPOUT.EQ.203) GO TO 6203
      IF (MAPOUT.EQ.204) GO TO 6204
      IF (MAPOUT.EQ.205) GO TO 6205
      IF (MAPOUT.EQ.206) GO TO 6206
      IF (MAPOUT.EQ.400) GO TO 6400
      IF (MAPOUT.EQ.401) GO TO 6401
      IF (MAPOUT.EQ.402) GO TO 6402
      IF (MAPOUT.EQ.403) GO TO 6403
      IER = 6
      RETURN
C
 6005 IMAXOU = 53
      JMAXOU = 57
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 27.
      POLEJ  = 49.
      GO TO 6700
C
 6025 IMAXOU = 53
      JMAXOU = 57
      XMESH  = 381.
      ROT    = 0.
      POLEI  = 27.
      POLEJ  = 29.
      GO TO 6700
C
 6026 IMAXOU = 53
      JMAXOU = 45
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 27.
      POLEJ  = 49.
      GO TO 6700
C
 6027 IMAXOU = 65
      JMAXOU = 65
      XMESH  = 381.
      ROT    = 0.
      POLEI  = 33.
      POLEJ  = 33.
      GO TO 6700
C
 6049 IMAXOU = 129
      JMAXOU = 129
      XMESH  = 190.5
      ROT    = 0.
      POLEI  = 65.
      POLEJ  = 65.
      GOTO 6700
C
 6051 IMAXOU = 129
      JMAXOU = 129
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 65.
      POLEJ  = 65.
      GOTO 6700
C
 6055 IMAXOU = 87
      JMAXOU = 71
      XMESH  = 254.
      ROT    = -25.
      POLEI  = 44.
      POLEJ  = 38.
      GOTO 6700
C
 6056 IMAXOU = 87
      JMAXOU = 71
      XMESH  = 127.
      ROT    = -25.
      POLEI  = 40.
      POLEJ  = 73.
      GOTO 6700
C
 6060 IMAXOU = 57
      JMAXOU = 57
      XMESH  = 190.5
      ROT    = -25.
      POLEI  = 29.
      POLEJ  = 49.
      GO TO 6700
C
 6100 IMAXOU = 83
      JMAXOU = 83
      XMESH  = 91.452
      ROT    = -25.
      POLEI  = 40.5
      POLEJ  = 88.5
      GO TO 6700
C
 6101 IMAXOU = 113
      JMAXOU = 91
      XMESH  = 91.452
      ROT    = -25.
      POLEI  = 58.5
      POLEJ  = 92.5
      GO TO 6700
C
 6201 CONTINUE
      XMESH  = 381./2.
      IMAXOU = INT((lenl+lenr)/XMESH) + 1
      JMAXOU = INT((lenu+lend)/XMESH) + 1
      ROT    = 0.
      POLEI  = FLOAT ( INT(lenl/XMESH +1) )
      POLEJ  = FLOAT ( INT(lend/XMESH +1) )
      GO TO 6700
C
 6202 CONTINUE
      XMESH  = 381./2.
      IMAXOU = INT(plength/XMESH) + 1
      JMAXOU = INT(plength/XMESH) + 1
      ROT    = -90.
      POLEI  = FLOAT ( INT(IMAXOU/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXOU/2 +1) )
      GO TO 6700
C
 6203 CONTINUE
      XMESH  = 381./3.
      IMAXOU = INT((lenl+lenr)/XMESH) + 1
      JMAXOU = INT((lenu+lend)/XMESH) + 1
      ROT    = -10.
      POLEI  = FLOAT ( INT(lenl/XMESH +1) )
      POLEJ  = FLOAT ( INT(lend/XMESH +1) )
      GO TO 6700
C
 6204 CONTINUE
      XMESH  = 381./3.
      IMAXOU = INT((lenls+lenrs)/XMESH) + 1
      JMAXOU = INT((lenus+lends)/XMESH) + 1
      ROT    = -90.
      POLEI  = ( (lenls/XMESH +1) )
      POLEJ  = ( (lends/XMESH +1) )
      GO TO 6700
C
 6205 CONTINUE
      XMESH  = 381./15.
      IMAXOU = INT(plength/XMESH) + 1
      JMAXOU = INT(plength/XMESH) + 1
      ROT    = 0.
      POLEI  = FLOAT ( INT(IMAXOU/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXOU/2 +1) )
      GO TO 6700
C
 6206 CONTINUE
      XMESH  = 381./15.
      IMAXOU = INT(plength/XMESH) + 1
      JMAXOU = INT(plength/XMESH) + 1
      ROT    = -90.
      POLEI  = FLOAT ( INT(IMAXOU/2 +1) )
      POLEJ  = FLOAT ( INT(JMAXOU/2 +1) )
      GO TO 6700
C
 6400 IMAXOU = 39
      JMAXOU = 39
      XMESH  = 508.
      ROT    = 0.
      POLEI  = 20.
      POLEJ  = 20.
      GO TO 6700
C
C     THIS ONE GETS  SPECIAL TREATMENT BECAUSE WE ARE
C     INTERCHANGING ROWS AND COLUMNS FOR GRIDPRINT AFTER INTERPOLATION
C        (ACTUALLY IT IS DONE ALL AT ONCE)
C
 6401 IMAXOU = 25
      JMAXOU = 35
      XMESH  = 254.
      ROT    = -25.
      POLEI  = 18.
      POLEJ  = 31.75
C
      IJOUT  = 0
      DO 64011 J=1,JMAXOU
          XI = JMAXOU - J + 1
          XXI = XI - POLEI
          DO 64011 I = 1,IMAXOU
              XJ = I
              XXJ = XJ - POLEJ
              CALL W3FB01(XXI, XXJ, XMESH, XLAT, WLON)
              WLON = WLON - ROT
              IF (WLON.GT.360.)  WLON = WLON - 360.
              IF (WLON.LT.0.) WLON = WLON + 360.
              XIIN = (360.-WLON)/DEG + 1.
              XJIN = XLAT/DEG + 1.
              CALL W3FT01
     1         (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
              IJOUT = IJOUT + 1
              DATA(IJOUT) = D
64011 CONTINUE
      RETURN
C
 6402 IMAXOU = 97
      JMAXOU = 97
      XMESH  = 254.
      ROT    = -25.
      POLEI  = 49.
      POLEJ  = 49.
      GOTO 6700
C
 6403 IMAXOU = 97
      JMAXOU = 97
      XMESH  = 254.
      ROT    = 0.
      POLEI  = 49.
      POLEJ  = 49.
      GOTO 6700
C
C     FIND INPUT LOLA I,J FOR DESIRED POLA OUTPUT POINTS
C
 6700 IJOUT = 0
CD      PRINT *,'6700 ',XMESH, IMAXOU, JMAXOU, ROT, POLEI, POLEJ
      DO 6740 J=1,JMAXOU
          XJ    = J - POLEJ
          DO 6740 I=1,IMAXOU
              XI    = I - POLEI
              GOTO (6710, 6720), NTHSTH
 6710         CALL W3FB01(XI, XJ, XMESH, XLAT, WLON)
              WLON  = WLON - ROT
              GO TO 6730
 6720         CALL W3FB03(XI, XJ, XMESH, XLAT, WLON)
              WLON  = WLON + ROT
              XLAT  = XLAT + 90.
 6730         IF (WLON.GT.360.)  WLON = WLON - 360.
              IF (WLON.LT.0.)  WLON = WLON + 360.
              XIIN  = (360.-WLON)/DEG + 1.
              XJIN  = XLAT/DEG + 1.
              CALL W3FT01
     1        (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
              IJOUT = IJOUT + 1
              DATA(IJOUT) = D
 6740 CONTINUE
      RETURN
C
C     ##################################################################
C
C     LOLA TO LOLA
C
C     SELECT INPUT GRID INFO
C
 7000 IF (MAPIN.EQ.21)  GO TO 7021
      IF (MAPIN.EQ.22)  GO TO 7021
      IF (MAPIN.EQ.29)  GO TO 7029
      IF (MAPIN.EQ.30)  GO TO 7029
      IF (MAPIN.EQ.33)  GO TO 7033
      IF (MAPIN.EQ.34)  GO TO 7033
      IF (MAPIN.EQ.35)  GO TO 7035
      IF (MAPIN.EQ.36)  GO TO 7035
      IF (MAPIN.EQ.45)  GOTO 7045
      IF (MAPIN.EQ.46)  GOTO 7045
      IER = 7
      RETURN
C
 7021 IMAXIN = 73
      JMAXIN = 19
      DEGIN  = 5.0
      GO TO 8000
C
 7029 IMAXIN = 145
      JMAXIN = 37
      DEGIN  = 2.5
      GO TO 8000
C
 7033 IMAXIN = 181
      JMAXIN = 46
      DEGIN  = 2.0
      GO TO 8000
C
 7035 IMAXIN = 361
      JMAXIN = 91
      DEGIN  = 1.0
      GO TO 8000
C
 7045 IMAXIN = 97
      JMAXIN = 25
      DEGIN  = 3.75
      GOTO 8000
C
C     SELECT OUTPUT LOLA GRID
C
 8000 IF (MAPOUT.EQ.21)  GO TO 8021
      IF (MAPOUT.EQ.22)  GO TO 8021
      IF (MAPOUT.EQ.29)  GO TO 8029
      IF (MAPOUT.EQ.30)  GO TO 8029
      IF (MAPOUT.EQ.33)  GO TO 8033
      IF (MAPOUT.EQ.34)  GO TO 8033
      IF (MAPOUT.EQ.35)  GO TO 8035
      IF (MAPOUT.EQ.36)  GO TO 8035
      IF (MAPOUT.EQ.45)  GO TO 8045
      IF (MAPOUT.EQ.46)  GO TO 8045
      IF (MAPOUT.EQ.500) GO TO 8500
      IF (MAPOUT.EQ.501) GO TO 8501
      IER = 8
      RETURN
C
 8021 IMINOU = 1
      IMAXOU = 73
      JMINOU = 1
      JMAXOU = 19
      DEGOU  = 5.
      GO TO 8700
C
 8029 IMINOU = 1
      IMAXOU = 145
      JMINOU = 1
      JMAXOU = 37
      DEGOU  = 2.5
      GO TO 8700
C
 8033 IMINOU = 1
      IMAXOU = 181
      JMINOU = 1
      JMAXOU = 46
      DEGOU  = 2.0
      GO TO 8700
C
 8035 IMINOU = 1
      IMAXOU = 361
      JMINOU = 1
      JMAXOU = 91
      DEGOU  = 1.0
      GO TO 8700
C
 8045 IMINOU = 1
      IMAXOU = 97
      JMINOU = 1
      JMAXOU = 25
      DEGOU  = 3.75
      GOTO 8700
C
 8500 IMINOU = 93
      IMAXOU = 117
      JMINOU = 1
      JMAXOU = 37
      DEGOU  = 2.5
      GO TO 8700
C
 8501 IMINOU = 116
      IMAXOU = 140
      JMINOU = 1
      JMAXOU = 46
      DEGOU  = 2.0
      GO TO 8700
C
 8700 IJOUT  = 0
      RDEG   = DEGOU/DEGIN
      DO 8710 J=JMINOU, JMAXOU
          XJIN   = (J-1)*RDEG + 1.
          DO 8710 I=IMINOU, IMAXOU
              XIIN   = (I-1)*RDEG + 1.
              CALL W3FT01
     1        (XIIN, XJIN, FIELD, D, IMAXIN, JMAXIN, 1, INTERP)
              IJOUT  = IJOUT + 1
              DATA(IJOUT) = D
 8710 CONTINUE
      RETURN
C
      END
      SUBROUTINE ADJUEX(QHST,SNOW,FLAGI)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
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
      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1  QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2  QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M),
     3  QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      REAL QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT
      REAL QV, QRHO, QW, FW
      INTEGER IEN, MLFIX
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      REAL SINWAT, COSWAT, UWAT, VWAT
      COMMON/PMLPARM/DCVM, WUP, COSGAM, RTC, STC, QTOC
      REAL DCVM, WUP, COSGAM, RTC, STC, QTOC
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      REAL ZOW, FAKTH, ABLFIX, SURFWIN, ECMTYP
      COMMON/WORK/TMP(0:L,0:M), WRK(0:L,0:M,2), TMP2(0:L,0:M),
     1  TMP3(0:L,0:M), SPACE(0:L,0:M,7)
      REAL TMP, WRK, TMP2, TMP3, SPACE
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -WRK:  DUMMY ARRAYS
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C=======================================================================
      REAL QHST(0:L,0:M), SNOW(0:L,0:M), FLAGI(0:L,0:M), TMP4(0:L,0:M)
     1  ,TMP5(0:L,0:M)
      REAL FLAGM, SS, TT, QFMO1, QSFO, QSM
      INTEGER I, J
C=======================================================================
      FLAGM=1.0-FLOAT(MLFIX)
  200 CALL VECMAX(QHST,0.,TMP)
      DO 210 J=1,MM
      DO 210 I=0,L
       QTOC=(TFREZ-QT(I,J))*QH(I,J)*FLAGI(I,J)*OM(I,J)
       HT(I,J)=HT(I,J)+QTOC*FLAGI(I,J)
       QHST(I,J)=QHST(I,J)+CC/CLO*QTOC*FLAGI(I,J)
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
       QT(I,J)=QT(I,J)-(QT(I,J)-TFREZ+TMP3(I,J)*CLO/CC/QH(I,J))
     1         *FLAGI(I,J)*OM(I,J)
       QS(I,J)=QS(I,J)+QSFO/QH(I,J)*FLAGI(I,J)*OM(I,J)
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
       SS=SS*FLAGI(I,J)+1.0-FLAGI(I,J)
       TT=TT*FLAGI(I,J)+1.0-FLAGI(I,J)
       QDT(I,J)=QDT(I,J)-(QDT(I,J)-(QTB(I,J)*QHB(I,J)-HT(I,J))/TT
     1          +QH(I,J))*FLAGI(I,J)*FLAGM*OM(I,J)
       QDS(I,J)=QDS(I,J)-(QDS(I,J)-(QSB(I,J)*QHB(I,J)-HS(I,J))/SS
     1          +QH(I,J))*FLAGI(I,J)*FLAGM*OM(I,J)
  220 CONTINUE
      IF (MLFIX.EQ.1) GOTO 300
      CALL VERDIF

  300 CONTINUE

      RETURN
      END
      SUBROUTINE BUDGET(FH,T,LRHS,KG, LWDN, SWDN)
C=======================================================================
C  PROGRAMMED BY:
C     A.STOESSEL               MPI, HAMBURG                         1989
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
C  EXTERNALS:
C     -VAPOR: CALCULATES VAPOR PRESSURE
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/STP/TX,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1 ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M), WRK(0:L,0:M,2), ALB(0:L,0:M), 
     1TRK(0:L,0:M), A2(0:L,0:M), FLAI(LMDP), FAKTS(LMDP), SPACE(LMDP,4)
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
     2 ALB1(LMDP), A21(LMDP), HICE1(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
C================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), DIFF(LMDP),
     1TT(LMDP), TT1(0:L,0:M), TMYUS1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
      IMAX=30
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
      DO 79 J=1,MM
      DO 79 I=0,L
       TMYUS1(I,J)=OM(I,J)+2.
       IF (A(I,J,LRHS).EQ.0.) TMYUS1(I,J)=2.
   79 CONTINUE
      TB=TFREZ+TMELT
C-----------------------------------------------------------------------
C  STORE EXTERNAL VARIABLES INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 82 J=1,MM
      DO 82 I=0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 82
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J)
   82 CONTINUE
      IF (K.EQ.0) GOTO 87
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(T1,ESTI,2,K)
      D1=RHOAIR*CPAIR*CSENS
      D2I=RHOAIR*SUBL*CLAT
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 33 N=1,K
       STP(N)=T1(N)
       EA=TD1(N)*ESTA(N)
       FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1       -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2       EPSI/PA1(N)+(STP(N)-TB)/HICE1(N)*CON
       T1(N)=T1(N)+1.
       TT(N)=0.
   33 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE SURFACE TEMPERATURE (START OF ITERATION PROCEDURE)
C-----------------------------------------------------------------------
      DO 3 ITER=1,IMAX
       CALL VAPOR(T1,ESTI,2,K)
       DO 34 N=1,K
        STPP(N)=STP(N)
        FPP(N)=FP(N)
        STP(N)=T1(N)
        EA=TD1(N)*ESTA(N)
        FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1        -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2        EPSI/PA1(N)+(STP(N)-TB)/HICE1(N)*CON
        FDIFF=FP(N)-FPP(N)
        T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1        MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
        DIFF(N)=T1(N)-STP(N)
        TT(N)=SIGN(1.,.01-ABS(DIFF(N)))
   34  CONTINUE
    3 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED HEAT BALANCE EQUATION
C-----------------------------------------------------------------------
      DO 83 N=1,K
       FLAG=.5*(1.+SIGN(1.,T1(N)-TMELT))
       T1(N)=T1(N)*(1.-FLAG)+TMELT*FLAG
       EA=TD1(N)*ESTA(N)
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       FLSE1(N)=D1*UG1(N)*(TA1(N)-T1(N))
       FLLA1(N)=D2I*UG1(N)*(EA-ESTI(N))*EPSI/PA1(N)
       Q1=D3*T1(N)**4
       Q2= SWDN1(N) 
       Q3= LWDN1(N)
       FHI=A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB-T1(N))/HICE1(N)*CON)/CLO
       FHB=((TB-T1(N))/HICE1(N)*CON)/CLB
       FH1(N)=FHI+FHB
   83 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 84 J=1,MM
      DO 84 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 84
       IF (A(I,J,LRHS).EQ.0.) GOTO 84
       K=K+1
       FH(I,J)=FH1(K)
       T(I,J)=T1(K)-TMELT
       TT1(I,J)=TT(K)
       IF (TT1(I,J).LE.0.) WRITE(16,701) IIC
       IF (KG.NE.4) GOTO 84
       TA(I,J)=TA1(K)
       FLSE(I,J)=FLSE1(K)
       FLLA(I,J)=FLLA1(K)
   84 CONTINUE

   87 CONTINUE

      RETURN
  701 FORMAT (1X,I4,'ITERATION EXCEEDED')
      END
      SUBROUTINE ECMBDI(FH,T,LRHS,KG, LWDN, SWDN)
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C  PURPOSE:
C     -CALCULATES GROWTH RATES FOR ICE COVERED PART OF A GRID CELL WITH
C       ASL-PARAMETERIZATION ACC.TO LOUIS(79)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT (NOTE: SEE EKMAH)
C  METHOD:
C     -ICE OR SNOW SURFACE TEMPERATURES ARE CALCULATED BY ITERATION
C       (REGULA FALSI)
C     -MONIN-OBUKHOV THEORY WITH MONIN-OBUKHOV LENGTH REPLACED BY THE
C       RICHARDSON NUMBER
C  OPTIONS:
C     -STATEMENTS FOR ADDITIONAL WIND TURNING (ACC.TO STOESSEL (1990))
C  INTERFACE:
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    ICE OR SNOW SURFACE TEMPERATURE IN CELSIUS
C     -LRHS: RUNNING INDEX VALUE FOR OLD TIME STEP
C     -KG:   INDEX FOR ICE THICKNESS CATEGORIES
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -RISTAB: CALC.THE STAB.FUNCTIONS WITH FIXED ROUGHNESS LENGTH
C     -RESIST: CALC.STAB.FUNCTIONS FOR ADDITIONAL WIND TURNING(OPTIONAL)
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/STP/TX,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1 ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M), WRK(0:L,0:M,2), ALB(0:L,0:M), 
     1 TRK(0:L,0:M), A2(0:L,0:M), FLAI(LMDP), FAKTS(LMDP), QG(LMDP), 
     2 THETG(LMDP), SPACE(LMDP,2)
C=======================================================================
C     -HICE:  EFFECTIVE ICE THICKNESS (OPTIONALLY FOR SEVEN CATEGORIES)
C     -WRK:   DUMMY ARRAYS
C     -ALB:   ALBEDO
C     -TRK:   DUMMY ARRAY
C     -A2:    FLAG FOR SNOW CONDITIONS
C     -FLAI:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -QG:    DUMMY ARRAY
C     -THETG: POTENTIAL AIR TEMPERATURE
C=======================================================================
      REAL FH(0:L,0:M), T(0:L,0:M), ESTA(LMDP), ESTI(LMDP), EA1(LMDP),
     1 ZA(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI: SATURATION VAPOR PRESSURE OVER ICE
C     -EA1:  RELATIVE HUMIDITY
C     -ZA:   HEIGHT OF FORCING LEVEL
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1 FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), T1(LMDP), 
     2 ALB1(LMDP), A21(LMDP), HICE1(LMDP), UST(LMDP), CD1(LMDP), 
     3 FM1(LMDP), TMPL(LMDP)
C=======================================================================
C    Remark: The following are introduced to use externally-computed
C      downwelling radiation parameters.  BG
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
C======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), TT(LMDP),
     1TMYUS1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
      REAL SINBET1(LMDP), COSBET1(LMDP), BETA1(LMDP), WMUE(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE OPTIONAL ADDITIONAL
C    WIND TURNING
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
      IMAX=30
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
      DO 79 J=1,MM
      DO 79 I=0,L
       TMYUS1(I,J)=OM(I,J)+2.
       IF (A(I,J,LRHS).EQ.0.)TMYUS1(I,J)=2.
   79 CONTINUE
      TB=TFREZ+TMELT
C-----------------------------------------------------------------------
C  STORE EXTERNAL VARIABLES INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 82 J=1,MM
      DO 82 I=0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 82
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J)
C**NEXT STATEMENT FOR ADDITIONAL WIND TURNING:
C      FM1(K)=FM(I,J)
   82 CONTINUE
      IF (K.EQ.0) GOTO 87
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(T1,ESTI,2,K)
      DO 31 N=1,K
       EA1(N)=TD1(N)*ESTA(N)
       FLAG=.5*(1.-SIGN(1.,PA1(N)-1.E5))
       THETG(N)=FLAG*TA1(N)+(1.-FLAG)*(TA1(N)+6.5E-3*RGAS
     1          *TA1(N)*LOG(1.E5/PA1(N))/GRAV)*(PA1(N)/1.E5)**KAPPA
       ZA(N)=MAX(30.,((PA1(N)-100000.)*.08))
       TMPL(N)=GRAV*ZA(N)*(THETG(N)- T1(N)+.61* T1(N)*(EA1(N)-ESTI(N))
     1         *EPSI/PA1(N))/ T1(N)/UG1(N)**2
   31 CONTINUE
      CALL RISTAB(TMPL,ZOI,K,PA1, PH1, PH2)
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 33 N=1,K
       STP(N)=T1(N)
       FLSE1(N)=UG1(N)*(THETG(N)-STP(N))*RHOAIR*CPAIR
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*EPSI/PA1(N)*RHOAIR*VAPL
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1       -FLSE1(N)-FLLA1(N)+(STP(N)-TB)/HICE1(N)*CON
       T1(N)=T1(N)+1.
       TT(N)=0.
   33 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE SURFACE TEMPERATURE (START OF ITERATION PROCEDURE)
C-----------------------------------------------------------------------
      DO 3 ITER=1,IMAX
       CALL VAPOR(T1,ESTI,2,K)
       DO 32 N=1,K
        TMPL(N)=GRAV*ZA(N)*(THETG(N)-T1(N)+.61*T1(N)*(EA1(N)-ESTI(N))
     1          *EPSI/PA1(N))/T1(N)/UG1(N)**2
   32  CONTINUE
       CALL RISTAB(TMPL,ZOI,K,PA1, PH1, PH2)
       DO 34 N=1,K
        STPP(N)=STP(N)
        FPP(N)=FP(N)
        STP(N)=T1(N)
        FLSE1(N)=UG1(N)*(THETG(N)-STP(N))*RHOAIR*CPAIR
     1           *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
        FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*EPSI/PA1(N)*RHOAIR*VAPL
     1           *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
        FP(N)=D3*STP(N)**4- SWDN1(N)- LWDN1(N)
     1        -FLSE1(N)-FLLA1(N)+(STP(N)-TB)/HICE1(N)*CON
        FDIFF=FP(N)-FPP(N)
        T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)
     1        /MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
        DIFF=T1(N)-STP(N)
        TT(N)=SIGN(1.,.01-ABS(DIFF))
   34  CONTINUE
    3 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 83 N=1,K
       FLAG=.5*(1.+SIGN(1.,T1(N)-TMELT))
       T1(N)=T1(N)*(1.-FLAG)+TMELT*FLAG
   83 CONTINUE
      CALL VAPOR(T1,ESTI,2,K)
      DO 52 N=1,K
       TMPL(N)=GRAV*ZA(N)*(THETG(N)-T1(N)+.61*T1(N)*(EA1(N)-ESTI(N))
     1         *EPSI/PA1(N))/T1(N)/UG1(N)**2
   52 CONTINUE
      CALL RISTAB(TMPL,ZOI,K,PA1, PH1, PH2)
      DO 85 N=1,K
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       FLSE1(N)=UG1(N)*(THETG(N)-T1(N))*RHOAIR*CPAIR
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*EPSI/PA1(N)*RHOAIR*VAPL
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       Q1=D3*T1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       UST(N)=(.4/LOG(ZA(N)/ZOI))*UG1(N)*SQRT(PH2(N))
       CD1(N)=(UST(N)/UG1(N))**2
C**NEXT STATEMENT FOR ADDITIONAL WIND TURNING:
C      WMUE(N)=.4*UST(N)/ABS(FM1(N))/ZA(N)/PH1(N)*PH2(N)**2*TMPL(N)
       FHI=A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB-T1(N))/HICE1(N)*CON)/CLO
       FHB=((TB-T1(N))/HICE1(N)*CON)/CLB
       FH1(N)=FHI+FHB
   85 CONTINUE
C**NEXT CALL AND LOOP FOR ADDITIONAL WIND TURNING:
C     CALL RESIST(WMUE,K, AF, BF, CF)
C     DO 86 N=1,K
C      SINBET1(N)=-BF(N)/VONKAR*UST(N)/UG1(N)
C      FLAG=SIGN(1.,SINBET1(N))
C      SINBET1(N)=MIN(ABS(SINBET1(N)), 1.)*FLAG
C      COSBET1(N)=SQRT(1.-SINBET1(N)*SINBET1(N))
C      BETA1(N)=ACOS(COSBET1(N))/RAD
C  86 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 84 J=1,MM
      DO 84 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 84
       IF (A(I,J,LRHS).EQ.0.) GOTO 84
       K=K+1
       FH(I,J)=FH1(K)
       T(I,J)=T1(K)-TMELT
C      IF (TT(K).LE.0.) WRITE(16,701) IIC
       IF (KG.NE.4) GOTO 84
       CD(I,J)=CD1(K)
C**NEXT FOUR STATEMENTS FOR ADDITIONAL WIND TURNING:
C      WMUE1(I,J)=WMUE(K)
C      SINBET(I,J)=SINBET1(K)
C      COSBET(I,J)=COSBET1(K)
C      BETA(I,J)=BETA1(K)
   84 CONTINUE

   87 CONTINUE

      RETURN
  701 FORMAT (1X,I4,'ITERATION EXCEEDED')
      END
      SUBROUTINE ECMBDO(FH,QT, LWDN, SWDN)
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C  PURPOSE:
C     -CALCULATES GROWTH RATES OF NEW ICE IN THE ICE FREE PART OF A GRID
C       CELL WITH ASL-PARAMETERIZATION ACC.TO LOUIS(79)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT (NOTE: SEE EKMAH)
C  METHOD:
C     -HEAT BUDGET EQUATION FOR OPEN WATER
C     -MONIN-OBUKHOV THEORY WITH MONIN-OBUKHOV LENGTH REPLACED BY THE
C       RICHARDSON NUMBER
C  INTERFACE:
C     -FH: GROWTH RATE IN METERS OF ICE
C     -QT: SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -LWDN: Downwelling longwave in W/M**2
C     -SWDN: Downwelling shortwave in W/M**2
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -RWSTAB: CALC.THE STAB.FUNCTIONS WITH VARIABLE ROUGHNESS LENGTH
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1 BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD (0:L,0:M), ACL(0:L,0:M), 
     1 PA(0:L,0:M), UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1 ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/WORK/WRK(0:L,0:M,6), FLAW(LMDP), FAKTS(LMDP), TRK(LMDP),
     1 THETG(LMDP), SPACE(LMDP,2)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -TRK:   DUMMY ARRAY
C     -THETG: POTENTIAL TEMPERATURE
C=======================================================================
      REAL FH(0:L,0:M), QT(0:L,0:M), ESTA(LMDP), ESTW(LMDP), EA1(LMDP),
     1 ZA(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -QT:   SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW: SATURATION VAPOR PRESSURE OVER WATER
C     -EA1:  RELATIVE HUMIDITY
C     -ZA:   HEIGHT OF FORCING LEVEL
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1 FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), QT1(LMDP), 
     2 ZOW1(LMDP), WUST(LMDP), CD1(LMDP), TMPL(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
C-----------------------------------------------------------------------
C  SELECT GRID CELLS AND STORE THEM INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 93 J=1,MM
      DO 93 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 93
       K=K+1
       ZOW1(K)=ZOW(I,J)
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J)
   93 CONTINUE
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 112 N=1,K
       EA1(N)=TD1(N)*ESTA(N)
       FLAG=.5*(1.-SIGN(1.,PA1(N)-1.E5))
       THETG(N)=FLAG*TA1(N)+(1.-FLAG)*(TA1(N)+6.5E-3*RGAS
     1          *TA1(N)*LOG(1.E5/PA1(N))/GRAV)*(PA1(N)/1.E5)**KAPPA
       ZA(N)=MAX(30.,((PA1(N)-100000.)*.08))
       TMPL(N)=GRAV*ZA(N)*(THETG(N)-QT1(N)+.61*QT1(N)*(EA1(N)-ESTW(N))
     1         *EPSI/PA1(N))/QT1(N)/UG1(N)**2
  112 CONTINUE
C-----------------------------------------------------------------------
C  GET THE STABILITY FUNCTIONS
C-----------------------------------------------------------------------
      CALL RWSTAB(TMPL,ZOW1,K,PA1, PH1, PH2)
C-----------------------------------------------------------------------
C  CALCULATE HEAT FLUXES AND GROWTH RATES
C-----------------------------------------------------------------------
      DO 25 N=1,K
       FLSE1(N)=UG1(N)*(THETG(N)-QT1(N))*RHOAIR*CPAIR*(.4/LOG(ZA(N)/
     1          ZOW1(N)))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTW(N))*EPSI/PA1(N)*RHOAIR*VAPL*
     1          (.4/LOG(ZA(N)/ZOW1(N)))**2*PH1(N)/.74
       Q1=D3*QT1(N)**4
       Q2 = SWDN1(N)
       Q3 = LWDN1(N)
       WUST(N)=(.4/LOG(ZA(N)/ZOW1(N)))*UG1(N)*SQRT(PH2(N))
       CD1(N)=(WUST(N)/UG1(N))**2
       FH1(N)=(Q1-Q2-Q3-FLSE1(N)-FLLA1(N))/CLB
   25 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 81 J=1,MM
      DO 81 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 81
       K=K+1
       ZOW(I,J)=MAX(1.5E-5,.032*WUST(K)**2/GRAV)
       CD(I,J)=CD1(K)
       FH(I,J)=FH1(K)
   81 CONTINUE

      RETURN
      END
      SUBROUTINE EKMAH(FH,T,LRHS,KG, LWDN, SWDN)
C=======================================================================
C  PROGRAMMED BY:
C     -C.KOCH                     UNI, BONN                         1986
C  MODIFIED EXTENSIVELY BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C  PURPOSE:
C     -CALCULATES GROWTH RATES FOR THE ICE COVERED PART OF A GRID CELL
C       WITH ABL-MODEL ACC.TO KOCH(88)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT AND TURNING
C       ANGLE TO BE USED IN SUBROUTINE RELCON (NOTE THAT BY THIS
C       CONFIGURATION THE RESULTANT STRESS IS STAGGERED IN TIME,
C       I.E. THOSE RESULTS OF THE PRESENT ROUTINE WHICH ARE RELEVANT
C       FOR THE DYNAMICS WILL FIRST BE USED AT THE NEXT TIME STEP)
C  METHOD:
C     -HEAT BUDGET EQUATION OVER ICE (OPTIONALLY FOR SEVEN THICKNESS
C       CATEGORIES)
C     -MONIN-OBUKHOV THEORY FOR SURFACE LAYER (DERIVED QUANTITIES ONLY)
C     -ROSSBY NUMBER SIMILARITY THEORY FOR EKMAN LAYER (ABL)
C     -THE STABILITY PARAMETER (WMUE), THE FRICTION VELOCITY (UST) AND
C       THE ICE OR SNOW SURFACE TEMPERATURE ARE SOLVED PER ITERATION
C       (REGULA FALSI)
C     -IN ORDER TO ACCELERATE THE INTEGRATION, ONLY THOSE GRID CELLS
C       WHICH DID NOT PASS THE SOLUTION CRITERIA ARE SELECTED FOR
C       FURTHER ITERATIONS; THIS METHOD REQUIRES AN INTERMEDIATE
C       STORAGE OF THE VARIABLES INTO A ONE-DIMENSIONAL ARRAY
C  OPTIONS:
C     -THE FRICTION VELOCITY CAN BE CALCULATED VIA THE RESISTANCE LAWS
C       OF THE EKMAN-LAYER (SURFWIN=0) OR VIA THE MONIN-OBUKHOV THEORY
C       (SURFWIN=1)
C  INTERFACE:
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    ICE OR SNOW SURFACE TEMPERATURE IN CELSIUS
C     -LRHS: RUNNING INDEX VALUE FOR OLD TIME STEP
C     -KG:   INDEX FOR ICE THICKNESS CATEGORIES
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -STAB:   CALCULATION OF STABILITY FUNCTIONS FOR SURFACE LAYER
C     -RESIST: CALCULATION OF STABILITY FUNCTIONS FOR EKMAN LAYER
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/GEO/PI,RAD
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/STP/TX,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M), 
     1  BETA(0:L,0:M), TAUX (L,M), TAUY (L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD (0:L,0:M), ACL(0:L,0:M), 
     1  PA(0:L,0:M), UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1  ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M), WRK(0:L,0:M,2), ALB(0:L,0:M), 
     1  TRK(0:L,0:M), A2(0:L,0:M), FLAI(LMDP), FAKTS(LMDP), QG(LMDP),
     2  THETG(LMDP), SPACE(LMDP,2)
C=======================================================================
C     -HICE:  EFFECTIVE ICE THICKNESS (OPTIONALLY FOR SEVEN CATEGORIES)
C     -WRK:   DUMMY ARRAYS
C     -ALB:   ALBEDO
C     -TRK:   DUMMY ARRAY
C     -A2:    FLAG FOR SNOW CONDITIONS
C     -FLAI:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -QG:    DUMMY ARRAY
C     -THETG: POTENTIAL AIR TEMPERATURE
C=======================================================================
      REAL FH(0:L,0:M), T(0:L,0:M), ESTA(LMDP), ESTI(LMDP), DELT(LMDP),
     1  SUM1(LMDP), SUM2(LMDP), FAKT1(LMDP), QAL(LMDP)
C=======================================================================
C     -FH:    GROWTH RATE IN METERS OF ICE
C     -T:     SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA:  SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI:  SATURATION VAPOR PRESSURE OVER ICE
C     -DELT:  VERTICAL TEMPERATURE GRADIENT
C     -SUM1:  LOG OF MODIFIED SURFACE ROSSBY NUMBER
C     -SUM2:  VERTICAL SPECIFIC HUMIDITY GRADIENT FOR VIRTUAL TEMP.GRAD.
C     -FAKT1: FACTOR FOR CALCULATION OF STABILITY PARAMETER
C     -QAL:   FACTOR FOR RESISTANCE LAWS OF BAROTROPIC EKMAN LAYER
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1  FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), T1(LMDP), 
     2  ALB1(LMDP), A21(LMDP), HICE1(LMDP), UST(LMDP), CD1(LMDP), 
     3  WMUE(LMDP), FM1(LMDP), SINBET1(LMDP), COSBET1(LMDP), 
     4  BETA1(LMDP), TMPL(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE NECESSARY FOR THE OPTIMIZATION OF THE
C    ITERATION PROCEDURES
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), UPAST(LMDP),
     1TMYUS1(0:L,0:M), TMYUS(LMDP), TMUE(LMDP), TUST(LMDP), PAST(LMDP)
     2,TT(LMDP), TT1(0:L,0:M), TMUE1(0:L,0:M), TUST1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
C  FOR SINGLE ITERATION LOOPS:
      IMAX=10
C  FOR OVERALL ITERATION LOOP:
      IWMAX=50
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
      DO 79 J=1,MM
      DO 79 I=0,L
       TMYUS1(I,J)=OM(I,J)+2.
       IF (A(I,J,LRHS).EQ.0.)TMYUS1(I,J)=2.
   79 CONTINUE
      TB=TFREZ+TMELT
      ITERW=0
C-----------------------------------------------------------------------
C  START OF OVERALL ITERATION PROCEDURE
C-----------------------------------------------------------------------
   88 CONTINUE
      K=0
C-----------------------------------------------------------------------
C  SELECT GRID CELLS FOR FURTHER ITERATIONS AND BUILD UP ONE-DIM. ARRAY
C-----------------------------------------------------------------------
      DO 82 J=1,MM
      DO 82 I=0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 82
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J)
       FM1(K)=FM(I,J)
       IF (ITERW.EQ.0) GOTO 82
       WMUE(K)=WMUE1(I,J)
       UST(K)=UST1(I,J)
   82 CONTINUE
      IF (K.EQ.0) GOTO 91
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATIONS
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      DO 31 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
       DELT(N)=THETG(N)-T1(N)
       IF (ITERW.GT.0) GOTO 31
       UST(N)=0.1
       WMUE(N)=SIGN(50.,DELT(N))
       TMPL(N)=5.*ABS(FM1(N))*WMUE(N)/UST(N)
   31 CONTINUE
C-----------------------------------------------------------------------
C  REPEAT THE SINGLE ITERATION PROCEDURES TWICE
C-----------------------------------------------------------------------
      DO 7 ITERM=1,3
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR THE SURFACE TEMPERATURE
C-----------------------------------------------------------------------
       CALL RESIST(WMUE,K, AF, BF, CF)
       CALL STAB(TMPL,ZOI,K, PH1, PH2)
       CALL VAPOR(T1,ESTI,2,K)
       CALL VAPOR(T1,ESTA,1,K)
       DO 33 N=1,K
        FAKT=1./(LOG(UST(N)/ABS(FM1(N))/ZOI)-CF(N))
        FAKT1(N)=FAKTH*UST(N)*FAKT
        QAL(N)=PH1(N)*FAKT
        STP(N)=T1(N)
        EA=MAX(0.,ESTA(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/EPSI)
        TA1(N)=MAX(200.,STP(N)*(1.-QAL(N))+QAL(N)*THETG(N))
        FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1        -FAKT1(N)*(THETG(N)-STP(N)+SUBL/CPAIR*(QG(N)-EPSI
     2        /PA1(N)*ESTI(N)))+(STP(N)-TB)/HICE1(N)*CON
        T1(N)=T1(N)+1.
   33  CONTINUE
C-----------------------------------------------------------------------
C  START THE ITERATION FOR THE SURFACE TEMPERATURE
C-----------------------------------------------------------------------
       DO 3 ITER=1,IMAX
        CALL VAPOR(T1,ESTI,2,K)
        CALL VAPOR(T1,ESTA,1,K)
        DO 34 N=1,K
         STPP(N)=STP(N)
         FPP(N)=FP(N)
         STP(N)=T1(N)
         EA=MAX(0.,ESTA(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/.623)
         TA1(N)=MAX(200.,STP(N)*(1.-QAL(N))+QAL(N)*THETG(N))
         FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1         -FAKT1(N)*(THETG(N)-STP(N)+SUBL/CPAIR*(QG(N)-0.623
     2         /PA1(N)*ESTI(N)))+(STP(N)-TB)/HICE1(N)*CON
         FDIFF=FP(N)-FPP(N)
         T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1         MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
         DIFF=T1(N)-STP(N)
         TT(N)=SIGN(1.,.01-ABS(DIFF))
   34   CONTINUE
    3  CONTINUE
C-----------------------------------------------------------------------
C  MAKE SURE THAT THE SURFACE TEMP. DOES NOT EXCEED THE MELTING POINT
C-----------------------------------------------------------------------
       DO 83 N=1,K
        FLAG=.5*(1.+SIGN(1.,T1(N)-TMELT))
        T1(N)=T1(N)*(1.-FLAG)+TMELT*FLAG
   83  CONTINUE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR THE STABILITY PARAMETER
C-----------------------------------------------------------------------
       CALL VAPOR(T1,ESTI,2,K)
       DO 42 N=1,K
        PAST(N)=WMUE(N)
        DELT(N)=THETG(N)-T1(N)
        SUM1(N)=LOG( UST(N)/ABS(FM1(N))/ZOI)
        SUM2(N)=0.61*(QG(N)-0.623/PA1(N)*ESTI(N))
        FAKT1(N)=GRAV*0.064/ABS(FM1(N))/UST(N)
        STP(N)=WMUE(N)
        TMPL(N)=5.*ABS(FM1(N))*STP(N)/UST(N)
   42  CONTINUE
       CALL STAB(TMPL,ZOI,K, PH1, PH2)
       CALL RESIST(STP,K, AF, BF, CF)
       DO 43 N=1,K
        FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
        SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
        FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1        /(T1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
        WMUE(N)=WMUE(N)*1.5
   43  CONTINUE
C-----------------------------------------------------------------------
C  START THE ITERATION FOR THE STABILITY PARAMETER
C-----------------------------------------------------------------------
       DO 4 ITER=1,IMAX
        DO 44 N=1,K
         STPP(N)=STP(N)
         FPP(N)=FP(N)
         STP(N)=WMUE(N)
         TMPL(N)=5.*ABS(FM1(N))*STP(N)/ UST(N)
   44   CONTINUE
        CALL STAB(TMPL,ZOI,K, PH1, PH2)
        CALL RESIST(STP,K, AF, BF, CF)
        DO 45 N=1,K
         FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
         SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
         FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1         /(T1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
         FDIFF=FP(N)-FPP(N)
         WMUE(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
         DIFF=WMUE(N)-STP(N)
         TMUE(N)=SIGN(1.,10.-ABS(DIFF))
   45   CONTINUE
    4  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE FRICTION VELOCITY
C-----------------------------------------------------------------------
C**FOR CYCLE 7 SKIP UST-ITERATION:
       IF (SURFWIN.EQ.1)THEN
        DO 22 N=1,K
   22   UST(N)=.4*UG1(N)/PH2(N)
       ELSE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        CALL RESIST(WMUE,K, AF, BF, CF)
        DO 52 N=1,K
         UPAST(N)=UST(N)
         STP(N)=UST(N)
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
         FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
         STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOI)
         UST(N)=UST(N)*0.8
   52   CONTINUE
C-----------------------------------------------------------------------
C  START ITERATION FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        DO 5 ITER=1,IMAX+1
         DO 53 N=1,K
          FPP(N)=FP(N)
          STPP(N)=STP(N)
          STP(N)=UST(N)
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
          FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
          STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOI)
          FDIFF=FP(N)-FPP(N)
          FLAG=.5*(1.+SIGN(1.,ABS(FDIFF)-1.E-10))
          FDIFF=FDIFF*FLAG+.1*(1.-FLAG)
          UST(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
          DIFF= UST(N)-STP(N)
          TUST(N)=SIGN(1.,.0001-ABS(DIFF))
   53    CONTINUE
    5   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  DETERMINE WHETHER WE SUCCEEDED IN FINDING ANY SOLUTION
C-----------------------------------------------------------------------
       DO 54 N=1,K
        UST(N)=MAX(UST(N), 0.014)
        UDIFF=ABS(UST(N)-UPAST(N))*(1.-SURFWIN)
        DIFF=ABS(WMUE(N)-PAST(N))
        TMYUS(N)=SIGN(1.,.01-UDIFF)
        TMYUS(N)=SIGN(1.,10.-DIFF)+TMYUS(N)
        TMPL(N)=5.*ABS(FM1(N))*WMUE(N)/ UST(N)
   54  CONTINUE
    7 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE THE QUANTITIES NEEDED FOR THE NEXT OVERALL ITERATION STEP
C-----------------------------------------------------------------------
      K=0
      DO 92 J=1,MM
      DO 92 I=0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 92
       K=K+1
       T(I,J)=T1(K)-TMELT
       TMPL1(I,J)=TMPL(K)
       TMYUS1(I,J)=TMYUS(K)
       TT1(I,J)=TT(K)
       TUST1(I,J)=TUST(K)
       TMUE1(I,J)=TMUE(K)
       UST1(I,J)=UST(K)
       WMUE1(I,J)=WMUE(K)
   92 CONTINUE
C-----------------------------------------------------------------------
C  FINISH OVERALL ITERATION AFTER EXCEEDING THE SPECIFIED MAXIMUM
C-----------------------------------------------------------------------
      ITERW=ITERW+1
C**ITERATION-INFO CANCELLED:
      IF (ITERW.GT.IWMAX) GOTO 91
      GOTO 88
C  90 WRITE(16,700) IIC
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED T, WMUE AND UST
C-----------------------------------------------------------------------
   91 CONTINUE
      K=0
      DO 93 J=1,MM
      DO 93 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 93
       IF (A(I,J,LRHS).EQ.0.) GOTO 93
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J)
       FM1(K)=FM(I,J)
       TMPL(K)=TMPL1(I,J)
       WMUE(K)=WMUE1(I,J)
       UST(K)=UST1(I,J)
   93 CONTINUE
      CALL VAPOR(TA1,ESTA,1,K)
      DO 112 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
  112 CONTINUE
      CALL STAB(TMPL,ZOI,K, PH1, PH2)
      CALL RESIST(WMUE,K, AF, BF, CF)
      DO 55 N=1,K
       FAKT=1./(LOG(UST(N)/ABS(FM1(N))/ZOI)-CF(N))
       FAKT1(N)=FAKTH*UST(N)*FAKT
       QAL(N)=PH1(N)*FAKT
       TA1(N)=T1(N)*(1.-QAL(N))+QAL(N)*THETG(N)
       EA=MAX(0.,ESTI(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/.623)
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       ALPHA=FAKTH*UST(N)/PH1(N)
       STRE=UST(N)**2*RHOAIR
       FLSE1(N)=ALPHA*(TA1(N)-T1(N))
       FLLA1(N)=ALPHA*(EA-ESTI(N))/CPAIR*VAPL*0.623/PA1(N)
       ZL=5.*ABS(FM1(N))*WMUE(N)/UST(N)
       Q1=D3*T1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       SINBET1(N)=-BF(N)/VONKAR*UST(N)/UG1(N)
       FLAG=SIGN(1.,SINBET1(N))
       SINBET1(N)=MIN(ABS(SINBET1(N)), 1.)*FLAG
       COSBET1(N)=SQRT(1.-SINBET1(N)*SINBET1(N))
       BETA1(N)=ACOS(COSBET1(N))/RAD
       CD1(N)=(UST(N)/UG1(N))**2
       FHI=A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB-T1(N))/HICE1(N)*CON)/CLO
       FHB=((TB-T1(N))/HICE1(N)*CON)/CLB
       FH1(N)=FHI+FHB
   55 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED T, WMUE AND UST
C-----------------------------------------------------------------------
      K=0
      DO 84 J=1,MM
      DO 84 I=0,L
        IF (OM(I,J).EQ.0.) GOTO 84
        IF (A(I,J,LRHS).EQ.0.) GOTO 84
        K=K+1
        FH(I,J)=FH1(K)
        IF (KG.NE.4)GOTO84
        CD(I,J)=CD1(K)
        SINBET(I,J)=SINBET1(K)
        COSBET(I,J)=COSBET1(K)
   84 CONTINUE

      RETURN
  701 FORMAT (1X,I4,'ITERATION EXCEEDED')
      END
      SUBROUTINE EKMAO(FH,QT, LWDN, SWDN)
C=======================================================================
C  PROGRAMMED BY:
C     -C.KOCH                     UNI, BONN                         1986
C  MODIFIED EXTENSIVELY BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C  PURPOSE:
C     -CALCULATES GROWTH RATES OF NEW ICE IN THE ICE FREE PART OF A GRID
C       CELL WITH ABL-MODEL ACC.TO KOCH(88)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT AND TURNING
C       ANGLE TO BE USED IN SUBROUTINE RELCON (NOTE: SEE EKMAH)
C  METHOD:
C     -HEAT BUDGET EQUATION FOR OPEN WATER
C     -MONIN-OBUKHOV THEORY FOR SURFACE LAYER (DERIVED QUANTITIES ONLY)
C     -ROSSBY NUMBER SIMILARITY THEORY FOR EKMAN LAYER (ABL)
C     -THE STABILITY PARAMETER (WMUE) AND THE FRICTION VELOCITY (WUST)
C       ARE SOLVED PER ITERATION (REGULA FALSI)
C     -IN ORDER TO ACCELERATE THE INTEGRATION, ONLY THOSE GRID CELLS
C       WHICH DID NOT PASS THE SOLUTION CRITERIA ARE SELECTED FOR
C       FURTHER ITERATIONS; THIS METHOD REQUIRES AN INTERMEDIATE
C       STORAGE OF THE VARIABLES INTO A ONE-DIMENSIONAL ARRAY
C  OPTIONS:
C     -THE FRICTION VELOCITY CAN BE CALCULATED VIA THE RESISTANCE LAWS
C       OF THE EKMAN-LAYER (SURFWIN=0) OR VIA THE MONIN-OBUKHOV THEORY
C       (SURFWIN=1)
C  INTERFACE:
C     -FH: GROWTH RATE IN METERS OF ICE
C     -QT: SEA SURFACE=OML TEMPERATURE IN CELSIUS
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -STAB:   CALCULATION OF STABILITY FUNCTIONS FOR SURFACE LAYER
C     -RESIST: CALCULATION OF STABILITY FUNCTIONS FOR EKMAN LAYER
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1  ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1  ,WUST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/WORK/WRK(0:L,0:M,6), FLAW(LMDP), FAKTS(LMDP), QG(LMDP),
     1THETG(LMDP), SPACE(LMDP,2)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -QG:    SPECIFIC HUMIDITY AT 850 HPA
C     -THETG: POTENTIAL TEMPERATURE AT 850 HPA
C=======================================================================
      REAL FH(0:L,0:M), QT(0:L,0:M), ESTA(LMDP), ESTW(LMDP), DELT(LMDP)
     1  ,SUM1(LMDP), SUM2(LMDP), FAKT1(LMDP), QAL(LMDP)
C=======================================================================
C     -FH:    GROWTH RATE IN METERS OF ICE
C     -QT:    SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA:  SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW:  SATURATION VAPOR PRESSURE OVER WATER
C     -DELT:  VERTICAL TEMPERATURE GRADIENT
C     -SUM1:  LOG OF MODIFIED SURFACE ROSSBY NUMBER
C     -SUM2:  VERTICAL SPECIFIC HUMIDITY GRADIENT FOR VIRTUAL TEMP.GRAD.
C     -FAKT1: FACTOR FOR CALCULATION OF STABILITY PARAMETER
C     -QAL:   FACTOR FOR RESISTANCE LAWS OF BAROTROPIC EKMAN LAYER
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1  FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), QT1(LMDP), 
     2  ZOW1(LMDP), WUST(LMDP), CD1(LMDP), WMUE(LMDP), FM1(LMDP), 
     3  SINBET1(LMDP), COSBET1(LMDP), TMPL(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
C===========
C  REMARK: THESE VARIABLES ARE NECESSARY FOR THE OPTIMIZATION OF THE
C    ITERATION PROCEDURES
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), UPAST(LMDP),
     1  TWMYUS1(0:L,0:M), TWMYUS(LMDP), TWMUE(LMDP), TWUST(LMDP), 
     2  PAST(LMDP), TWMUE1(0:L,0:M), TWUST1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
C  FOR SINGLE ITERATION LOOPS:
      IMAX=10
C  FOR OVERALL ITERATION LOOP:
      IWMAX=50
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
      DO 79 J=1,MM
      DO 79 I=0,L
   79 TWMYUS1(I,J)=OM(I,J)+2.
      TB=TFREZ+TMELT
      ITERW=0
C-----------------------------------------------------------------------
C  START OF OVERALL ITERATION PROCEDURE
C-----------------------------------------------------------------------
   88 CONTINUE
      K=0
C-----------------------------------------------------------------------
C  SELECT GRID CELLS FOR FURTHER ITERATIONS AND BUILD UP ONE-DIM. ARRAY
C-----------------------------------------------------------------------
      DO 80 J=1,MM
      DO 80 I=0,L
       IF (TWMYUS1(I,J).EQ.2.) GOTO 80
       K=K+1
       ZOW1(K)=ZOW(I,J)
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       FM1(K)=FM(I,J)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J)
       IF (ITERW.EQ.0) GOTO 80
       WUST(K)=WUST1(I,J)
       WMUE(K)=WMUE1(I,J)
   80 CONTINUE
      IF (K.EQ.0) GOTO 91
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATIONS
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 11 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
       DELT(N)=THETG(N)-QT1(N)
       IF (ITERW.GT.0) GOTO 11
       WUST(N)=.1
       WMUE(N)=SIGN(50.,DELT(N))
   11 CONTINUE
C-----------------------------------------------------------------------
C  REPEAT THE SINGLE ITERATION PRODECURES TWICE
C-----------------------------------------------------------------------
      DO 6 ITERM=1,3
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR THE STABILITY PARAMETER
C-----------------------------------------------------------------------
       DO 12 N=1,K
        PAST(N)=WMUE(N)
        SUM1(N)=LOG(WUST(N)/ABS(FM1(N))/ZOW1(N))
        SUM2(N)=0.61*(QG(N)-EPSI/PA1(N)*ESTW(N))
        FAKT1(N)=GRAV*0.064/ABS(FM1(N))/WUST(N)
        STP(N)=WMUE(N)
   12  CONTINUE
       CALL STAB(TMPL,ZOW(0,0), K, PH1, PH2)
       CALL RESIST(STP,K, AF, BF, CF)
       DO 13 N=1,K
        FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
        SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
        FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1        /(QT1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
        WMUE(N)=WMUE(N)*1.5
   13  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE STABILITY PARAMETER (START OF ITERATION)
C-----------------------------------------------------------------------
       DO 1 ITER=1,IMAX
        DO 14 N=1,K
         STPP(N)=STP(N)
         FPP(N)=FP(N)
         STP(N)=WMUE(N)
         TMPL(N)=5.*ABS(FM1(N))*STP(N)/WUST(N)
   14   CONTINUE
        CALL STAB(TMPL,ZOW(0,0), K, PH1, PH2)
        CALL RESIST(STP,K, AF, BF, CF)
        DO 15 N=1,K
         FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
         SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
         FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1         /(QT1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
         FDIFF=FP(N)-FPP(N)
         WMUE(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
         DIFF=WMUE(N)-STP(N)
         TWMUE(N)=SIGN(1.,10.-ABS(DIFF))
   15   CONTINUE
    1  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE FRICTION VELOCITY
C-----------------------------------------------------------------------
C**FOR CYCLE 7 SKIP UST-ITERATION:
       IF (SURFWIN.EQ.1)THEN
        DO 66 N=1,K
   66   WUST(N)=.4*UG1(N)/PH2(N)
       ELSE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        CALL RESIST(WMUE,K, AF, BF, CF)
        DO 22 N=1,K
         UPAST(N)=WUST(N)
         STP(N)=WUST(N)
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
         FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
         STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOW1(N))
         WUST(N)=WUST(N)*0.8
   22   CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE FRICTION VELOCITY PER ITERATION
C-----------------------------------------------------------------------
        DO 2 ITER=1,IMAX+1
         DO 23 N=1,K
          FPP(N)=FP(N)
          STPP(N)=STP(N)
          STP(N)=WUST(N)
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
          FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
          STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOW1(N))
          FDIFF=FP(N)-FPP(N)
          WUST(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1            MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
          DIFF=WUST(N)-STP(N)
          TWUST(N)=SIGN(1.,.0001-ABS(DIFF))
   23    CONTINUE
    2   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  DETERMINE WHETHER WE SUCCEEDED IN FINDING ANY SOLUTION
C-----------------------------------------------------------------------
       DO 24 N=1,K
        WUST(N)=MAX(WUST(N), 0.014)
        UDIFF=ABS(WUST(N)-UPAST(N))*(1.-SURFWIN)
        DIFF=ABS(WMUE(N)-PAST(N))
        TWMYUS(N)=SIGN(1.,.01-UDIFF)
        TWMYUS(N)=SIGN(1.,10.-DIFF)+TWMYUS(N)
        TMPL(N)=5.*ABS(FM1(N))*WMUE(N)/WUST(N)
   24  CONTINUE
    6 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE THE QUANTITIES NEEDED FOR THE NEXT OVERALL ITERATION STEP
C-----------------------------------------------------------------------
      K=0
      DO 92 J=1,MM
      DO 92 I=0,L
       IF (TWMYUS1(I,J).EQ.2.) GOTO 92
       K=K+1
       TWMUE1(I,J)=TWMUE(K)
       TWUST1(I,J)=TWUST(K)
       TMPL1(I,J)=TMPL(K)
       TWMYUS1(I,J)=TWMYUS(K)
       WUST1(I,J)=WUST(K)
       WMUE1(I,J)=WMUE(K)
   92 CONTINUE
C-----------------------------------------------------------------------
C  FINISH OVERALL ITERATION AFTER EXCEEDING THE SPECIFIED MAXIMUM
C-----------------------------------------------------------------------
      ITERW=ITERW+1
C**ITERATION-INFO CANCELLED:
      IF (ITERW.GT.IWMAX) GOTO 91
      GOTO 88
C  90 WRITE(16,701) IIC
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED STABILITY AND FRICTION VELOCITY
C-----------------------------------------------------------------------
   91 CONTINUE
      K=0
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
       SWDN1(K) = SWDN(I,J)
       FM1(K)=FM(I,J)
       TMPL(K)=TMPL1(I,J)
       WMUE(K)=WMUE1(I,J)
       WUST(K)=WUST1(I,J)
   93 CONTINUE
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 112 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
  112 CONTINUE
      CALL STAB(TMPL,ZOW(0,0), K, PH1, PH2)
      CALL RESIST(WMUE,K, AF, BF, CF)
      DO 25 N=1,K
       FAKT=1./(LOG(WUST(N)/ABS(FM1(N))/ZOW1(N))-CF(N))
       FAKT1(N)=FAKTH*WUST(N)*FAKT
       QAL(N)=PH1(N)*FAKT
       TA1(N)=QT1(N)*(1.-QAL(N))+QAL(N)*THETG(N)
       EA=MAX(0.,ESTW(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/EPSI)
       ALPHE=FAKTH*WUST(N)/(SUM1(N)-CF(N))
       ALPHA=FAKTH*WUST(N)/PH1(N)
       STRE=WUST(N)**2*RHOAIR
       FLSE1(N)=ALPHA*(TA1(N)-QT1(N))
       FLLA1(N)=ALPHA*(EA-ESTW(N))/CPAIR*VAPL*EPSI/PA1(N)
       ZL=5.*ABS(FM1(N))*WMUE(N)/WUST(N)
       Q1=D3*QT1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       SINBET1(N)=-BF(N)/VONKAR*WUST(N)/UG1(N)
       FLAG=SIGN(1.,SINBET1(N))
       SINBET1(N)=MIN(ABS(SINBET1(N)), 1.)*FLAG
       COSBET1(N)=SQRT(1.-SINBET1(N)*SINBET1(N))
       CD1(N)=(WUST(N)/UG1(N))**2
       FH1(N)=(Q1-Q2-Q3-FLSE1(N)-FLLA1(N))/CLB
   25 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 81 J=1,MM
      DO 81 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 81
       K=K+1
       FH(I,J)=FH1(K)
       CD(I,J)=CD1(K)
       SINBET(I,J)=SINBET1(K)
       COSBET(I,J)=COSBET1(K)
C**ITERATION-INFO CANCELLED:
C      IF (TWMUE1(I,J).LE.0..OR.TWUST1(I,J).LE.0.) WRITE(16,701) IIC
   81 CONTINUE

      RETURN
  701 FORMAT (1X,I4,'ITERATION EXCEEDED')
      END
      SUBROUTINE GROWTH(LRHS,LNEW, LWDN, SWDN)
C=======================================================================
C  PROGRAMMED BY:
C     W.D.HIBLER III           CRREL, HANOVER, USA                  1979
C     W.B.OWENS                MPI, HAMBURG                         1987
C     R. W. Grumbine           NMC, Camp Springs, MD                1992
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
C     -Downwelling radiation accepted as an argument
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
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      COMMON/VEL/U(L,M,3), V(L,M,3)
      REAL U, V
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/TEMP/TICE(0:L,0:M)
      COMMON/TEMPM/TICM(0:L,0:M,NLEVEL)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2 QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M),
     3 QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      COMMON/SNOFLG/SNOFLG
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/WORK/TMP(0:L,0:M), RH(0:L,0:M), RA(0:L,0:M), TMP2(0:L,0:M),
     1 TMP3(0:L,0:M), TMP4(0:L,0:M), QHST(0:L,0:M), QFM(0:L,0:M),
     2 PREC(0:L,0:M), SN(0:L,0:M), QTM(0:L,0:M), SH(0:L,0:M)
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
C-----------------------------------------------------------------------
C  CREATE OPEN WATER DUE TO SHEAR DEFORMATION
C-----------------------------------------------------------------------
      CALL SHDEF(LNEW,TMP3)
      CALL VECMAX(A(0,0,LRHS), ARMAG, TMP)
      DO 301 J=1,MM
      DO 301 I=0,L
       TMP2(I,J)=(1.0-TMP(I,J))*(0.5*(1.0+TMP(I,J))-ARMAG)
     1           /((1.-ARMAG)*(0.5*(1.+ARMAG)-ARMAG))
       A(I,J,LNEW)=A(I,J,LNEW)-TMP3(I,J)*(1.0-TMP2(I,J))*DT
  301 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE COS OF ZENITH DISTANCE AND SOLAR CONSTANT
C   Removed by BG.  Now use externally computed radiation
C-----------------------------------------------------------------------
C  CALCULATE THE GROWTH RATE FOR THIN ICE (OPEN OCEAN)
C-----------------------------------------------------------------------
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP:
      IF (ABLFIX.EQ.1.) THEN
       CALL OBUDGET(RA,QT, LWDN, SWDN)
      ELSE
       IF (ECMTYP.EQ.1.) THEN
        CALL ECMBDO(RA,QT, LWDN, SWDN)
       ELSE
        CALL EKMAO(RA,QT, LWDN, SWDN)
       END IF
      END IF
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
      DO 8 K=1, NLEVEL
       DO 6 J=1,MM
       DO 6 I=0,L
C  SET ALBEDO ACC.TO PRESENCE OF SNOW(TMP4) AND MELTING COND.(TMP3):
        TMP4(I,J)=0.5*(1.-SIGN(1.,-HSN(I,J,LRHS)))
        TMP3(I,J)=0.5*(1.+SIGN(1.,TICE(I,J)))
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP3(I,J)=0.5*(1.+SIGN(1.,TICM(I,J,K)))
        TMP2(I,J)=TMP4(I,J)*(TMP3(I,J)*ALBSNM+(1.-TMP3(I,J))*ALBSN)
     1           +(1.-TMP4(I,J))*(TMP3(I,J)*ALBM+(1.-TMP3(I,J))*ALBI)
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP(I,J)=(2*K-1)*SN(I,J)/FLOAT(NLEVEL)
    6  CONTINUE
C      CALL EKMAH (RH,TICE, LWDN, SWDN)
C**FOR N LEVEL MODEL,INS. THE FOLL.6 STATEM.AND COMM.OUT PREV.STATEM.:
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP:
       IF (ABLFIX.EQ.1.) THEN
        CALL BUDGET(SH,TICM(0,0,K), LRHS,K, LWDN, SWDN)
       ELSE
        IF (ECMTYP.EQ.1.) THEN
         CALL ECMBDI(SH,TICM(0,0,K), LRHS,K, LWDN, SWDN)
        ELSE
         CALL EKMAH(SH,TICM(0,0,K), LRHS,K, LWDN, SWDN)
        END IF
       END IF
C  GET MEAN GROWTH RATE OF THICK ICE:
       DO 7 J=1,MM
       DO 7 I=0,L
        RH(I,J)=RH(I,J)+SH(I,J)/FLOAT(NLEVEL)
    7  CONTINUE
    8 CONTINUE
      DO 10 J=1,MM
      DO 10 I=0,L
C-----------------------------------------------------------------------
C  DETERMINE THERMODYNAMIC ICE THICKNESS CHANGE FOR CONTINUITY EQUATION
C-----------------------------------------------------------------------
       RA(I,J)=RA(I,J)*OM(I,J)*DT
       RH(I,J)=RH(I,J)*OM(I,J)*DT
       SH(I,J)=RH(I,J)*A(I,J,LRHS)+(1.0-A(I,J,LRHS))*RA(I,J)
C-----------------------------------------------------------------------
C  ADD SNOWFALL TO SNOW LAYER, OR ADD PRECIPITATION FOR MIXED LAYER
C-----------------------------------------------------------------------
       TMP3(I,J)=0.5*(1.-SIGN(1.,TAIR(I,J)))*A(I,J,LRHS)*SNOFLG
       TMP(I,J)=RPREC(I,J)*DT*OM(I,J)
       PREC(I,J)=(1.-TMP3(I,J))*TMP(I,J)
       HSN(I,J,LNEW)=HSN(I,J,LNEW)+TMP3(I,J)*TMP(I,J)*RHOWAT/RHOSNO
       TMP(I,J)=RH(I,J)*A(I,J,LRHS)
   10 CONTINUE
C-----------------------------------------------------------------------
C  IF SH BECOMES NEGATIVE, FIRST MELT ANY SNOW THAT IS PRESENT
C-----------------------------------------------------------------------
      CALL VECMIN(TMP,0.,TMP2)
      DO 12 J=1,MM
      DO 12 I=0,L
   12 TMP2(I,J)=HSN(I,J,LNEW)+TMP2(I,J)*RHOICE/RHOSNO
C  MAKE SURE WE DO NOT END UP WITH NEGATIVE SNOW THICKNESS:
      CALL VECMAX(TMP2,0.,SN)
      DO 15 J=1,MM
      DO 15 I=0,L
       TMP2(I,J)=HSN(I,J,LNEW)-SN(I,J)
       HSN(I,J,LNEW)=SN(I,J)
C  MODIFY THE ICE MELT AND PRECIPITATION TO ACCOUNT FOR SNOW MELT:
       RH(I,J)=SH(I,J)+TMP2(I,J)*RHOSNO/RHOICE
       PREC(I,J)=PREC(I,J)+TMP2(I,J)*RHOSNO/RHOWAT
C-----------------------------------------------------------------------
C  COMPUTE CHANGE IN ICE MASS (OR HEAT STORAGE IN OML) DUE TO ATM.FORC.
C-----------------------------------------------------------------------
       QHST(I,J)=H(I,J,LNEW)-(QT(I,J)-TFREZ)*QH(I,J)*CC/CLO+RH(I,J)
   15 CONTINUE
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
       QTM(I,J)=(-TMP2(I,J)-(QT(I,J)-TFREZ)*QH(I,J)*CC/CLO)*OM(I,J)
       QFM(I,J)=(QFM(I,J)-TMP(I,J))*OM(I,J)*RHOICE/RHOWAT
       FW(I,J)=-QFM(I,J)
   20 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE OML VARIABLES
C-----------------------------------------------------------------------
C  CALCULATE KINETIC ENERGY FOR MIXED LAYER
      DO 1000 J = 1, MM
        DO 1100 I = 1, LM
          QV(I,J) = (0.25* (SQRT(U(I,J,LNEW)**2+V(I,J,LNEW)**2)
     1                     +SQRT(U(I+1,J,LNEW)**2+V(I+1,J,LNEW)**2)
     2                     +SQRT(U(I,J+1,LNEW)**2+V(I,J+1,LNEW)**2)
     3                     +SQRT(U(I+1,J+1,LNEW)**2+V(I+1,J+1,LNEW)**2))
     4               + 0.01 )**3 * OM(I,J)
 1100   CONTINUE
 1000 CONTINUE

      CALL PMLEX(QHST, SN, QFM, PREC, QTM)
C-----------------------------------------------------------------------
C  UPDATE VARIABLES DUE TO OCEANIC HEAT FLUX
C-----------------------------------------------------------------------
      DO 120 J=1,MM
      DO 120 I=0,L
C  UPDATE ICE THICKNESS OR HEAT STORAGE, RESPECTIVELY:
       QHST(I,J)=QHSTO(I,J)+OM(I,J)*(QHST(I,J)-QHSTO(I,J))
C  UPDATE NET FREEZING RATE:
       FW(I,J)=FW(I,J)-QFM(I,J)
C  UPDATE SNOW THICKNESS:
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
      RETURN
      END
      SUBROUTINE OBUDGET(FH,QT, LWDN, SWDN)
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
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      REAL ZOW, FAKTH, ABLFIX, SURFWIN, ECMTYP
      COMMON/THFOR/TAIR(0:L,0:M), TD (0:L,0:M), ACL(0:L,0:M), 
     1  PA(0:L,0:M), UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL TAIR, TD, ACL, PA, UG, TA, RPREC
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1  ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      REAL FLSE, FLLA, WMUE1, UST1, TMPL1
      COMMON/WORK/WRK(0:L,0:M,6), FLAW(LMDP), FAKTS(LMDP), SPACE(LMDP,4)
      REAL WRK, FLAW, FAKTS, SPACE
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
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      INTEGER I, J, K, N
      REAL D1, D2W, EA, Q1, Q2, Q3
C-----------------------------------------------------------------------
C  SELECT THE GRID CELLS AND STORE THEM IN SELECTIVE 1-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
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
       SWDN1(K) = SWDN(I,J)
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
      DO 81 J=1,MM
      DO 81 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 81
       K=K+1
       FH(I,J)=FH1(K)
       TA(I,J)=TA1(K)
       FLSE(I,J)=FLSE1(K)
       FLLA(I,J)=FLLA1(K)
   81 CONTINUE

      RETURN
      END
      SUBROUTINE PMLEX(QHST,SNOW,QFM,QPR,QTM)
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C  PURPOSE:
C     -PROGNOSTIC CALCULATION OF OCEANIC MIXED LAYER VARIABLES (OML-
C       MODEL) AND VERTICAL OCEANIC HEAT FLUX
C  METHOD:
C     -HEAT AND SALT BUDGET OF WATER COLUMN
C     -KRAUS-TURNER TYPE PARAMETERIZATION FOR ENTRAINMENT HEAT FLUX WITH
C       EXPONENTIALLY SHAPED PYCNOCLINE
C     -ENERGY BALANCE FOR CLOSURE
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
      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M), 
     2QHSTO(0:L,0:M),  HS(0:L,0:M),  HT(0:L,0:M),  QV(0:L,0:M),
     3QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      COMMON/PMLPARM/DCVM, WUP, COSGAM, RTC, STC, QTOC
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
C=======================================================================
      COMMON/WORK/TMP(0:L,0:M), WRK(0:L,0:M,2), TMP2(0:L,0:M), 
     1TMP3(0:L,0:M), TMP4(0:L,0:M), SPACE(0:L,0:M,6)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -WRK:  DUMMY ARRAYS
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
      DO 10 J=1,MM
      DO 10 I=0,L
       QFM(I,J)=-QFM(I,J)*(QS(I,J)-SICE)-QPR(I,J)*QS(I,J)
       QFM(I,J)=QFM(I,J)*OM(I,J)
       QTM(I,J)= QTM(I,J)*CLO/CC*OM(I,J)
       QRHO(I,J)=(BETAS*QFM(I,J)-BETAT*QTM(I,J))/DT
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
      IF (MLFIX.NE.1) GOTO 30
C-----------------------------------------------------------------------
C  THE NEXT LOOP ENTERS ONLY FOR FIXED MIXED LAYER DEPTH
C-----------------------------------------------------------------------
      DO 20 J=1,MM
      DO 20 I=0,L
       IEN(I,J)=1
       HT(I,J)=HT(I,J)+QTOC/CC*DT*OM(I,J)
       QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/QH(I,J)+QTB(I,J)
       QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/QH(I,J)+QSB(I,J)
       QTM(I,J)=QTOC
   20 CONTINUE
      GOTO 200
C-----------------------------------------------------------------------
C  DETERMINE SIGN OF ENTRAINMENT
C-----------------------------------------------------------------------
   30 CONTINUE
      CALL VECMAX(TMP,DCVM,DCV)
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
      DO 70 J=1,MM
      DO 70 I=0,L
       RET(I,J)=(TMP3(I,J)-QH(I,J))*(1.-FLOAT(IEN(I,J)))/RTC
   70 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE ENTRAINMENT VELOCITY
C-----------------------------------------------------------------------
      DO 100 J=1,MM
      DO 100 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
       ADS=1.0+(EXP(-DELTAD/QDS(I,J))-1.0)*QDS(I,J)/DELTAD
       ADT=1.0+(EXP(-DELTAD/QDT(I,J))-1.0)*QDT(I,J)/DELTAD
       SSS=SS*ADS
       TTT=TT*ADT
       DETA=GRAV*QH(I,J)*(BETAS*SSS-BETAT*TTT)
       TMP(I,J)=DETA-GRAV*QH(I,J)*TTT*DCV(I,J)
     1   *(BETAT-CC/CLO*BETAS*RHOICE/RHOWAT*(QS(I,J)-SICE))*FLAGI(I,J)
  100 CONTINUE
      CALL VECMAX(TMP,EPSAA,TMP2)
      CALL VECMAX(ENT,0.,TMP)
      DO 110 J=1,MM
      DO 110 I=0,L
       ENT(I,J)=TMP(I,J)/TMP2(I,J)*DT*FLOAT(IEN(I,J))
  110 CONTINUE
      CALL VECMIN(ENT,ENTMAX,TMP)
C-----------------------------------------------------------------------
C  DETERMINE NEW MIXED LAYER DEPTH
C-----------------------------------------------------------------------
      DO 120 J=1,MM
      DO 120 I=0,L
       TMP2(I,J)=QH(I,J)-(WUP*DT-TMP(I,J)*FLOAT(IEN(I,J))-
     1           RET(I,J)*(1.0-FLOAT(IEN(I,J))))*OM(I,J)
       TMP3(I,J)=HMLREF
       TMP(I,J)=QH(I,J)
  120 CONTINUE
      CALL VECMINC(TMP2,TMP3,QH)
C-----------------------------------------------------------------------
C  DETERMINE MIXED LAYER AND PYCNOCLINE VARIABLES
C-----------------------------------------------------------------------
      DO 140 J=1,MM
      DO 140 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
       FLAGTO=(1.+SIGN(1.,ABS(TT)-TTMIN))/2.*SIGN(1.,-TT)
       ADS=1.0+(EXP(-DELTAD/QDS(I,J))-1.0)*QDS(I,J)/DELTAD
       ADT=1.0+(EXP(-DELTAD/QDT(I,J))-1.0)*QDT(I,J)/DELTAD
       SSS=SS*ADS
       TTT=TT*ADT
       HV=TMP(I,J)
       ENT(I,J)=(QH(I,J)-HV+WUP*DT)*FLOAT(IEN(I,J))
       DH1=QH(I,J)-HV
       HV=2.0*QH(I,J)*HV/(QH(I,J)+HV)
       SHB=QSB(I,J)*QHB(I,J)
       THB=QTB(I,J)*QHB(I,J)
       QS(I,J)=QS(I,J)+(QFM(I,J)/HV+SSS*ENT(I,J)/HV)*OM(I,J)
       SS=QSB(I,J)-QS(I,J)
       FLAGS=(1.+SIGN(1.,ABS(SS)-SSMIN))/2.
       SS=SS*FLAGS+1.0-FLAGS
       QDS(I,J)=QDS(I,J)-(QDS(I,J)-(SHB-HS(I,J))/SS+QH(I,J))*FLAGS
     1          *OM(I,J)
       QDS(I,J)=QDS(I,J)-DH1*(1.0-FLAGS)*OM(I,J)
       QS(I,J)=QS(I,J)-(QS(I,J)-(HS(I,J)-SHB)/(QH(I,J)+QDS(I,J))-
     1         QSB(I,J))*(1.0-FLAGS)*OM(I,J)
       QT(I,J)=QT(I,J)+(QTM(I,J)/HV+TTT*ENT(I,J)/HV)*OM(I,J)
       TT=QTB(I,J)-QT(I,J)
       FLAGTN=(1.+SIGN(1.,ABS(TT)-TTMIN))/2.*SIGN(1.,-TT)
       FLAGT=FLAGTO*FLAGTN*(1.+FLAGTO*FLAGTN)/2.
       TT=TT*FLAGT+1.0-FLAGT
       QDT(I,J)=QDT(I,J)-(QDT(I,J)-(THB-HT(I,J))/TT+QH(I,J))*FLAGT
     1          *OM(I,J)
       QDT(I,J)=QDT(I,J)-DH1*(1.0-FLAGT)*OM(I,J)
       QT(I,J)=QT(I,J)-(QT(I,J)-(HT(I,J)-THB)/(QH(I,J)+QDT(I,J))-
     1         QTB(I,J))*(1.0-FLAGT)*OM(I,J)
       QTM(I,J)=TTT*ENT(I,J)*FLAGT*OM(I,J)
  140 CONTINUE
C-----------------------------------------------------------------------
C  SIMULATE VERTICAL DIFFUSION
C-----------------------------------------------------------------------
      CALL VERDIF
C-----------------------------------------------------------------------
C  ADJUST MIXED LAYER VARIABLES AFTER THE PREVIOUS MODIFICATIONS
C-----------------------------------------------------------------------
  200 CONTINUE
      CALL ADJUEX(QHST,SNOW,FLAGI)
C-----------------------------------------------------------------------
C  STABILITY ADJUSTMENT (CONVECTION)
C-----------------------------------------------------------------------
      IF (MLFIX.EQ.1) GO TO 300
      DO 270 J=1,MM
      DO 270 I=0,L
       STAB=BETAS*(QSB(I,J)-QS(I,J))-BETAT*(QTB(I,J)-QT(I,J))
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
       FLAGS=(1.0-SIGN(1.,STAB-EPSAA))/2.
       TMP(I,J)=QH(I,J)
       TMP3(I,J)=HMLREF+10.0
C  PARTIAL OVERTURNING IN ICE COVERED OCEAN:
       SSTAR=QSB(I,J)-BETAT/BETAS*(QTB(I,J)-QT(I,J))-0.01
       QH(I,J)=QH(I,J)+(QS(I,J)-SSTAR)*(QH(I,J)+QDS(I,J)-QHSTO(I,J))/
     1         (QTB(I,J)-TFREZ)/(CC/CLO*(SSTAR-SICE)-BETAT/BETAS)
     2         *FLAGI(I,J)*OM(I,J)*FLAGS
C  OVERTURNING IN OPEN OCEAN:
       QH(I,J)=QH(I,J)+(QH(I,J)-TMP3(I,J))*(1.-FLAGI(I,J))*FLAGS*OM(I,J)
       QS(I,J)=QS(I,J)-(QS(I,J)-SSTAR)*(1.-FLAGI(I,J))*FLAGS*OM(I,J)
       HS(I,J)=HS(I,J)-(HS(I,J)-(SSTAR-QSB(I,J))*(QH(I,J)+QDS(I,J))
     1         -QSB(I,J)*QHB(I,J))*(1.0-FLAGI(I,J))*FLAGS*OM(I,J)
  270 CONTINUE
      CALL VECMAXC(QH,TMP,TMP2)
      CALL VECMINC(TMP2,TMP3,QH)
      CALL VERDIF
      DO 280 J=1,MM
      DO 280 I=0,L
       TMP2(I,J)=QT(I,J)
       QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/(QH(I,J)+QDT(I,J))+QTB(I,J)
       QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/(QH(I,J)+QDS(I,J))+QSB(I,J)
       QTM(I,J)=QTM(I,J)+(QT(I,J)-TMP2(I,J))*QH(I,J)
       QTM(I,J)=QTM(I,J)*CC/DT*OM(I,J)
  280 CONTINUE
      CALL ADJUEX(QHST,SNOW,FLAGI)
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
       QHST(I,J)=QHST(I,J)-(QHST(I,J)+(QT(I,J)-TFREZ)*QH(I,J)*CC/CLO)*
     1           FLAGI(I,J)
C  ON OUTPUT: QTM IS ENTRAINMENT HEAT FLUX IN [W/M**2]
       QTM(I,J)=QTM(I,J)*(1.-FLAGI(I,J))*FLOAT(IEN(I,J))
C  ON OUTPUT: QFM = MELTING RATE DUE TO OCEANIC HEAT FLUX IN [M]
       QFM(I,J)=(TMP(I,J)-TMP2(I,J))*OM(I,J)*RHOICE/RHOWAT
       IEN(I,J)=INT(1.0-FLAGI(I,J)+0.3)
  310 CONTINUE
C  PARAMETERIZE ADVECTIVE EFFECTS:
      CALL VECMIN(QDS,HMLREF,QDS)
      CALL VECMIN(QDT,HMLREF,QDT)
      CALL VECMIN(QH,HMLREF,QH)
      DO 401 J=1,MM
      DO 401 I=0,L
  401 QSS(I,J)=QSB(I,J)-(BETAT/BETAS)*(QTB(I,J)-QT(I,J))-.01
      CALL VECMINC(QS,QSS,QS)
      DO 400 J=1,MM
      DO 400 I=0,L
       HS(I,J)=(QS(I,J)-QSB(I,J))*(QH(I,J)+QDS(I,J))+QSB(I,J)*QHB(I,J)
       HT(I,J)=(QT(I,J)-QTB(I,J))*(QH(I,J)+QDT(I,J))+QTB(I,J)*QHB(I,J)
  400 CONTINUE
      RETURN
      END
      SUBROUTINE RESIST(X,K, AF, BF, CF)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -DETERMINATION OF STABILITY FUNCTIONS A,B,AND C (AF,BF,CF)
C  METHOD:
C     -ROSSBY NUMBER SIMILARITY THEORY (KOCH, 1986: 93)
C  INTERFACE:
C     -X: STABILITY PARAMETER (WMUE)
C     -K: MAXIMUM NUMBER OF GRID POINTS TO BE TREATED
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL AF(LMDP), BF(LMDP), CF(LMDP)
      REAL X(LMDP)
      INTEGER K
      INTEGER N
      REAL FLAG
C=======================================================================
      DO 1 N=1,K
       FLAG=.5*(1.+SIGN(1.,X(N)+50.))
       AF(N)=4.5*(1.-FLAG)+(-.00144*X(N)**2-.144*X(N)+.9)*FLAG
       FLAG=.5*(1.+SIGN(1.,X(N)+75.))
       BF(N)=1.0*(1.-FLAG)+(.00062*X(N)**2+.093*X(N)+4.4875)*FLAG
       FLAG=.5*(1.+SIGN(1.,X(N)+100.))
       CF(N)=7.5*(1.-FLAG)+(-.00065*X(N)**2-.13*X(N)+1.)*FLAG
    1 CONTINUE
      RETURN
      END
      SUBROUTINE RISTAB(RI, ZO, K, PA1, PH1, PH2)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF THE STABILITY FUNCTIONS FM AMD FH ACCORDING TO
C       LOUIS(79) WITH FIXED ROUGHNESS LENGTH
C  INTERFACE:
C     -RI:  RICHARDSON NUMBER
C     -ZO:  ROUGHNESS LENGTH (FIXED)
C     -K:   MAXIMUM GRID POINTS TO BE TREATED
C     -PA1: SURFACE AIR PRESSURE
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL PH1(LMDP), PH2(LMDP)
      REAL RI(LMDP), PA1(LMDP)
      REAL ZO
      INTEGER K
      INTEGER N
      REAL ZA, FAKT, CM, CH, FM, FH
C=======================================================================
      DO 2 N=1,K
       IF (RI(N).GE.0.) GOTO 1
       ZA=MAX(30.,((PA1(N)-100000.)*0.08))
       FAKT=(.4/LOG(ZA/ZO))**2*9.4*SQRT(ZA/ZO)
       CM=7.4*FAKT
       CH=5.3*FAKT
       FM=1.-9.4*RI(N)/(1.+CM*SQRT(ABS(RI(N))))
       FH=1.-9.4*RI(N)/(1.+CH*SQRT(ABS(RI(N))))
       GOTO 3
    1  CONTINUE
       FM=1./(1.+2.*4.7*RI(N))**2
       FH=FM
    3  CONTINUE
       PH1(N)=FH
       PH2(N)=FM
    2 CONTINUE
      RETURN
      END
      SUBROUTINE RWSTAB(RI, ZO, K, PA1, PH1, PH2)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF THE STABILITY FUNCTIONS FM AMD FH ACCORDING TO
C       LOUIS(79) FOR VARIABLE ROUGHNESS LENGTHS
C  INTERFACE:
C     -RI:  RICHARDSON NUMBER
C     -ZO:  ROUGHNESS LENGTH (VARIABLE)
C     -K:   MAXIMUM GRID POINTS TO BE TREATED
C     -PA1: SURFACE AIR PRESSURE
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL PH1(LMDP), PH2(LMDP)
      REAL RI(LMDP),PA1(LMDP),ZO(LMDP)
      INTEGER K
      INTEGER N
      REAL ZA, FAKT, CM, CH, FM, FH
C=======================================================================
      DO 2 N=1,K
       IF (RI(N).GE.0.) GOTO 1
       ZA=MAX(30.,((PA1(N)-100000.)*0.08))
       FAKT=(.4/LOG(ZA/ZO(N)))**2*9.4*SQRT(ZA/ZO(N))
       CM=7.4*FAKT
       CH=5.3*FAKT
       FM=1.-9.4*RI(N)/(1.+CM*SQRT(ABS(RI(N))))
       FH=1.-9.4*RI(N)/(1.+CH*SQRT(ABS(RI(N))))
       GOTO 3
    1  CONTINUE
       FM=1./(1.+2.*4.7*RI(N))**2
       FH=FM
    3  CONTINUE
       PH1(N)=FH
       PH2(N)=FM
    2 CONTINUE
      RETURN
      END
      SUBROUTINE STAB(OL,ZO,K, PH1, PH2)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF THE STABILITY FUNCTIONS PH1 AND PH2
C  METHOD:
C     -THE CALCULATIONS DIFFER ACCORDING TO THE SIGN OF THE MONIN-
C       OBUKHOV LENGTH (KOCH, 1986: 91-92)
C     -IN THIS VERSION ZA IS ASSIGNED FOR THE 10 M WIND
C  INTERFACE:
C     -OL: RECIPROCAL OF THE MONIN-OBUKHOV LENGTH TIMES 2 (LSTAR=2/OL)
C     -ZO: ROUGHNESS LENGTH
C     -K:  MAXIMUM NUMBER OF GRID POINTS TO BE TREATED
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL PH1(LMDP), PH2(LMDP)
      REAL OL(LMDP)
      REAL ZO, R, CRI, AS, BS, CS, DS, FLAG, ZLOG, ZOL
      INTEGER I, J, K, N
C=======================================================================
      R=SQRT(2.)
      CRI=-1./16.
C-----------------------------------------------------------------------
C  FIRST DETERMINE THE STABILITY FUNCTION FOR THE HEAT FLUXES
C-----------------------------------------------------------------------
      DO 2 N=1,K
       ZOL=OL(N)*ZO/2.
       IF (ZOL.GT.-(1.E-6)) GOTO 1
       CS=(1.-16.*OL(N))**0.5
       DS=(ABS(1.+16.*ZOL))**0.5
       IF (ZOL.GT.CRI+1.E-6) GOTO 10
       IF (ZOL.LT.CRI-1.E-6) GOTO 11
       PH1(N)=2.*(1.+1./CS)
       GOTO 2
C  FOR UNSTABLE STRATIFICATION:
   10  CONTINUE
       PH1(N)=LOG((CS-DS)*(1.+DS)/(CS+DS)/(1.-DS))/DS
       GOTO 2
   11  CONTINUE
       PH1(N)=2./DS*(ATAN(CS/DS)-ATAN(1./DS))
       GOTO 2
C  FOR NEUTRAL AND STABLE STRATIFICATION:
    1  CONTINUE
       ZLOG=LOG((2.+ZO)/ZO)
       PH1(N)=ZLOG+5.*(OL(N)+ZOL/ZLOG)
    2 CONTINUE
C-----------------------------------------------------------------------
C  NEXT DETERMINE THE STABILITY FUNCTION FOR THE MOMENTUM FLUX
C-----------------------------------------------------------------------
C     FLAG was unitialized as received.  
C     Set equal to zero by BG. 4/22/92.
      FLAG = 0.0
      DO 3 N=1,K
       OL(N)=5.*OL(N)
       ZOL=OL(N)*ZO/10.
       IF (ZOL.GT.-(1.E-6)) GOTO 4
       AS=(1.-16.*OL(N))**0.25
       BS=(ABS(1.+16.*ZOL))**0.25
       IF (ZOL.GT.CRI+1.E-6) GOTO 20
       IF (ZOL.LT.CRI-1.E-6) GOTO 21
       PH2(N)=4.*(1.-1./AS)
       GOTO 3
C  FOR UNSTABLE STRATIFICATION:
   20  CONTINUE
       PH2(N)=2./BS*(-0.5*LOG((BS+AS)*(BS-1.)/(BS-AS)/(1.+BS))
     1        +ATAN(AS/BS)-ATAN(1./BS))
       GOTO 3
   21  CONTINUE
       PH2(N)=(1.-FLAG)*(LOG(SQRT(AS**4+BS**4)*(1.+R*BS+BS*BS)
     1        /SQRT(1.+BS**4)/(AS*AS+R*AS*BS+BS*BS))
     2        +ATAN(AS*BS*R/(BS*BS-AS*AS))-ATAN(BS*R/(BS*BS-1.)))/BS/R
       GOTO 3
C  FOR NEUTRAL AND STABLE STRATIFICATION:
    4  CONTINUE
       ZLOG=LOG((10.+ZO)/ZO)
       PH2(N)=ZLOG+5.*(OL(N)+ZOL/ZLOG)
    3 CONTINUE
      RETURN
      END
      SUBROUTINE VAPOR(T,EST,K1,K)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     C.KOCH                  UNI, BONN                             1986
C  MODIFIED BY:
C     A.STOESSEL              MPI, HAMBURG                          1989
C  PURPOSE:
C     -CALCULATION OF SATURATION VAPOR PRESSURE FOR AIR TEMPERATURE
C       (K1=1), OVER ICE (K1=2) AND OVER WATER (K1=3)
C  INTERFACE:
C     -T:   TEMPERATURE OF ATMOSPHERE, ICE OR OCEAN
C     -EST: SATURATION VAPOR PRESSURE
C     -K1:  INDEX FOR CHOICE OF QUANTITY TO BE CALCULATED
C     -K:   INDEX FOR SELECTIVE LOOP
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL T(LMDP),EST(LMDP)
      INTEGER K, K1
      INTEGER N
C=======================================================================
      GOTO(1,2,3), K1

    1 CONTINUE
      DO 11 N=1,K
        EST(N)=611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),200.)-273.15+257.87))
   11 CONTINUE
      RETURN

    2 CONTINUE
      DO 22 N=1,K
        EST(N)=611.15*EXP((23.036-(MIN(T(N),273.15)-273.15)/333.7)*
     1        (MIN(T(N),273.15)-273.15)/(MAX(T(N),200.)-273.15+279.82))
   22 CONTINUE
      RETURN

    3 CONTINUE
      DO 33 N=1,K
        EST(N)=0.9815*611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),260.)-273.15+257.87))
   33 CONTINUE
      RETURN
      END
      SUBROUTINE VERDIF
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C  PURPOSE:
C     -SIMULATION OF VERTICAL DIFFUSION
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2 QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M),
     3 QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      REAL QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT
      REAL QV, QRHO, QW, FW
      INTEGER IEN, MLFIX
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
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
      ENTRY DDY(U,V,ETA,LN,K)
C=======================================================================
C  PURPOSE:
C     -CALCULATES THE Y-DERIVATIVES OF THAT PART OF THE INTERNAL ICE
C       STRESS WHICH DEPENDS ON THE CURRENT VELOCITIES
C=======================================================================
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
      SUBROUTINE OUTBCS
      IMPLICIT none
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
C  PURPOSE:
C     -SETS FORCE DUE TO INTERNAL ICE STRESS TO 0 AT OUTFLOW POINTS
C  METHOD:
C     -VISCOSITIES AND ICE STRENGTH ARE SET TO 0 AT OUTFLOW POINTS
C=======================================================================
      COMMON/PRESS/P(L,M)
      REAL P
      COMMON/OUTFLOW/NOUT,IOUT(LDO),JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
      COMMON/WORK/WRK(1:L,1:M,10),ZETA(1:L,1:M),ETA(1:L,1:M)
      REAL WRK, ZETA, ETA
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
      SUBROUTINE PLAST(LRHS)
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
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      INTEGER LRHS
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL H, A, HSN
      COMMON/PRESS/P(L,M)
      REAL P
      COMMON/WORK/TRK(1:L,1:M), E11(1:L,1:M), E22(1:L,1:M), 
     1 E12(1:L,1:M), WRK(1:L,1:M,6),
     2 ZETA(1:L,1:M), ETA(1:L,1:M)
      REAL TRK, E11, E22, E12, WRK, ZETA, ETA
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
      CALL STRAIN(LRHS, E11, E12, E22)
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
C  USE MIN AND MAX BULK VISCOSITIES TO CONSTRAIN THE VISCOSITIES
C-----------------------------------------------------------------------
C  MAXIMUM BULK VISCOSITY:
       TMP(I,J)=ZMAX*P(I,J)
       FLG(I,J)=0.5*(1.-SIGN(1.,(ZETA(I,J)-TMP(I,J))))
       ZETA(I,J)=FLG(I,J)*ZETA(I,J)+(1.-FLG(I,J))*TMP(I,J)
C  MINIMUM BULK VISCOSITY:
       FLG(I,J)=0.5*(1.+SIGN(1.,(ZETA(I,J)-ZMIN)))
       ZETA(I,J)=FLG(I,J)*ZETA(I,J)+(1.-FLG(I,J))*ZMIN
C  SHEAR VISCOSITY (EQ.11 IN HIBLER (79)):
       ETA(I,J)=ECM2*ZETA(I,J)
   10 CONTINUE
C-----------------------------------------------------------------------
C  NOW SET VISCOSITIES AND PRESSURE EQUAL TO ZERO AT OUTFLOW PTS
C-----------------------------------------------------------------------
      CALL OUTBCS

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
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      REAL  H(0:L,0:M,2), A(0:L,0:M,2)
      REAL  P(L,M)
      INTEGER LOLD, I, J
C=======================================================================
      DO 200 J=1,MM
      DO 200 I=1,L
        P(I,J)=PSTAR*H(I,J,LOLD)*EXP(-CSTAR*(1.0-A(I,J,LOLD)))
  200 CONTINUE

      RETURN
      END
      SUBROUTINE RELAX(LR,LN)
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE RELAX IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
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
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/VEL/U(L,M,3), V(L,M,3)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/WORK/TMP(1:L,1:M), RU(1:L,1:M), RV(1:L,1:M), DEN(1:L,1:M),
     1  AMAS(1:L,1:M), BU(1:L,1:M), BV(1:L,1:M), FX(1:L,1:M), 
     2  FY(1:L,1:M), ASY(1:L,1:M), ZETA(1:L,1:M), ETA(1:L,1:M)
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
C-----------------------------------------------------------------------
C  MAKE THE FIRST GUESS FOR THE VELOCITIES
C-----------------------------------------------------------------------
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
   20 DEN(I,J)=1./(1.+(VM(I,J)*ASY(I,J))**2*BU(I,J)*BV(I,J))
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
C-----------------------------------------------------------------------
C  DETERMINE THE RELAXATION FACTOR FOR THE RESIDUAL
C-----------------------------------------------------------------------
      IF ((MRELAX.GT.50).OR.(MRELAX.EQ.0))THEN
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
       CALL DDX(U,V,ZETA,LN,K, TMP)
       DO 125 J=2,MM
        IB=MOD(J+K,2)+2
        DO 125 I=IB,LM,2
  125   RU(I,J)=TMP(I,J)+TMP(I+1,J)
C  ENTER D{ZETA*[D(U)/DX+D(V)/DY]}/DY TO V EQUATION:
       CALL DDY(V,U,ZETA,LN,K, TMP)
       DO 130 J=2,MM
        IB=MOD(J+K,2)+2
        DO 130 I=IB,LM,2
  130   RV(I,J)=TMP(I,J)+TMP(I+1,J)
C-----------------------------------------------------------------------
C  NEXT DIFFERENTIATE THE SHEAR VISCOSITIES
C-----------------------------------------------------------------------
C  ADD D{ETA*[D(U)/DX-D(V)/DY]}/DX TO U EQUATION:
       CALL DDX(U,V,ETA,LN,K, TMP)
       DO 135 J=2,MM
        IB=MOD(J+K,2)+2
        DO 135 I=IB,LM,2
  135   RU(I,J)=RU(I,J)+TMP(I,J)-TMP(I+1,J)
C  ADD D{ETA*[D(V)/DY-D(U)/DX]}/DY TO V EQUATION:
       CALL DDY(V,U,ETA,LN,K, TMP)
       DO 140 J=2,MM
        IB=MOD(J+K,2)+2
        DO 140 I=IB,LM,2
  140   RV(I,J)=RV(I,J)+TMP(I,J)-TMP(I+1,J)
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DY TO U EQUATION:
       CALL DDY(U,V,ETA,LN,K, TMP)
       DO 145 J=2,MM
        IB=MOD(J+K,2)+2
        DO 145 I=IB,LM,2
  145   RU(I,J)=RU(I,J)+TMP(I,J)+TMP(I+1,J)
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DX TO V EQUATION:
       CALL DDX(V,U,ETA,LN,K, TMP)
       DO 150 J=2,MM
        IB=MOD(J+K,2)+2
        DO 150 I=IB,LM,2
  150   RV(I,J)=RV(I,J)+TMP(I,J)+TMP(I+1,J)
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
C**WE HAVE SUCCEEDED IN FINDING A NEW SOLUTION:
      RETURN
C**WE HAVE NOT FOUND A SOLUTION AFTER MMAX ITERATIONS:
  400 CONTINUE
      WRITE (*,2000) IIC,MRELAX
      STOP
 2000 FORMAT ('AT TIME STEP',I6,'AFTER',I3,'ITERATIONS, NO SOLUTION
     1        OBTAINED')
      END
      SUBROUTINE RELCON(LOLD,LRHS)
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
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/VEL/U(L,M,3), V(L,M,3)
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/FRWND/CDWIN, SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      COMMON/PRESS/P(L,M)
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/WORK/TMP(1:L,1:M), WRK(1:L,1:M,3), AMAS(1:L,1:M),
     1  BU(1:L,1:M), BV(1:L,1:M), FX(1:L,1:M),
     1  FY(1:L,1:M), ASY(1:L,1:M), ZETA(1:L,1:M), ETA(1:L,1:M)
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
       CDWIN=.25*(CD(I,J)+CD(I-1,J)+CD(I,J-1)+CD(I-1,J-1))*(1.-ABLFIX)
     1           +CDWIN*ABLFIX
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
      RETURN
      END
      SUBROUTINE STRAIN(LRHS, E11, E12, E22)
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
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LRHS
      COMMON/VEL/U(L,M,3),V(L,M,3)
      REAL U, V
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
C=======================================================================
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
      SUBROUTINE SHDEF(LRHS, OPEW)
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
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
C     COMMON viscp moved to rheology.inc
      INTEGER LRHS
      COMMON/WORK/TMP(1:L,1:M), E11(1:L,1:M), E22(1:L,1:M), E12(1:L,1:M)
     1 ,  SPACE(1:L,1:M,8)
      REAL TMP, E11, E22, E12, SPACE
      REAL OPEW(0:L,0:M)
      INTEGER I, J
      REAL DELT, DELT1
C=======================================================================
      CALL STRAIN(LRHS, E11, E12, E22)
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
      PARAMETER (DLAT   =   2.5)
      PARAMETER (DLON   =   5.0)
      PARAMETER (PTYPE  = 3         )
      PARAMETER (POLEI  = 37.)
      PARAMETER (POLEJ  = 44.)
      PARAMETER (DXDEG  = 111.1E+3)
      PARAMETER (DX     = 127.E+3 )
      PARAMETER (DY     = 127.E+3 )
      PARAMETER (DT     = 4.32E4    )
      INTEGER NLEVEL
      PARAMETER (NLEVEL = 7)
C     Meteorological model dependent variables
      REAL dlatm, dlonm, dtm
      INTEGER nwave, mwave, kdim
      PARAMETER (dlatm  = 1.0     )
      PARAMETER (dlonm  = 1.0     )
      PARAMETER (dtm    = 12.*3600.)
      PARAMETER (nwave  = 126      )
      PARAMETER (mwave  = (nwave+1)*(nwave+2))
      PARAMETER (kdim   = 18       )
C     Ocean mixed layer model parameters
      REAL QHW, QHS, ENTMAX, SICE, CW, BETAS, BETAT
      REAL EPSAA, HMLREF
      PARAMETER (QHW    = 7.0)
      PARAMETER (QHS    = 50.)
      PARAMETER (ENTMAX = 30.)
      PARAMETER (SICE   = 5.0)
      PARAMETER (CW     = 0.005)
      PARAMETER (BETAS  = 8.E-4)
      PARAMETER (BETAT  = 4.E-5)
      PARAMETER (EPSAA  = 2.E-6)
      PARAMETER (HMLREF = 500.0) 
C     Physical Parameters
      REAL F0, RHOICE, RHOSNO, CON, CONSN, TMELT, TFREZ, CC
      REAL CLO, CLB, RHOAIR, RHOWAT, CDWAT, VAPL, SUBL
      REAL D3, SIGMA, ALBI, ALBM, ALBW, ALBSN, ALBSNM, ZOI, ZOWI
      REAL CPAIR, KAPPA, CSENS, CLAT, EPSI, GRAV
      REAL RGAS, VONKAR, ATMLEV, OMEGA, LFUSE
      PARAMETER (CPAIR  = 1004.)
      PARAMETER (KAPPA  = 2./7.)
      PARAMETER (RGAS   = 8.31436E3/28.964)
      PARAMETER (VONKAR = 0.4)
      PARAMETER (CSENS  = 1.75E-3)
      PARAMETER (CLAT   = 1.75E-3)
      PARAMETER (EPSI   = 0.6219886)  !CRC #72
      PARAMETER (GRAV   = 9.80 )
      PARAMETER (OMEGA  = 7.292E-5)
      PARAMETER (F0     = 2.*OMEGA)
      PARAMETER (RHOICE = 9.1E2)
      PARAMETER (RHOSNO = 3.3E2)
      PARAMETER (CON    = 2.1656)
      PARAMETER (CONSN  = 0.31)
      PARAMETER (TMELT  = 273.15)
      PARAMETER (TFREZ  = -1.84)   !Tf for water at 34.5 psu, Gill
      PARAMETER (CC     = 4.217E6) !Pure water, Gill
      PARAMETER (VAPL   = 2.5008E6 ) !Gill 0C
      PARAMETER (SUBL   = 2.83459E6) !Gill 0C
      PARAMETER (LFUSE  = SUBL-VAPL)
      PARAMETER (CLO    = LFUSE*RHOICE)
      PARAMETER (CLB    = 0.9*LFUSE*RHOICE)
      PARAMETER (RHOAIR = 1.29)
      PARAMETER (RHOWAT = 1.028E3)
      PARAMETER (CDWAT  = 5.5E-3)
      PARAMETER (SIGMA  = 5.67E-8 ) !STEFAN-BOLTZMANN PARAMETER
      PARAMETER (D3     = 0.97*SIGMA)
      PARAMETER (ALBI   = 0.75)
      PARAMETER (ALBM   = 0.66)
      PARAMETER (ALBW   = 0.10)
      PARAMETER (ALBSN  = 0.85)
      PARAMETER (ALBSNM = 0.75)
      PARAMETER (ZOI    = 1.E-3)
      PARAMETER (ZOWI   = 1.E-4)
      PARAMETER (ATMLEV = 8.5E4)
      REAL PSTAR, ECCEN, ECM2, CSTAR, ZMAX, XMIN, GMIN, HNU, HNU2
      REAL MMAX, VRMAX, WT, ZMIN
      PARAMETER (PSTAR = 5.0E3)
      PARAMETER (ECCEN = 2.0)
      PARAMETER (ECM2  = 1./ECCEN/ECCEN)
      PARAMETER (CSTAR = 20.)
      PARAMETER (ZMAX  = 2.5E8)  !Note that this multiplies P.
      PARAMETER (ZMIN  = 4.0E8)  !While this is the actual value.
      PARAMETER (GMIN  = 1.E-20)
      PARAMETER (MMAX  = 800  )
      PARAMETER (VRMAX = 5.E-6)
      PARAMETER (WT    = 1.3  ) 
