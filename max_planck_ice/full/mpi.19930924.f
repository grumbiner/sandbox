C=======================================================================
C=========================SIMOD11=======================================
C=======================================================================
C  Cycles:
C     -The model can alternatively been run for cycles 6, 7, 8 and 11.
C     -The present cycle is indicated in the head line and is deter-
C        mined by the following characteristics:
C          -ABLFIX=0. and SURFWIN=0. and ECMTYP=0. for CYCLE 6.
C          -ABLFIX=0. and SURFWIN=1. and ECMTYP=0. for CYCLE 7.
C          -ABLFIX=1. and SURFWIN=1.               for CYCLE 8.
C          -ABLFIX=0. and SURFWIN=1. and ECMTYP=1. for CYCLE 11.
C=======================================================================
C  Current application:
C     -02.08.91: - 30-day run, cycle 11
C=======================================================================
      PROGRAM ICEMODEL(INPUT,OUTPUT,TAPE10,TAPE11,TAPE12,TAPE20,TAPE13,
     1TAPE15,TAPE16,TAPE17,TAPE18,TAPE19,TAPE8,TAPE14)
C=======================================================================
C  Programmed by:
C     W.Brechner Owens      MPI, Hamburg                          Aug.87
C  Modified by:
C     Achim Stoessel        MPI, Hamburg                          May 91
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
C        ---->==>FORFLD=============>BCSQ
C        |  <-==>PRESS(LOLD)
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
C        |                        ==>OBUDGET/ECMBUDO/EKMAO
C        |                        ==>BUDGET/ECMBUDI/EKMAH
C        |                        ==>PMLEX
C        |    ==>OUTSM(LNEW)
C        |    ==>BCSH(LNEW)
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
C     -PRESS:   calculates ice strength
C     -INITREL: performes initial relaxation to balance initial values
C     -PLAST:   calculates viscosities
C     -RELCON:  calculates diagnostic terms of momentum balance
C     -RELAX:   solves momentum balance by overrelaxation
C     -SADVECT: calculates advection terms for continuity equation
C     -SDIFFUS: calculates diffusion terms for continuity equation
C     -GROWTH:  represents thermodynamic part of the model
C     -DRUCKF:  prints integer results in domain-mask shape
C  References:
C     -Hibler,W.D.,III(1979): A dynamic thermodynamic sea ice model.
C         J.Phys.Oceanogr.,9, 815-846.
C     -Hibler,W.D.(1980): Documentation for a two-level dynamic-thermo-
C         dynamic sea ice model. CRREL, Hanover, N.H., Spec. Rep., 80-8.
C     -Hibler,W.D.(1984): The role of sea ice dynamics in modeling CO2
C         increases. In: Climate processes and climate sensitivity,
C         eds.: J.Hansen and T.Takahashi, Geophys.Monogr.,29,238-253.
C     -Hockney,R.W. and C.R.Jesshope (1981): Parallel computers, archi-
C         tecture, programming and algorithms. Adam Hilger LTD, Bristol.
C     -Koch,C.(1988): A coupled sea ice - atmospheric boundary layer
C         model. Part I: Description of the model and 1979 standard run.
C         Beitr.Phys.Atmosph., 61(4), 344-354.
C     -Lemke,P., W.B.Owens and W.D.Hibler III(1990): A coupled sea ice -
C         mixed layer - pycnocline model for the Weddell Sea. J.Geophys.
C         Res., 95(C6), 9513-9525.
C     -Leppaeranta,M. and W.D.Hibler III (1985): The role of plastic ice
C         interaction in marginal ice zone dynamics. J.Geophys.Res., 90,
C         (C6), 11899-11909.
C     -Louis,J.F.(1979): A parametric model of vertical eddy fluxes in
C         the atmosphere. Bound.Layer Met.,17,187-202.
C     -Mesinger,F. and A.Arakawa (1976): Numerical Methods used in atmo-
C         spheric models. GARP Publ.Ser.No.17.
C     -Owens,W.B. and P.Lemke(1990): Sensitivity Studies with a sea ice-
C         mixed layer - pycnocline model in the Weddell Sea. J.Geophys.
C         Res., 95(C6), 9527-9538.
C     -Stoessel,A., P.Lemke and W.B.Owens(1990): Coupled sea ice - mixed
C         layer simulations for the Southern Ocean. J.Geophys.Res., 95
C         (C6), 9539-9555.
C     -Stoessel,A.(1990): Meereismodellierung im Suedlichen Ocean. Max-
C         Planck-Inst.f.Meteorologie, Hamburg, Examensarbeit Nr.6.
C=======================================================================
      PARAMETER(L=74, M=14, LM=L-1, MM=M-1, LP=L+1, MP=M+1, LDO=4*L)
C=======================================================================
C  Parameter:
C     -L: number of grid points in X-direction (even number only!)
C     -M: number of grid points in Y-direction (even number only!)
C=======================================================================
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
C=======================================================================
C  Common CORR: contains coriolis parameter related variables
C     -FM:      coriolis parameter for grid center [1/s]
C     -F:       coriolis parameter for grid edge [1/s]
C     -COSPHI:  COS of grid center latitude []
C     -SINPHI:  SIN of grid center latitude []
C=======================================================================
      COMMON/IPARM/RHOICE,H0,HNU,HNU2,ARMIN,ARMAX,HMIN,RHOSNO,CC
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
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
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
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
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
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
C=======================================================================
C  Common COORD: contains metric coefficients
C     -PM:      metric coefficients for X-direction []
C     -PN:      metric coefficients for Y-direction []
C     -DNDX:    metric term for centripetal forces in Y-direction []
C     -DMDX:    metric term for centripetal forces in X-direction []
C=======================================================================
      COMMON/VEL/U(L,M,3),V(L,M,3)
C=======================================================================
C  Common VEL: ice velocity field (defined st grid edge points)
C     -U:       X-component of ice velocity [m/s]
C     -V:       Y-component of ice velocity [m/s]
C=======================================================================
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
C=======================================================================
C  Common THCK: thickness fields (defined on grid center points)
C     -H:       mean ice thickness [m]
C     -A:       ice compactness [100%=1]
C     -HSN:     snow thickness [m]
C=======================================================================
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
C=======================================================================
C  Common FRWND: atmospheric parameters and wind forcing
C     -RHOAIR:  air density [kg/m**3]
C     -CDWIN:   atmospheric drag coefficient []
C     -SINWIN:  SIN of ABL turning angle []
C     -COSWIN:  COS of ABL turning angle []
C     -UWIN:    X-component of wind velocity [m/s]
C     -VWIN:    Y-component of wind velocity [m/s]
C=======================================================================
      COMMON/FRWAT/RHOWAT,CDWAT,SINWAT,COSWAT,UWAT(L,M),VWAT(L,M)
C=======================================================================
C  Common FRWAT: oceanic parameters and current forcing
C     -RHOWAT:  water density [kg/m**3]
C     -CDWAT:   oceanic drag coefficient []
C     -SINWAT:  SIN of OBL turning angle []
C     -COSWAT:  COS of OBL turning angle []
C     -UWAT:    X-component of current velocity [m/s]
C     -VWAT:    Y-component of current velocity [m/s]
C=======================================================================
      COMMON/THFOR/TAIR(0:L,0:M),TD(0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
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
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
C=======================================================================
C  Common THPAR: contains various thermodynamic parameters
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
      COMMON/VISCP/PSTAR,CSTAR,ECCEN,ZMAX,ZMIN,GMIN,ECM2
C=======================================================================
C  Common VISCP: contains viscosity parameters
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
      COMMON/TEMPM/TICM(0:L,0:M,7)
C=======================================================================
C  Common TEMPM: surface temperatures for seven-level heat balance
C     -TICM:    surface temp. for seven ice thickness categories [C]
C=======================================================================
      COMMON/RELAXP/MMAX,VRMAX,WT
C=======================================================================
C  Common RELAXP: parameters for overrelaxation routine
C     -MMAX:    maximum iteration steps []
C     -VRMAX:   cut off velocity difference between iteration steps[m/s]
C     -WT:      relaxation factor []
C=======================================================================
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
C=======================================================================
C  Common MASK: contains definition points for specific domain
C     -VM:      mask for grid edge points []
C     -HM:      mask for grid center points []
C     -OM:      mask to separate outflow grid points []
C     -FLM:     mask for fluxes normal to boundaries []
C=======================================================================
      COMMON/PML/QS(0:L,0:M),QT(0:L,0:M),QH(0:L,0:M),QSB(0:L,0:M),
     1QTB(0:L,0:M),QHB(0:L,0:M),QDS(0:L,0:M),QDT(0:L,0:M),
     2QHSTO(0:L,0:M),HS(0:L,0:M),HT(0:L,0:M),QV(0:L,0:M),QRHO(0:L,0:M),
     3QW(0:L,0:M),IEN(0:L,0:M),FW(0:L,0:M),MLFIX
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
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
C=======================================================================
C  Common GEO: parameters for solar radiation
C     -PI:      circle number []
C     -RAD:     factor for conversion of degree into radiant (PI/180)[]
C     -SOL:     solar constant times factor for sun distance [W/m**2]
C     -COSZ:    cosine of zenith distance of the sun []
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
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
      COMMON/TAU/CD(0:L,0:M),SINBET(0:L,0:M),COSBET(0:L,0:M),
     1BETA(0:L,0:M),TAUX(L,M),TAUY(L,M)
C=======================================================================
C  Common TAU: variables of ABL model derived dynamic forcing of sea ice
C     -CD:      drag coefficient []
C     -SINBET:  SIN of wind turning angle []
C     -COSBET:  COS of wind turning angle []
C     -BETA:    wind turning angle [deg]
C     -TAUX:    X-component of wind stress [N/m**2]
C     -TAUY:    Y-component of wind stress [N/m**2]
C=======================================================================
      COMMON/FLUX/FLSE(0:L,0:M),FLLA(0:L,0:M),WMUE1(0:L,0:M)
     1,UST1(0:L,0:M),TMPL1(0:L,0:M)
C=======================================================================
C  Common FLUX: heat flux and ABL stability related variables
C     -FLSE:    sensible heat flux [W/m**2]
C     -FLLA:    latent heat flux [W/m**2]
C     -WMUE1:   stability parameter []
C     -UST1:    friction velocity [m/s]
C     -TMPL1:   Richardson number or mod.Monin-Obukhov length []or[1/m]
C=======================================================================
      COMMON/OUTFLOW/NOUT,IOUT(LDO),JOUT(LDO)
C=======================================================================
C  Common OUTFLOW: contains outflow cells
C     -NOUT:    number of outflow cells []
C     -IOUT:    X-coordinate of outflow cell []
C     -JOUT:    Y-coordinate of outflow cell []
C=======================================================================
      COMMON/PMLPARM/SICE,QHW,QHS,DCVM,ENTMAX,WUP,CW,COSGAM,BETAS,BETAT,
     1EPSAA,RTC,STC,QTOC
C=======================================================================
C  Common PMLPARM: contains parameters for OML model
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
      COMMON/WORK/TMP(0:L,0:M),RH(0:L,0:M),RA(0:L,0:M),DFX(0:L,0:M),
     1DFY(0:L,0:M),RSN(0:L,0:M),WRK(0:L,0:M,4),QTM(0:L,0:M),SH(0:L,0:M)
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
      DIMENSION HOSM(2),HOSNSM(2),FLAGI1(L,M),FLAGI2(L,M),
     1FLAGI(0:L,0:M),SHA(0:L,0:M),QTMA(0:L,0:M),FRS(0:L,0:M),
     2SB(0:L,0:M),BM(0:L,0:M),SHM(0:L,0:M),HMM(0:L,0:M),STAUX(L,M),
     3STAUY(L,M),STAUM(0:L,0:M),TAUM(0:L,0:M),SU(L,M),UMM(L,M),SV(L,M),
     4VMM(L,M),STA(0:L,0:M),SQTM(0:L,0:M),SFLSE(0:L,0:M),STICM(0:L,0:M)
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
      CALL OUTSM(H,1,HOSUM,HOSM(1))
      CALL OUTSM(A,1,SCR1,SCR2)
      CALL OUTSM(HSN,1,HSNOSUM,HOSNSM(1))
C-----------------------------------------------------------------------
C  INITIAL CALL FOR CYCLIC BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      CALL BCSH(H,LOLD)
      CALL BCSH(A,LOLD)
      CALL BCSH(HSN,LOLD)
      CALL BCSV(U,V,LOLD)
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
       FLAG=.5*(1.-SIGN(1.,.15-A(I,J,1)))
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
      WRITE(*,901)
      WRITE(*,903)
      WRITE(16,901)
      WRITE(16,903)
      WRITE(*,902)0,HSUM,HEX,HWEX,HSNSUM
      WRITE(16,902)0,HSUM,HEX,HWEX,HSNSUM
C-----------------------------------------------------------------------
C----------------------- MAIN COMPUTATIONAL LOOP -----------------------
C-----------------------------------------------------------------------
      DO 400 IIC=1,NTMES
       T=T+DT
C-----------------------------------------------------------------------
C  FIRST GET THE FORCING FIELDS FOR THIS TIME STEP
C-----------------------------------------------------------------------
       CALL FORFLD
C-----------------------------------------------------------------------
C  CALCULATE ICE PRESSURE WITH OLD VALUES OF ICE THICKNESSES
C-----------------------------------------------------------------------
       CALL PRESS(LOLD)
C-----------------------------------------------------------------------
C  NOW DO VELOCITY INTEGRATION
C**REMARK: IF FIRST TIME, THEN DO INITIAL RELAXATION TO ADJUST VELOCI-
C    TIES TO VISCOUS STRESSES AND FORCING FIELDS.
C-----------------------------------------------------------------------
       IF(T.EQ.DT.AND.NRREC.EQ.0)THEN
        CALL INITREL
       END IF
C-----------------------------------------------------------------------
C  DO THE INTEGRATION IN 2 PARTS, FIRST A PREDICTIVE TIMESTEP FOLLOWED
C    BY A CENTERED TIMESTEP
C-----------------------------------------------------------------------
       DO 60 JJC=1,2
        IF(JJC.EQ.1)THEN
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
         DO 40 J=2,MM
         DO 40 I=1,L
          U(I,J,LRHS)=.5*(U(I,J,LRHS)+U(I,J,LOLD))
          V(I,J,LRHS)=.5*(V(I,J,LRHS)+V(I,J,LOLD))
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
        CALL RELCON(LOLD,LRHS)
C-----------------------------------------------------------------------
C  NOW SOLVE FOR THE NEW VELOCITIES, USING OVER-RELAXATION
C-----------------------------------------------------------------------
        CALL RELAX(LRHS,LLHS)
   60  CONTINUE
C-----------------------------------------------------------------------
C  NOW START TO SOLVE THE CONTINUITY EQUATIONS, FIRST USING A FORWARD
C    (EULER) TIME STEP AND THEN A BACKWARD TIME STEP (FORWARD-BACKWARD
C    (MATSUNO) SCHEME)
C-----------------------------------------------------------------------
       DO 120 JJC=1,2
        IF(JJC.EQ.1)THEN
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
        CALL SADVECT(RH,H,U,V,LRHS,LNEW)
        CALL SADVECT(RA,A,U,V,LRHS,LNEW)
        CALL SADVECT(RSN,HSN,U,V,LRHS,LNEW)
C-----------------------------------------------------------------------
C  ADD IN HORIZONTAL DIFFUSION
C-----------------------------------------------------------------------
        IF(JJC.EQ.2)THEN
         CALL SDIFFUS(RH,H,LOLD)
         CALL SDIFFUS(RA,A,LOLD)
         CALL SDIFFUS(RSN,HSN,LOLD)
        END IF
C-----------------------------------------------------------------------
C  CALCULATE NEW THICKNESSES AND COMPACTNESS
C-----------------------------------------------------------------------
        DO 110 J=1,MM
        DO 110 I=0,L
         H(I,J,LNEW)=H(I,J,LOLD)+DELT*RH(I,J)
         A(I,J,LNEW)=A(I,J,LOLD)+DELT*RA(I,J)
         HSN(I,J,LNEW)=HSN(I,J,LOLD)+DELT*RSN(I,J)
  110   CONTINUE
C-----------------------------------------------------------------------
C  RESET HORIZONTAL BOUNDARY CONDITIONS FOR H AND A
C-----------------------------------------------------------------------
        CALL BCSH(H,LNEW)
        CALL BCSH(A,LNEW)
        CALL BCSH(HSN,LNEW)
  120  CONTINUE
C-----------------------------------------------------------------------
C  KINETIC ENERGY FOR MIXED LAYER
C-----------------------------------------------------------------------
       DO 125 J=1,MM
       DO 125 I=1,L
        QV(I,J)=(.25*(SQRT(U(I,J,LNEW)**2+V(I,J,LNEW)**2)
     1               +SQRT(U(I+1,J,LNEW)**2+V(I+1,J,LNEW)**2)
     2               +SQRT(U(I,J+1,LNEW)**2+V(I,J+1,LNEW)**2)
     3               +SQRT(U(I+1,J+1,LNEW)**2+V(I+1,J+1,LNEW)**2))
     4               +.01)**3*OM(I,J)
  125  CONTINUE
C-----------------------------------------------------------------------
C  NOW DO THE THERMODYNAMIC GROWTH CALCULATIONS FOR H AND A
C-----------------------------------------------------------------------
       CALL GROWTH(LOLD,LNEW)
C-----------------------------------------------------------------------
C  SMOOTH OUTFLOW THICKNESSES AND COMPACTNESSES
C-----------------------------------------------------------------------
       CALL OUTSM(H,LNEW,HOSUM,HOSM(LNEW))
       CALL OUTSM(A,LNEW,SCR1,SCR2)
       CALL OUTSM(HSN,LNEW,HOSNSUM,HOSNSM(LNEW))
C-----------------------------------------------------------------------
C  RESET HORIZONTAL BOUNDARY CONDITIONS FOR H AND A
C-----------------------------------------------------------------------
       CALL BCSH(H,LNEW)
       CALL BCSH(A,LNEW)
       CALL BCSH(HSN,LNEW)
C-----------------------------------------------------------------------
C----- THE REST OF THE COMP. LOOP IS RESERVED FOR VARIOUS OUTPUTS ------
C-----------------------------------------------------------------------
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
C**NEXT COMPUTATIONS DO NOT START BEFORE SPECIFIED TIME STEP:
       IF(IIC.GE.NFLD)THEN
C-----------------------------------------------------------------------
C  SELECT DAYS FOR INSTANTANEOUS ICE COMPACTNESS PLOTS
C-----------------------------------------------------------------------
        IF(IIC.EQ.1883.OR.IIC.EQ.1974.OR.IIC.EQ.2065.OR.IIC.EQ.2155)THEN
         WRITE(20,974)'A',IIC,T
         DO 330 J=1,MM
  330    WRITE(20,973)(A(I,J,LNEW),I=2,LM)
        END IF
C-----------------------------------------------------------------------
C  SUMMATION OF VARIABLES FOR FINAL PLOTS
C-----------------------------------------------------------------------
        DO 131 J=1,M
        DO 131 I=1,L
         FRS(I,J)=FRS(I,J)+FW(I,J)
         SB(I,J)=SB(I,J)+A(I,J,LNEW)
         SHM(I,J)=SHM(I,J)+H(I,J,LNEW)
         STAUX(I,J)=STAUX(I,J)+TAUX(I,J)
         STAUY(I,J)=STAUY(I,J)+TAUY(I,J)
         STA(I,J)=STA(I,J)+TA(I,J)
         SU(I,J)=SU(I,J)+U(I,J,LNEW)
         SV(I,J)=SV(I,J)+V(I,J,LNEW)
         SQTM(I,J)=SQTM(I,J)+QTM(I,J)
         SFLSE(I,J)=SFLSE(I,J)+FLSE(I,J)
         STICM(I,J)=STICM(I,J)+TICM(I,J,4)
         STAUM (I,J)=STAUM (I,J)+SQRT(TAUX(I,J)**2+TAUY(I,J)**2)
  131   CONTINUE
       END IF
C**NEXT COMPUTATIONS START AT SPECIFIED STATISTICS INTERVAL ONLY:
       IF(MOD(IIC,NSTAT).EQ.0)THEN
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
         FLAG=.5*(1.-SIGN(1.,.15-A(I,J,LNEW)))
         FLAG1=.5*(1.-SIGN(1.,.85-A(I,J,LNEW)))
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
        WRITE(18,960)HSUM,HEX,HEX1,HWEX
C**NEXT COMPUTATIONS DO NOT START BEFORE SPECIFIED TIME STEP:
        IF(IIC.GE.NFLD)THEN
C-----------------------------------------------------------------------
C  WRITE OUT STATISTICS
C-----------------------------------------------------------------------
         WRITE(*,902)IIC,HSUM,HEX,HWEX,HSNSUM,HOSUM,HOTSUM,QTMSUM,SHSUM
         WRITE(16,902)IIC,HSUM,HEX,HWEX,HSNSUM,HOSUM,HOTSUM,QTMSUM,SHSUM
C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
         IF(MOD((IIC+5),NSTA).EQ.0)THEN
C-----------------------------------------------------------------------
C  PRINT OUT GEOGRAPHICAL PATTERNS
C-----------------------------------------------------------------------
          CALL DRUCKF(H(0,0,LNEW),OM,10.,0.,'H*10 [M]',IIC,LP,MP)
          CALL DRUCKF(A(0,0,LNEW),OM,100.,0.,'A [%]',IIC,LP,MP)
          CALL DRUCKF(U(1,1,LNEW),VM,100.,0.,'U [CM/S]',IIC,L,M)
          CALL DRUCKF(V(1,1,LNEW),VM,100.,0.,'V [CM/S]',IIC,L,M)
          CALL DRUCKF(HSN(0,0,LNEW),OM,100.,0.,'HSNOW [CM]',IIC,LP,MP)
          CALL DRUCKF(QT,OM,10.,0.,'M L TEMP*10 [DEG C]',IIC,LP,MP)
          CALL DRUCKF(TICM(0,0,4),OM,1.,0.,'ICE TEMP [DEG C]',IIC,LP,MP)
C         CALL DRUCKF(TA,OM,1.,-273.15,'SURF.AIR TEMP.[C]',IIC,LP,MP)
C         CALL DRUCKF(UST1,OM,100.,0.,'UST *100',IIC,LP,MP)
C         CALL DRUCKF(WMUE1,OM,1.,0.,'MUE',IIC,LP,MP)
          CALL DRUCKF(QS,OM,10.,-300.,'SALT M L *10 -300',IIC,LP,MP)
          CALL DRUCKF(QH,OM,1.,0.,'M L DEPTH [M]',IIC,LP,MP)
          CALL DRUCKF(QTMA,OM,1.,0.,'OC. HEAT FLUX [W/M**2]',IIC,LP,MP)
          CALL DRUCKF(SHA,OM,.1,0.,'ATM. HEAT FL./10[W/M**2]',IIC,LP,MP)
          WRITE(16,901)
         END IF
C**NEXT OUTPUTS ARE RELEASED AFTER SPECIFIC TIME INTERVAL:
C**REMARK: MOD. FOR REAL-TIME DAILY FORCING: 6TH YEAR:+5; 5TH YEAR:+20.
         IF(MOD((IIC+5),NPLT).EQ.0)THEN
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
           FLAG1=.5*(1.-SIGN(1.,.85-BM(I,J)))
           HEX2=HEX2+FLAG1*OM(I,J)*Z1*1.E-6/(PN(I,J)*PM(I,J))
           HWEX2=HWEX2+OM(I,J)*Z1*1.E-6*BM(I,J)/(PN(I,J)*PM(I,J))
  309     CONTINUE
C-----------------------------------------------------------------------
C  WRITE MODEL OUTPUT FOR PLOTTING
C-----------------------------------------------------------------------
          WRITE(15,974)'U',IIC,T
          DO 310 J=1,MM
  310     WRITE(15,973)(U(I,J,LNEW)*FLAGI1(I,J),I=2,LM)
          WRITE(15,974)'V',IIC,T
          DO 312 J=1,MM
  312     WRITE(15,973)(V(I,J,LNEW)*FLAGI1(I,J),I=2,LM)
          WRITE(15,974)'UM',IIC,T
          DO 311 J=1,MM
  311     WRITE(15,973)(UMM(I,J)*FLAGI2(I,J),I=2,LM)
          WRITE(15,974)'VM',IIC,T
          DO 313 J=1,MM
  313     WRITE(15,973)(VMM(I,J)*FLAGI2(I,J),I=2,LM)
          WRITE(15,974)'TAUX',IIC,T
          DO 315 J=1,MM
  315     WRITE(15,973)(TAUX(I,J),I=2,LM)
          WRITE(15,974)'TAUY',IIC,T
          DO 317 J=1,MM
  317     WRITE(15,973)(TAUY(I,J),I=2,LM)
          WRITE(17,974)'H',IIC,T
          DO 314 J=1,MM
  314     WRITE(17,973)(H(I,J,LNEW),I=2,LM)
          WRITE(17,974)'A',IIC,HEX2
          DO 316 J=1,MM
  316     WRITE(17,973)(A(I,J,LNEW),I=2,LM)
C         WRITE (17,974)'BM',IIC,HWEX2
C         DO 317 J=1,MM
C 317     WRITE(17,973)(BM(I,J),I=2,LM)
          WRITE (17,974)'TICM',IIC,T
          DO 318 J=1,MM
  318     WRITE(17,973)(TICM(I,J,4)*FLAGI(I,J),I=2,LM)
          WRITE(17,974)'TA',IIC,T
          DO 320 J=1,MM
  320     WRITE(17,973)(TA(I,J),I=2,LM)
          WRITE(17,974)'FLSE',IIC,T
          DO 322 J=1,MM
  322     WRITE(17,973)(FLSE(I,J),I=2,LM)
          WRITE(17,974)'QH',IIC,T
          DO 324 J=1,MM
  324     WRITE(17,973)(QH(I,J),I=2,LM)
          WRITE(17,974)'QTMA',IIC,T
          DO 326 J=1,MM
  326     WRITE(17,973)(QTMA(I,J)*FLAGI2(I,J),I=2,LM)
          WRITE(17,974)'TAUM',IIC,T
          DO 328 J=1,MM
  328     WRITE(17,973)(TAUM(I,J),I=2,LM)
          IF(MOD(IIC,NTMES).EQ.0)THEN
           WRITE(17,974)'FRS',IIC,T
           DO 329 J=1,MM
  329      WRITE(17,973)(FRS(I,J),I=2,LM)
          END IF
         END IF
        END IF
C-----------------------------------------------------------------------
C  ZERO OUT AVERAGING ARRAYS
C-----------------------------------------------------------------------
        DO 390 J=0,M
        DO 390 I=0,L
         QTMA(I,J)=.0
         SHA(I,J)=.0
  390   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  WRITE OUT RESTART DATA
C-----------------------------------------------------------------------
C      IF(MOD(IIC,NRST).EQ.0)THEN
C       WRITE(14,9101)((U(I,J,LNEW),V(I,J,LNEW),I=1,L),J=1,M)
C       WRITE(14,9101)((H(I,J,LNEW),A(I,J,LNEW),I=0,L),J=0,M)
C       WRITE(14,9101)((HSN(I,J,LNEW),TICE(I,J),I=0,L),J=0,M)
C       WRITE(14,9101)((QT(I,J),QS(I,J),QH(I,J),I=0,L),J=0,M)
C       WRITE(14,9101)((QTB(I,J),QSB(I,J),QHB(I,J),I=0,L),J=0,M)
C       WRITE(14,9101)((QDT(I,J),QDS(I,J),I=0,L),J=0,M)
C       WRITE(14,9101)(((TICM(I,J,K),I=0,L),J=0,M),K=1,7)
C       NRREC=NRREC+1
C      END IF
C-----------------------------------------------------------------------
C  GET READY FOR NEW TIME STEP: SWITCH OLD AND NEW INDICES
C-----------------------------------------------------------------------
       LOLD=LNEW
       LNEW=3-LOLD
  400 CONTINUE
C-----------------------------------------------------------------------
C---------------------- END OF COMPUTATIONAL LOOP ----------------------
C-----------------------------------------------------------------------
      WRITE(*,980)NRREC-NOLRREC
      STOP
  901 FORMAT(' TIME',3X,' ICE  ',2X,' ICE  ',2X,' ICE ',3X,' SNOW ',2X,
     1       ' ICE OUTFLOW ',2X,'AVG. HEAT FLUXES'/
     2       ' STEP',3X,'VOLUME',2X,'EXTENT',2X,' AREA',3X,'VOLUME',2X,
     3       '        CUMM ',2X,' OCEAN    ATMOS ')
  903 FORMAT(8X,        '10**3 ',2X,'10**6 ',2X,'10**6',3X,'10**3 ',2X,
     1       '10**2   10**3',2X,'            10  '/
     2       8X,        'KM**3 ',2X,'KM**2 ',2X,'KM**2',3X,'KM**3 ',2X,
     3       'KM**3   KM**3',2X,'W/M**2    W/M**2')
  902 FORMAT(1X,I4,8F8.4)
  973 FORMAT(13E10.3)
  974 FORMAT(2X,A5,I8,E12.5)
  960 FORMAT(5F8.4)
 9101 FORMAT(5E13.4)
  980 FORMAT(' ',I5,' RESTART RECORDS WRITTEN')
      END
C=======================================================================
      SUBROUTINE INIT
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LDO=4*L)
C=======================================================================
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
      COMMON/IPARM/RHOICE,H0,HNU,HNU2,ARMIN,ARMAX,HMIN,RHOSNO,CC
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      COMMON/VEL/U(L,M,3),V(L,M,3)
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      COMMON/FRWAT/RHOWAT,CDWAT,SINWAT,COSWAT,UWAT(L,M),VWAT(L,M)
      COMMON/THFOR/TAIR(0:L,0:M),TD(0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/VISCP/PSTAR,CSTAR,ECCEN,ZMAX,ZMIN,GMIN,ECM2
      COMMON/TEMP/TICE(0:L,0:M)
      COMMON/TEMPM/TICM(0:L,0:M,7)
      COMMON/RELAXP/MMAX,VRMAX,WT
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/PML/QS(0:L,0:M),QT(0:L,0:M),QH(0:L,0:M),QSB(0:L,0:M),
     1QTB(0:L,0:M),QHB(0:L,0:M),QDS(0:L,0:M),QDT(0:L,0:M),
     2QHSTO(0:L,0:M),HS(0:L,0:M),HT(0:L,0:M),QV(0:L,0:M),QRHO(0:L,0:M),
     3QW(0:L,0:M),IEN(0:L,0:M),FW(0:L,0:M),MLFIX
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/TAU/CD(0:L,0:M),SINBET(0:L,0:M),COSBET(0:L,0:M),
     1BETA(0:L,0:M),TAUX(L,M),TAUY(L,M)
      COMMON/PMLPARM/SICE,QHW,QHS,DCVM,ENTMAX,WUP,CW,COSGAM,BETAS,BETAT,
     1EPSAA,RTC,STC,QTOC
      COMMON/SNOFLG/SNOFLG
C=======================================================================
C-----------------------------------------------------------------------
C  SPECIFY ZONAL AND ANNUAL MEAN OF CLOUDINESS (ACC.TO V.LOON (1972))
C-----------------------------------------------------------------------
      DATA ACL/2*.48,.57,.62,.69,.78,.8,2*.81,.8,.79,.77,3*.74/
C**20% REDUCTION OF CLOUDINESS:
C     DATA ACL/2*.38,.46,.50,.55,.62,.64,2*.65,.64,.63,.62,3*.59/
C**20% INCREASE OF CLOUDINESS:
C     DATA ACL/2*.58,.68,.74,.83,.94,.96,2*.97,.96,.95,.92,3*.89/
C-----------------------------------------------------------------------
C  SET TIME-STEPPING PARAMETERS
C-----------------------------------------------------------------------
      NTMES=30
      NRST=3600
      NSTAT=1
      NSTA=15
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
      F0=1.46E-04
      DO 1 J=0,M
       COSPHI(J)=COS((-81.25+2.5*J)*RAD)
       SINPHI(J)=SIN((-81.25+2.5*J)*RAD)
       FM(J)=F0*SINPHI(J)
    1 CONTINUE
      DO 2 J=1,M
    2 F(J)=F0*SIN((-82.5+2.5*J)*RAD)
C-----------------------------------------------------------------------
C  DETERMINE GRID SIZE AND TIME STEP
C-----------------------------------------------------------------------
C**REMARK: -DX=5.5560E+05 FOR SPHERICAL COORDINATES
C          -DX=2.3481E+05 FOR CARTESIAN COORDINATES
      DX=5.5560E+05
C     DX=2.3481E+05
      DY=2.7780E+05
      XL=DX*FLOAT(LM)
      DXSQ=DX*DX
      YL=DY*FLOAT(MM)
      DYSQ=DY*DY
      SX2=0.5/DXSQ
      SY2=0.5/DYSQ
      SXY=0.25/(DX*DY)
      DT=0.864E+05
      T=0.0
C-----------------------------------------------------------------------
C  SET COORDINATE MAPPING FACTORS
C-----------------------------------------------------------------------
C**REMARK: -CARTESIAN COORDINATES: PM=1 AND DX=DX AT MEAN LATITUDE
C          -SPHERICAL COORDINATES: PM=1./COSPHI(J) AND DX=DX AT LAT=0
      DO 209 J=0,M
      DO 209 I=0,L
       PM(I,J)=1./COSPHI(J)
       PN(I,J)=1.
  209 CONTINUE
      DO 210 J=1,M
      DO 210 I=1,L
       DNDX(I,J)=0.
       DMDY(I,J)=0.
  210 CONTINUE
C-----------------------------------------------------------------------
C  SET UP ICE AND SNOW PARAMETERS
C-----------------------------------------------------------------------
      RHOICE=0.91E+03
      RHOSNO=0.33E+03
      CON=2.1656
      CONSN=0.31
      H0=0.5
      ARMIN=0.15
      ARMAX=1.0
      HMIN=0.05
      PSTAR=5.0E+03
      ECCEN=2.0
      ECM2=1.0/ECCEN**2
      CSTAR=20.
      ZMAX=5.0E12/2.0E04
      ZMIN=4.0E+08
      GMIN=1.0E-20
      HNU=0.004*DX
      HNU2=DX**2*HNU
C-----------------------------------------------------------------------
C  SET UP SURFACE HEAT AND MOMENTUM TRANSFER PARAMETERS
C-----------------------------------------------------------------------
      TMELT=273.16
      TFREZ=-1.96
      CC=4.2E6
      CLO=3.02E8
      CLB=2.70E8
      RHOAIR=1.3E+00
      RHOWAT=1.0E+03
      CDWIN=1.2E-03
      CDWAT=5.5E-03
C**FOR SURFACE WINDS TURNING ANGLE EQUAL TO ZERO:
C     SINWIN=-0.4226E+00
C     COSWIN=0.9063E+00
      SINWIN=0.
      COSWIN=1.
      SINWAT=-0.4226E+00
      COSWAT=0.9063E+00
      VAPL=2.5E6
      SUBL=2.834E6
      FAKTH=RHOAIR*.4*1004.
      D3=5.5E-08
      ALBI=0.75
      ALBM=0.66
      ALBW=0.10
      ALBSN=0.85
      ALBSNM=0.75
      ZOI=1.E-3
C-----------------------------------------------------------------------
C  SET OML PARAMETERS
C-----------------------------------------------------------------------
      FLAGM=FLOAT(MLFIX)
      QHW=7.0
      QHS=50.0
      QHSMAX=50.0
      DCVM=EXP(-QHSMAX/QHS)
      ENTMAX=30.0
      WUP=3.0E-7
      IF(MLFIX.EQ.1)WUP=0.0
      SICE=5.0
      CW=0.005
      COSGAM=.9135
      BETAS=0.0008
      BETAT=0.00004
      EPSAA=2.0E-6
      RTC=10.0*86400.0/DT
      STC=300.0*86400.0/DT
      QTOC=2.00
C-----------------------------------------------------------------------
C  SET UP PARAMETERS FOR THE RELAX SUBROUTINE
C-----------------------------------------------------------------------
      MMAX=800
      VRMAX=5.0E-06
      WT=1.3
C-----------------------------------------------------------------------
C  INITIALIZE HORIZONTAL BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      CALL BCSINIT
C-----------------------------------------------------------------------
C  INITIALIZE THE SI, OML AND ABL VARIABLES (OPT. FROM RESTART TAPE)
C-----------------------------------------------------------------------
C     IF(NRREC.NE.0)THEN
C      DO 420 K=1,NRREC
C       READ(9,901,ERR=100)((U(I,J,1),V(I,J,1),I=1,L),J=1,M)
C       READ(9,901,ERR=100)((H(I,J,1),A(I,J,1),I=0,L),J=0,M)
C       READ(9,901,ERR=100)((HSN(I,J,1),TICE(I,J),I=0,L),J=0,M)
C       READ(9,901,ERR=100)((QT(I,J),QS(I,J),QH(I,J),I=0,L),J=0,M)
C       READ(9,901,ERR=100)((QTB(I,J),QSB(I,J),QHB(I,J),I=0,L),J=0,M)
C       READ(9,901,ERR=100)((QDT(I,J),QDS(I,J),I=0,L),J=0,M)
C       READ(9,901,ERR=100)(((TICM(I,J,N),I=0,L),J=0,M),N=1,7)
C 420  CONTINUE
C     ELSE
      DO 280 J=1,M
      DO 280 I=1,L
       U(I,J,1)=0.
       V(I,J,1)=0.
  280 CONTINUE
      DO 281 J=0,M
      DO 281 I=0,L
       ZOW(I,J)=1.E-4
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
       TAUX(I,J)=0.
       TAUY(I,J)=0.
  281 CONTINUE
      DO 283 J=0,7
      DO 283 I=0,L
       H(I,J,1)=2.*(1.-J*.14)*OM(I,J)
       A(I,J,1)=1.*(1.-J*.14)*OM(I,J)
       HSN(I,J,1)=.5*(1.-J*.14)*OM(I,J)
  283 CONTINUE
      DO 282 K=1,7
      DO 282 J=0,M
      DO 282 I=0,L
       TICM(I,J,K)=TICE(I,J)
  282 CONTINUE
C-----------------------------------------------------------------------
C  READ IN FIELDS THAT ARE CONSTANT IN TIME
C-----------------------------------------------------------------------
      READ(12,802,ERR=100)((QTB(I,J),I=2,LM),J=1,M-2)
      READ(12,802,ERR=100)((QSB(I,J),I=2,LM),J=1,M-2)
      READ(12,802,ERR=100)((UWAT(I,J),VWAT(I,J),I=2,LM),J=1,MM)
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
      WRITE(16,403)L,M,MLFIX,ABLFIX,ECMTYP,SURFWIN,DX,DY,DT,H0,PSTAR,WT,
     3ZOI,SINWIN,COSWIN,QTOC,CDWIN,CDWAT
      RETURN
  100 WRITE(*,400)
  403 FORMAT(
     1 ' L      = ',I12,    ' M      = ',I12,    ' MLFIX  = ',I12,/,
     2 ' ABLFIX = ',1PE12.3,' ECMTYP = ',1PE12.3,' SURFWIN= ',1PE12.3,/,
     3 ' DX     = ',1PE12.3,' DY     = ',1PE12.3,' DT     = ',1PE12.3,/,
     4 ' H0     = ',1PE12.3,' PSTAR  = ',1PE12.3,' WT     = ',1PE12.3,/,
     5 ' ZOI    = ',1PE12.3,' SINWIN = ',1PE12.3,' COSWIN = ',1PE12.3,/,
     6 ' QTOC   = ',1PE12.3,' CDWIN  = ',1PE12.3,' CDWAT  = ',1PE12.3)
  802 FORMAT(18F6.2)
C 901 FORMAT(5E13.4)
  400 FORMAT('1  READ ERROR IN INIT ')
      STOP
      END
C=======================================================================
      SUBROUTINE BCSINIT
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  PURPOSE:
C     -READS DOMAIN MASKS
C     -IDENTIFIES OUTFLOW CELLS
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LDO=4*L)
C=======================================================================
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/OUTFLOW/NOUT,IOUT(LDO),JOUT(LDO)
C=======================================================================
C-----------------------------------------------------------------------
C  INITIALIZE THE MASKS
C-----------------------------------------------------------------------
      READ(10,801)((VM(I,J),I=1,L),J=1,M)
      READ(10,802)((HM(I,J),I=0,L),J=0,M)
      READ(10,802)((OM(I,J),I=0,L),J=0,M)
C-----------------------------------------------------------------------
C  DETEMINE NUMBER OF OUTFLOW CELLS
C-----------------------------------------------------------------------
      NOUT=0
      DO 10 J=0,M
      DO 10 I=0,L
       IF((OM(I,J).EQ. 0.0).AND.(HM(I,J).NE.0.0))THEN
        NOUT=NOUT+1
        IOUT(NOUT)=I
        JOUT(NOUT)=J
        IF(NOUT.GT.LDO)THEN
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
  801 FORMAT(74G1.0)
  802 FORMAT(75G1.0)
  811 FORMAT('NUMBER OF OUTFLOW GRID POINTS EXCEEDS THE DIMENSION',I3)
      END
C=======================================================================
      SUBROUTINE OUTSM(H,LNEW,HOSUM,HOSM)
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
      PARAMETER(L=74,   M=14,   LDO=4*L)
C=======================================================================
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/OUTFLOW/NOUT,IOUT(LDO),JOUT(LDO)
      DIMENSION H(0:L,0:M,2),TMP(0:L,0:M)
C=======================================================================
C     -TMP: TEMPORARY REGISTER
C=======================================================================
      HOSUM=0.0
      HOSM=0.0
      DO 80 N=1,NOUT
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
C=======================================================================
      SUBROUTINE BCSH(H,LNEW)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LM2=L-2)
C=======================================================================
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      DIMENSION H(0:L,0:M,2)
C=======================================================================
C-----------------------------------------------------------------------
C  SET VARIABLES OUTSIDE DEFINED DOMAIN TO 0
C-----------------------------------------------------------------------
      DO 10 J=0,M
      DO 10 I=0,L
       H(I,J,LNEW)=OM(I,J)*H(I,J,LNEW)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S
C-----------------------------------------------------------------------
      DO 30 J=2,MM
       H(0,J,LNEW)=H(LM2,J,LNEW)
       H(1,J,LNEW)=H(LM ,J,LNEW)
       H(L,J,LNEW)=H(2  ,J,LNEW)
   30 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE BCSV(U,V,LNEW)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON /MASK/ VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      DIMENSION U(L,M,3),V(L,M,3)
C=======================================================================
C-----------------------------------------------------------------------
C  SET VARIABLES OUTSIDE DEFINED DOMAIN TO 0
C-----------------------------------------------------------------------
      DO 10 J=1,M
      DO 10 I=1,L
       U(I,J,LNEW)=VM(I,J)*U(I,J,LNEW)
       V(I,J,LNEW)=VM(I,J)*V(I,J,LNEW)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S
C-----------------------------------------------------------------------
      DO 30 J=2,MM
       U(1,J,LNEW)=U(LM,J,LNEW)
       V(1,J,LNEW)=V(LM,J,LNEW)
       U(L,J,LNEW)=U(2 ,J,LNEW)
       V(L,J,LNEW)=V(2 ,J,LNEW)
   30 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE FORFLD
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C  PURPOSE:
C     -READS TEMPORALLY VARYING BOUNDARY CONDITIONS (FORCING FIELDS)
C  EXTERNALS:
C     -BCSQ: SETS FORCING DATA AT CYCLIC BOUNDARIES
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   MM2=M-2)
C=======================================================================
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      COMMON/THFOR/TAIR(0:L,0:M),TD(0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE WHEN TO REWIND THE TAPES
C-----------------------------------------------------------------------
C  MODIFICATION FOR REAL-TIME DAILY FORCING VALUES:
      INUMB=MOD((IIC-1),365)
      IF(INUMB.LT.1)THEN
       REWIND(8)
      END IF
      IF(INUMB.LT.1.AND.IIC.LT.NFLD)THEN
       REWIND(11)
       REWIND(13)
       REWIND(14)
       REWIND(19)
      END IF
C-----------------------------------------------------------------------
C  READ IN WINDS [M/S]
C-----------------------------------------------------------------------
      READ(11,802,ERR=100)((UWIN(I,J),I=2,LM),(VWIN(I,J),I=2,LM),J=1,MM)
      DO 30 J=1,M
       UWIN(1,J)=UWIN(LM,J)
       VWIN(1,J)=VWIN(LM,J)
       UWIN(L,J)=UWIN(2 ,J)
       VWIN(L,J)=VWIN(2 ,J)
   30 CONTINUE
      DO 31 I=1,L
       UWIN(I,M)=UWIN(I,MM)
       VWIN(I,M)=VWIN(I,MM)
   31 CONTINUE
C-----------------------------------------------------------------------
C  READ IN ATMOSPHERIC TEMPERATURES [C]
C-----------------------------------------------------------------------
      READ(13,802,ERR=100)((TAIR(I,J),I=2,LM),J=1,MM2)
      CALL BCSQ(TAIR)
      DO 12 I=0,L
       TAIR(I,MM)=TAIR(I,MM2)
       TAIR(I,0)=TAIR(I,1)
       TAIR(I,M)=TAIR(I,MM)
   12 CONTINUE
C-----------------------------------------------------------------------
C  READ IN RELATIVE HUMIDITY [%]
C-----------------------------------------------------------------------
      READ(19,801,ERR=100)((TD(I,J),I=2,LM),J=1,MM2)
      CALL BCSQ(TD)
      DO 41 I=0,L
       TD(I,MM)=TD(I,MM2)
       TD(I,0)=TD(I,1)
       TD(I,M)=TD(I,MM)
   41 CONTINUE
C-----------------------------------------------------------------------
C  READ IN SURFACE PRESSURE [HPA]
C-----------------------------------------------------------------------
C**REMARK: FOR PATIT FORMAT 803:
      READ(14,801,ERR=100)((PA(I,J),I=2,LM),J=1,MM2)
      CALL BCSQ(PA)
      DO 13 I=0,L
       PA(I,MM)=PA(I,MM2)
       PA(I,0)=PA(I,1)
       PA(I,M)=PA(I,MM)
   13 CONTINUE
C-----------------------------------------------------------------------
C  CHANGE HPA TO PA
C-----------------------------------------------------------------------
      DO 14 J=0,M
      DO 14 I=0,L
   14 PA(I,J)=PA(I,J)*100.
C-----------------------------------------------------------------------
C  READ IN PRECIPITATION RATE IN [MM/MONTH]
C-----------------------------------------------------------------------
      READ(8,802,ERR=100)((RPREC(I,J),I=2,LM),J=1,MM2)
      CALL BCSQ(RPREC)
      DO 42 I=0,L
       RPREC(I,MM)=RPREC(I,MM2)
       RPREC(I,0)=RPREC(I,1)
       RPREC(I,M)=RPREC(I,MM)
   42 CONTINUE
C-----------------------------------------------------------------------
C  TRANSFER PRECIPITAION RATE INTO [M/SEC]
C-----------------------------------------------------------------------
      DO 43 J=0,M
      DO 43 I=0,L
  43  RPREC(I,J)=(RPREC(I,J)*1.E-03)/(30.*8.64E+04)
C-----------------------------------------------------------------------
C  CALCULATION OF WIND SPEED
C-----------------------------------------------------------------------
      DO 6 J=1,M
      DO 6 I=1,L
       UG(I,J)=.25*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     1             +SQRT(UWIN(I+1,J  )**2+VWIN(I+1,J  )**2)
     2             +SQRT(UWIN(I  ,J+1)**2+VWIN(I  ,J+1)**2)
     3             +SQRT(UWIN(I+1,J+1)**2+VWIN(I+1,J+1)**2))
    6 CONTINUE
      RETURN
  100 WRITE(*,400)
  801 FORMAT(18F6.1)
  802 FORMAT(18F6.2)
  803 FORMAT(18F7.1)
  400 FORMAT('READ ERROR IN FORFLD')
      STOP
      END
C=======================================================================
      SUBROUTINE BCSQ(Q)
C=======================================================================
C  PURPOSE:
C     -SETS CYCLIC BOUNDARY COND. FOR VALUES AT GRID EDGE
C  METHOD:
C     -OVERLAP OF VARIABLES AT THE SEAM
C  INTERFACE:
C     -Q:    VARIABLE TO BE TREATED
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LM2=L-2)
C=======================================================================
      DIMENSION Q(0:L,0:M)
C=======================================================================
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S
C-----------------------------------------------------------------------
      DO 30 J=0,M
       Q(0,J)=Q(LM2,J)
       Q(1,J)=Q(LM,J)
       Q(L,J)=Q(2,J)
   30 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE PLAST(LRHS)
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
C     -USES ICE STRENGTH DETERMINED IN SUBROUTINE PRESS
C     -DETERMINES STRAIN RATE INVARIANTS (EQ.(9) IN HIBLER (1979))
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C  EXTERNALS:
C     -STRAIN: CALCULATES STRAIN RATE TENSOR
C     -OUTBCS: SETS VISCOSITIES AND ICE STRENGTH TO 0 AT OUTFLOW POINTS
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/VISCP/PSTAR,CSTAR,ECCEN,ZMAX,ZMIN,GMIN,ECM2
      COMMON/PRESS/P(L,M)
      COMMON/WORK/TRK(L,M),E11(L,M),E22(L,M),E12(L,M),WRK(L,M,6),
     1ZETA(L,M),ETA(L,M)
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
      DIMENSION TMP(L,M),FLG(L,M)
C=======================================================================
C     -TMP: TEMPORARY REGISTER
C     -FLG: FLAGS FOR CHANGING APPLICATIONS
C=======================================================================
C-----------------------------------------------------------------------
C  CALCULATE STRAIN RATE TENSOR
C-----------------------------------------------------------------------
      CALL STRAIN(LRHS)
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
C=======================================================================
      ENTRY PRESS(LOLD)
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF ICE STRENGTH (EQ.17 IN HIBLER (79))
C  INTERFACE:
C     -LOLD: RUNNING INDEX FOR OLD TIME STEP
C=======================================================================
      DO 200 J=1,MM
      DO 200 I=1,L
  200 P(I,J)=PSTAR*H(I,J,LOLD)*EXP(-CSTAR*(1.0-A(I,J,LOLD)))
      RETURN
      END
C=======================================================================
      SUBROUTINE STRAIN(LRHS)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/VEL/U(L,M,3),V(L,M,3)
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      COMMON/WORK/TRK(L,M),E11(L,M),E22(L,M),E12(L,M)
C=======================================================================
C     -TRK:  DUMMY REGISTER
C     -E11:  X-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E22:  Y-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E12:  DEVIATORIC (SHEAR) STRAIN RATE
C=======================================================================
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
C=======================================================================
      SUBROUTINE OUTBCS
C=======================================================================
      PARAMETER(L=74,   M=14,   LDO=4*L)
C=======================================================================
C  PURPOSE:
C     -SETS FORCE DUE TO INTERNAL ICE STRESS TO 0 AT OUTFLOW POINTS
C  METHOD:
C     -VISCOSITIES AND ICE STRENGTH ARE SET TO 0 AT OUTFLOW POINTS
C=======================================================================
      COMMON/PRESS/P(L,M)
      COMMON/OUTFLOW/NOUT,IOUT(LDO),JOUT(LDO)
      COMMON/WORK/WRK(L,M,10),ZETA(L,M),ETA(L,M)
C=======================================================================
C     -WRK:  DUMMY REGISTERS
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
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
C=======================================================================
      SUBROUTINE INITREL
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/VEL/U(L,M,3),V(L,M,3)
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/WORK/TRK(L,M,4),AMAS(L,M),BU(L,M),BV(L,M),FX(L,M),FY(L,M),
     1ASY(L,M),ZETA(L,M),ETA(L,M)
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
       BU(I,J)=1./(1./BU(I,J)-AMAS(I,J)/DT)
       BV(I,J)=1./(1./BV(I,J)-AMAS(I,J)/DT)
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
C=======================================================================
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
      COMMON/IPARM/RHOICE,H0,HNU,HNU2,ARMIN,ARMAX,HMIN,RHOSNO,CC
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/VEL/U(L,M,3),V(L,M,3)
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      COMMON/FRWAT/RHOWAT,CDWAT,SINWAT,COSWAT,UWAT(L,M),VWAT(L,M)
      COMMON/PRESS/P(L,M)
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      COMMON/TAU/CD(0:L,0:M),SINBET(0:L,0:M),COSBET(0:L,0:M),
     1BETA(0:L,0:M),TAUX(L,M),TAUY(L,M)
      COMMON/WORK/TMP(L,M),WRK(L,M,3),AMAS(L,M),BU(L,M),BV(L,M),FX(L,M),
     1FY(L,M),ASY(L,M),ZETA(L,M),ETA(L,M)
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
   10 AMAS(I,J)=RHOICE*0.25*(H(I-1,J,LOLD)  +H(I,J,LOLD)
     1                      +H(I-1,J-1,LOLD)+H(I,J-1,LOLD))
C-----------------------------------------------------------------------
C  DET.SYM.PARTS OF INT.ICE STRESS AND ADV.CONTR.DUE TO COORD.TRANSFORM.
C-----------------------------------------------------------------------
      CALL BCOEF(LRHS)
C-----------------------------------------------------------------------
C  START LOOP OVER ALL POINTS FOR THE OTHER TERMS
C-----------------------------------------------------------------------
      DO 600 J=2,MM
      DO 600 I=1,L
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
C  ADD COORDINATE TRANSFORMATION TERM (TANGENS TERMS)
C-----------------------------------------------------------------------
       TMP(I,J)=0.25*((V(I,J,LRHS)  +V(I,J-1,LRHS)
     1                +V(I-1,J,LRHS)+V(I-1,J-1,LRHS))*DNDX(I,J)
     2               -(U(I,J,LRHS)  +U(I-1,J,LRHS)
     3                +U(I-1,J,LRHS)+U(I-1,J-1,LRHS))*DMDY(I,J))
C-----------------------------------------------------------------------
C  ADD COEFFICIENT FOR CORIOLIS FORCE
C-----------------------------------------------------------------------
       ASY(I,J)=ASY(I,J)+AMAS(I,J)*(F(J)+TMP(I,J))
C-----------------------------------------------------------------------
C  ADD CONTRIBUTION FROM THE SEA SURFACE TILT
C-----------------------------------------------------------------------
       FX(I,J)=FX(I,J)-VWAT(I,J)*AMAS(I,J)*F(J)
       FY(I,J)=FY(I,J)+UWAT(I,J)*AMAS(I,J)*F(J)
C-----------------------------------------------------------------------
C  ADD THE AIR/ICE STRESS
C-----------------------------------------------------------------------
       CDWIN=.25*(CD(I,J)+CD(I-1,J)+CD(I,J-1)+CD(I-1,J-1))*(1.-ABLFIX)
     1           +CDWIN*ABLFIX
       SINWIN=.25*(SINBET(I,J)+SINBET(I-1,J)+SINBET(I,J-1)
     1            +SINBET(I-1,J-1))*(1.-SURFWIN)         +SINWIN*SURFWIN
       COSWIN=.25*(COSBET(I,J)+COSBET(I-1,J)+COSBET(I,J-1)
     1            +COSBET(I-1,J-1))*(1.-SURFWIN)         +COSWIN*SURFWIN
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
       BU(I,J)=1./BU(I,J)
       BV(I,J)=1./BV(I,J)
  600 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE BCOEF(LRHS)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/VEL/U(L,M,3),V(L,M,3)
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      COMMON/WORK/TMP(L,M),WRK(L,M,3),AMAS(L,M),BU(L,M),BV(L,M),FX(L,M),
     1FY(L,M),ASY(L,M),ZETA(L,M),ETA(L,M)
C=======================================================================
C     -TMP:  TEMPORARY VARIABLE
C     -WRK:  DUMMY REGISTERS
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -FX:   X-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -FY:   Y-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -ASY:  ASYMMETRIC COEFFICIENTS OF THE MOMENTUM EQUATION
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
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
C=======================================================================
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/RELAXP/MMAX,VRMAX,WT
      COMMON/VEL/U(L,M,3),V(L,M,3)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/WORK/TMP(L,M),RU(L,M),RV(L,M),DEN(L,M),AMAS(L,M),BU(L,M),
     1BV(L,M),FX(L,M),FY(L,M),ASY(L,M),ZETA(L,M),ETA(L,M)
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
      IF(MRELAX.GT.MMAX)GOTO 400
C-----------------------------------------------------------------------
C  DETERMINE THE RELAXATION FACTOR FOR THE RESIDUAL
C-----------------------------------------------------------------------
      IF((MRELAX.GT.50).OR.(MRELAX.EQ.0))THEN
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
       CALL DDX(U,V,ZETA,LN,K)
       DO 125 J=2,MM
        IB=MOD(J+K,2)+2
        DO 125 I=IB,LM,2
  125   RU(I,J)=TMP(I,J)+TMP(I+1,J)
C  ENTER D{ZETA*[D(U)/DX+D(V)/DY]}/DY TO V EQUATION:
       CALL DDY(V,U,ZETA,LN,K)
       DO 130 J=2,MM
        IB=MOD(J+K,2)+2
        DO 130 I=IB,LM,2
  130   RV(I,J)=TMP(I,J)+TMP(I+1,J)
C-----------------------------------------------------------------------
C  NEXT DIFFERENTIATE THE SHEAR VISCOSITIES
C-----------------------------------------------------------------------
C  ADD D{ETA*[D(U)/DX-D(V)/DY]}/DX TO U EQUATION:
       CALL DDX(U,V,ETA,LN,K)
       DO 135 J=2,MM
        IB=MOD(J+K,2)+2
        DO 135 I=IB,LM,2
  135   RU(I,J)=RU(I,J)+TMP(I,J)-TMP(I+1,J)
C  ADD D{ETA*[D(V)/DY-D(U)/DX]}/DY TO V EQUATION:
       CALL DDY(V,U,ETA,LN,K)
       DO 140 J=2,MM
        IB=MOD(J+K,2)+2
        DO 140 I=IB,LM,2
  140   RV(I,J)=RV(I,J)+TMP(I,J)-TMP(I+1,J)
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DY TO U EQUATION:
       CALL DDY(U,V,ETA,LN,K)
       DO 145 J=2,MM
        IB=MOD(J+K,2)+2
        DO 145 I=IB,LM,2
  145   RU(I,J)=RU(I,J)+TMP(I,J)+TMP(I+1,J)
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DX TO V EQUATION:
       CALL DDX(V,U,ETA,LN,K)
       DO 150 J=2,MM
        IB=MOD(J+K,2)+2
        DO 150 I=IB,LM,2
  150   RV(I,J)=RV(I,J)+TMP(I,J)+TMP(I+1,J)
C-----------------------------------------------------------------------
C  ADD IN THE ADVECTION TERMS
C-----------------------------------------------------------------------
       CALL MADV(LR,LN,K)
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
       CALL BCSV(U,V,LN)
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
      IF(VERR.GT.VRMAX)GOTO 120
C**WE HAVE SUCCEEDED IN FINDING A NEW SOLUTION:
      RETURN
C**WE HAVE NOT FOUND A SOLUTION AFTER MMAX ITERATIONS:
  400 CONTINUE
      WRITE(*,2000)IIC,MRELAX
      STOP
 2000 FORMAT('AT TIME STEP',I6,'AFTER',I3,'ITERATIONS, NO SOLUTION
     1        OBTAINED')
      END
C=======================================================================
      SUBROUTINE DDX(U,V,ETA,LN,K)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      COMMON/WORK/TMP(L,M)
C=======================================================================
C     -TMP: TEMPORARY ARRAY
C=======================================================================
      DIMENSION U(L,M,3),V(L,M,3),ETA(L,M)
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
C=======================================================================
      SUBROUTINE MADV(LR,LN,K)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      COMMON/VEL/U(L,M,3),V(L,M,3)
      COMMON/WORK/TMP(L,M),RU(L,M),RV(L,M),DEN(L,M),AMAS(L,M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -RU:   X-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -RV:   V-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -DEN:  DENOMINATOR OF THE SOLUTION EQUATION
C     -AMAS: ICE MASS
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
C=======================================================================
      SUBROUTINE SADVECT(RH,H,U,V,LRHS,LADV)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
      DIMENSION RH(0:L,0:M),H(0:L,0:M,2),U(L,M,2),V(L,M,2)
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
C=======================================================================
      SUBROUTINE SDIFFUS(RH,H,LRHS)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/IPARM/RHOICE,H0,HNU,HNU2,ARMIN,ARMAX,HMIN,RHOSNO,CC
      COMMON/DRV/DX,DXSQ,DY,DYSQ,SX2,SY2,SXY
      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M)
      DIMENSION TMP(0:L,0:M),DFX(0:L,0:M),DFY(0:L,0:M),RH(0:L,0:M),
     1H(0:L,0:M,2)
C=======================================================================
C     -TMP: TEMPORARY ARRAY
C     -DFX: FLUX IN X-DIRECTION
C     -DFY: FLUX IN Y-DIRECTION
C     -RH:  CONTRIBUTION TO CONTINUITY EQUATION
C     -H:   SCALAR VARIABLE
C=======================================================================
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
C=======================================================================
      SUBROUTINE BCSFLX(FX,FY)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1)
C=======================================================================
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      DIMENSION FX(0:L,0:M),FY(0:L,0:M)
C=======================================================================
C-----------------------------------------------------------------------
C  SET DIFFUSIVE FLUXES ACROSS THE BOUNDARIES TO 0
C-----------------------------------------------------------------------
      DO 10 J=0,M
      DO 10 I=0,L
       FX(I,J)=FLM(I,J,1)*FX(I,J)
       FY(I,J)=FLM(I,J,2)*FY(I,J)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      DO 30 J=2,MM
       FX(1,J)=FX(LM,J)
       FY(1,J)=FY(LM,J)
       FX(L,J)=FX(2,J)
       FY(L,J)=FY(2,J)
   30 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE GROWTH(LRHS,LNEW)
C=======================================================================
C  PROGRAMMED BY:
C     W.D.HIBLER III           CRREL, HANOVER, USA                  1979
C     W.B.OWENS                MPI, HAMBURG                         1987
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
C  OPTIONS:
C     -INCLUSION OF SEVEN-LEVEL ICE THICKNESS DISTRIBUTION
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD TIME STEP
C     -LNEW: RUNNING INDEX FOR NEW TIME STEP
C  EXTERNALS:
C     -SHDEF:   CREATES OPEN WATER DUE TO SHEAR DEFORMATION
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMIN:  THE THIRD ARGUMENT IS MINIMUM OF THE FIRST TWO ARGUMENTS
C     -SHWARA:  CALCULATES SHORT WAVE RADIATION
C     -OBUDGET: CALCULATES OPEN WATER HEAT BUDGET
C     -ECMBUDO: CALCULATES OPEN WATER HEAT BUDGET WITH ASL-MODEL
C     -EKMAO:   CALCULATES OPEN WATER HEAT BUDGET WITH ABL-MODEL
C     -BUDGET:  CALCULATES HEAT BUDGET OVER ICE
C     -ECMBUDI: CALCULATES HEAT BUDGET OVER ICE WITH ASL-MODEL
C     -EKMAH:   CALCULATES HEAT BUDGET OVER ICE WITH ABL-MODEL
C     -PMLEX:   CALCULATES OML-VARIABLES (OML-MODEL)
C     -VECMINC: SAME AS VECMIN, THE SECOND ARGUMENT BEING AN ARRAY
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LP=L+1,   MP=M+1)
C=======================================================================
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/IPARM/RHOICE,H0,HNU,HNU2,ARMIN,ARMAX,HMIN,RHOSNO,CC
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/FRWAT/RHOWAT,CDWAT,SINWAT,COSWAT,UWAT(L,M),VWAT(L,M)
      COMMON/THFOR/TAIR(0:L,0:M),TD(0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/TEMP/TICE(0:L,0:M)
      COMMON/TEMPM/TICM(0:L,0:M,7)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/PML/QS(0:L,0:M),QT(0:L,0:M),QH(0:L,0:M),QSB(0:L,0:M),
     +QTB(0:L,0:M),QHB(0:L,0:M),QDS(0:L,0:M),QDT(0:L,0:M),
     +QHSTO(0:L,0:M),HS(0:L,0:M),HT(0:L,0:M),QV(0:L,0:M),
     +QRHO(0:L,0:M),QW(0:L,0:M),IEN(0:L,0:M),FW(0:L,0:M),MLFIX
      COMMON/SNOFLG/SNOFLG
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/WORK/TMP(0:L,0:M),RH(0:L,0:M),RA(0:L,0:M),TMP2(0:L,0:M),
     1TMP3(0:L,0:M),TMP4(0:L,0:M),QHST(0:L,0:M),QFM(0:L,0:M),
     2PREC(0:L,0:M),SN(0:L,0:M),QTM(0:L,0:M),SH(0:L,0:M)
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
C=======================================================================
C-----------------------------------------------------------------------
C  CREATE OPEN WATER DUE TO SHEAR DEFORMATION
C-----------------------------------------------------------------------
      CALL SHDEF(LNEW,TMP3)
      CALL VECMAX(A(0,0,LRHS),0.85,TMP)
      DO 301 J=1,MM
      DO 301 I=0,L
       TMP2(I,J)=(1.0-TMP(I,J))*(0.5*(1.0+TMP(I,J))-0.85)
     1           /(0.15*(0.5*1.85-0.85))
       A(I,J,LNEW)=A(I,J,LNEW)-TMP3(I,J)*(1.0-TMP2(I,J))*DT
  301 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE COS OF ZENITH DISTANCE AND SOLAR CONSTANT
C-----------------------------------------------------------------------
      CALL SHWARA(IIC)
C-----------------------------------------------------------------------
C  CALCULATE THE GROWTH RATE FOR THIN ICE (OPEN OCEAN)
C-----------------------------------------------------------------------
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP:
      IF(ABLFIX.EQ.1.)THEN
       CALL OBUDGET(RA,QT)
      ELSE
       IF(ECMTYP.EQ.1.)THEN
        CALL ECMBUDO(RA,QT)
       ELSE
        CALL EKMAO(RA,QT)
       END IF
      END IF
C-----------------------------------------------------------------------
C  CALCULATE EFFECTIVE ICE THICKNESS
C-----------------------------------------------------------------------
C  MAKE SURE WE HAVE NON-ZERO COMPACTNESS:
      CALL VECMAX(A(0,0,LRHS),ARMIN,TMP)
      DO 5 J=1,MM
      DO 5 I=0,L
C  INCLUDE SNOW THICKNESS FOR CONDUCTION EFFECT THROUGH SNOW:
       TMP(I,J)=(H(I,J,LRHS)+HSN(I,J,LRHS)*CON/CONSN)/TMP(I,J)
C**FOR 7 LAYER THERMODYNAMICS, INSERT THE FOLLOWING STATEMENT:
       RH(I,J)=0.0
    5 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES FOR THICK ICE
C-----------------------------------------------------------------------
C  MAKE SURE WE HAVE NON-ZERO THICKNESS FOR CONDUCTION TERM IN BUDGET:
      CALL VECMAX(TMP,HMIN,TMP)
C**FOR 7 LEVEL MODEL OF ICE, INSERT THE FOLLOWING 2 STATEMENTS:
      CALL VECMAX(TMP,HMIN,SN)
      DO 8 K=1,7
       DO 6 J=1,MM
       DO 6 I=0,L
C  SET ALBEDO ACC.TO PRESENCE OF SNOW(TMP4) AND MELTING COND.(TMP3):
        TMP4(I,J)=0.5*(1.-SIGN(1.,-HSN(I,J,LRHS)))
        TMP3(I,J)=0.5*(1.+SIGN(1.,TICE(I,J)))
C**FOR 7 LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP3(I,J)=0.5*(1.+SIGN(1.,TICM(I,J,K)))
        TMP2(I,J)=TMP4(I,J)*(TMP3(I,J)*ALBSNM+(1.-TMP3(I,J))*ALBSN)
     1           +(1.-TMP4(I,J))*(TMP3(I,J)*ALBM+(1.-TMP3(I,J))*ALBI)
C**FOR 7 LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP(I,J)=(2*K-1)*SN(I,J)/7.
    6  CONTINUE
C      CALL EKMAH (RH,TICE)
C**FOR 7 LEVEL MODEL,INS. THE FOLL.6 STATEM.AND COMM.OUT PREV.STATEM.:
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP:
       IF(ABLFIX.EQ.1.)THEN
        CALL BUDGET(SH,TICM(0,0,K),LRHS,K)
       ELSE
        IF(ECMTYP.EQ.1.)THEN
         CALL ECMBUDI(SH,TICM(0,0,K),LRHS,K)
        ELSE
         CALL EKMAH(SH,TICM(0,0,K),LRHS,K)
        END IF
       END IF
C  GET MEAN GROWTH RATE OF THICK ICE:
       DO 7 J=1,MM
       DO 7 I=0,L
        RH(I,J)=RH(I,J)+SH(I,J)/7.
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
       TMP(I,J)=SH(I,J)
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
       RH(I,J)=SH(I,J)-TMP2(I,J)*RHOSNO/RHOICE
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
   17 SN(I,J)=SN(I,J)+TMP3(I,J)*RHOICE/RHOSNO
C-----------------------------------------------------------------------
C  UPDATE FRESH WATER INPUT, HEAT STORAGE, HEAT FLUX AND FREEZING RATE
C-----------------------------------------------------------------------
      CALL VECMIN(SN,0.,TMP2)
      CALL VECMAX(SN,0.,SN)
      CALL VECMAX(H(0,0,LNEW),0.,QFM)
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
      CALL PMLEX(QHST,SN,QFM,PREC,QTM)
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
      CALL VECMINC(RH,H(0,0,LNEW),RH)
      DO 125 J=1,MM
      DO 125 I=0,L
  125 RH(I,J)=-RH(I,J)
C-----------------------------------------------------------------------
C  UPDATE THERMODYNAMIC CHANGE IN ICE COMPACTNESS (EQ.16 IN HIBLER 79)
C-----------------------------------------------------------------------
C  MAKE SURE WE DO NOT DIVIDE BY 0 IF H=0:
      CALL VECMAX(H(0,0,LRHS),HMIN,TMP3)
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
      CALL VECMINC(A(0,0,LNEW),TMP,A(0,0,LNEW))
      CALL VECMIN(A(0,0,LNEW),ARMAX,A(0,0,LNEW))
      CALL VECMAX(A(0,0,LNEW),0.,A(0,0,LNEW))
      RETURN
      END
C=======================================================================
      SUBROUTINE SHDEF(LRHS,OPEW)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LP=L+1,   MP=M+1)
C=======================================================================
      COMMON/VISCP/PSTAR,CSTAR,ECCEN,ZMAX,ZMIN,GMIN,ECM2
      COMMON/WORK/TMP(L,M),E11(L,M),E22(L,M),E12(L,M)
      DIMENSION OPEW(0:L,0:M)
C=======================================================================
      CALL STRAIN(LRHS)
      DO 10 J=1,MM
      DO 10 I=1,LM
       DELT=(E11(I,J)**2+E22(I,J)**2)*(1.0+ECM2)+4.0*ECM2*E12(I,J)**2
     1     +2.0*E11(I,J)*E22(I,J)*(1.0-ECM2)
       DELT1=SQRT(DELT)
       OPEW(I,J)=0.5*(DELT1-E11(I,J)-E22(I,J))
   10 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SHWARA(IIC)
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL            MPI, HAMBURG                           1987
C  MODIFIED BY:
C     -A.STOESSEL            MPI, HAMBURG                           1989
C  PURPOSE:
C     -CALCULATION OF COSINE OF ZENITH DISTANCE AND SOLAR CONSTANT
C  METHOD:
C     -THE DAILY MEAN SOLAR RADIATION IS DETERMINED BY AVERAGING THE
C       HOURLY CALCULATED COSINE OF ZENITH DISTANCE IN ORDER TO OPTIMIZE
C       THE ABL-ROUTINES
C  INTERFACE:
C     -IIC: CURRENT TIME STEP=DAY OF INTEGRATION
C=======================================================================
      PARAMETER(L=74,   M=14)
C=======================================================================
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE DAY OF THE YEAR
C-----------------------------------------------------------------------
C**MODIFICATION FOR ACTUAL DAILY FORCING VALUES:
      DJ=MOD(IIC,365)*360./365.
C     DJ=MOD(IIC,360)
C-----------------------------------------------------------------------
C  DECLINATION OF THE SUN
C-----------------------------------------------------------------------
      DECL=23.44*COS((170.6-DJ)*RAD)
      SINDECL=SIN(DECL*RAD)
      COSDECL=COS(DECL*RAD)
C-----------------------------------------------------------------------
C  DETERMINE SOLAR CONSTANT
C-----------------------------------------------------------------------
      SOL=1353.*(1.000110+0.034221*COS(DJ*RAD)+0.001280*SIN(DJ*RAD)
     1+0.000718*COS(2.*DJ*RAD)+0.000077*SIN(2.*DJ*RAD))
C-----------------------------------------------------------------------
C  START CALCULATING COSINE OF ZENITH DISTANCE AS FUNCTION OF LATITUDE
C-----------------------------------------------------------------------
      DO 2 J=0,M
       TZ=0.
       SCOSZ=0.
C  SUM UP HOURLY VALUES:
       DO 1 IT=1,24
        TZ=TZ+1.
C  HOUR ANGLE:
        HA=(12.2-TZ)*PI/12.
C  COSINE OF ZENITH DISTANCE:
        COSZ(J)=SINPHI(J)*SINDECL+COSPHI(J)*COSDECL*COS(HA)
        COSZ(J)=MAX(COSZ(J),0.)
        SCOSZ=SCOSZ+COSZ(J)
    1  CONTINUE
C  DAILY MEAN COSINE OF ZENITH DISTANCE:
       COSZ(J)=SCOSZ/24.
   2  CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE OBUDGET(FH,QT)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/THFOR/TAIR(0:L,0:M),TD (0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/FLUX/FLSE(0:L,0:M),FLLA(0:L,0:M),WMUE1(0:L,0:M)
     1,UST1(0:L,0:M),TMPL1(0:L,0:M)
      COMMON/WORK/WRK(0:L,0:M,6),FLAW(LMDP),FAKTS(LMDP)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C=======================================================================
      DIMENSION FH(0:L,0:M),QT(0:L,0:M),ESTA(LMDP),ESTW(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -QT:   SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW: SATURATION VAPOR PRESSURE OVER WATER
C=======================================================================
      DIMENSION TA1(LMDP),TD1(LMDP),ACL1(LMDP),PA1(LMDP),UG1(LMDP),
     1COSZ1(LMDP),FLSE1(LMDP),FLLA1(LMDP),FH1(LMDP),QT1(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
C-----------------------------------------------------------------------
C  SELECT THE GRID CELLS AND STORE THEM IN SELECTIVE 1-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 93 J=1,MM
      DO 93 I=0,L
       IF(OM(I,J).EQ.0.)GOTO93
       K=K+1
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
   93 CONTINUE
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 112 N=1,K
       FLAW(N)=(1.-ALBW)*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FAKTS(N)=1.+0.3*ACL1(N)**2
  112 CONTINUE
      D1=RHOAIR*1004.*1.75E-3
      D2W=RHOAIR*VAPL*1.75E-3
C-----------------------------------------------------------------------
C  CALCULATE HEAT FLUXES AND GROWTH RATES
C-----------------------------------------------------------------------
      DO 25 N=1,K
       EA=TD1(N)*ESTA(N)
       FEU=0.605+5.95*1.E-7*EA*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
       FLSE1(N)=D1*UG1(N)*(TA1(N)-QT1(N))
       FLLA1(N)=D2W*UG1(N)*(EA-ESTW(N))*.623/PA1(N)
       Q1=D3*QT1(N)**4
       Q2=FLAW(N)/FLAU
       Q3=FAKTS(N)*FEU*D3*TA1(N)**4
       FH1(N)=(Q1-Q2-Q3-FLSE1(N)-FLLA1(N))/CLB
   25 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 81 J=1,MM
      DO 81 I=0,L
       IF(OM(I,J).EQ.0.)GOTO81
       K=K+1
       FH(I,J)=FH1(K)
       TA(I,J)=TA1(K)
       FLSE(I,J)=FLSE1(K)
       FLLA(I,J)=FLLA1(K)
   81 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE BUDGET(FH,T,LRHS,KG)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/STP/TX,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/THFOR/TAIR(0:L,0:M),TD(0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/FLUX/FLSE(0:L,0:M),FLLA(0:L,0:M),WMUE1(0:L,0:M)
     1,UST1(0:L,0:M),TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M),WRK(0:L,0:M,2),ALB(0:L,0:M),TRK(0:L,0:M)
     1,A2(0:L,0:M),FLAI(LMDP),FAKTS(LMDP)
C=======================================================================
C     -HICE:  EFFECTIVE ICE THICKNESS (OPTIONALLY FOR SEVEN CATEGORIES)
C     -WRK:   DUMMY ARRAYS
C     -ALB:   ALBEDO
C     -TRK:   DUMMY ARRAY
C     -A2:    FLAG FOR SNOW CONDITIONS
C     -FLAI:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C=======================================================================
      DIMENSION FH(0:L,0:M),T(0:L,0:M),ESTA(LMDP),ESTI(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI: SATURATION VAPOR PRESSURE OVER ICE
C=======================================================================
      DIMENSION TA1(LMDP),TD1(LMDP),ACL1(LMDP),PA1(LMDP),UG1(LMDP),
     1COSZ1(LMDP),FLSE1(LMDP),FLLA1(LMDP),FH1(LMDP),T1(LMDP),ALB1(LMDP),
     2A21(LMDP),HICE1(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      DIMENSION STP(LMDP),STPP(LMDP),FP(LMDP),FPP(LMDP),DIFF(LMDP),
     1TT(LMDP),TT1(0:L,0:M),TMYUS1(0:L,0:M)
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
       IF(A(I,J,LRHS).EQ.0.)TMYUS1(I,J)=2.
   79 CONTINUE
      TB=TFREZ+TMELT
C-----------------------------------------------------------------------
C  STORE EXTERNAL VARIABLES INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 82 J=1,MM
      DO 82 I=0,L
       IF(TMYUS1(I,J).EQ.2.)GOTO82
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
   82 CONTINUE
      IF(K.EQ.0)GOTO87
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(T1,ESTI,2,K)
      DO 31 N=1,K
       FLAI(N)=(1.-ALB1(N))*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FAKTS(N)=1.+0.3*ACL1(N)**2
   31 CONTINUE
      D1=RHOAIR*1004.*1.75E-3
      D2I=RHOAIR*SUBL*1.75E-3
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 33 N=1,K
       STP(N)=T1(N)
       EA=TD1(N)*ESTA(N)
       FEU=0.601+5.95*1.E-7*EA*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
       FP(N)=D3*STP(N)**4-FLAI(N)/FLAU-FAKTS(N)*FEU*D3*TA1(N)**4
     1       -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2       .623/PA1(N)+(STP(N)-TB)/HICE1(N)*CON
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
        FEU=0.601+5.95*1.E-7*EA*EXP(1500./TA1(N))
        FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
        FP(N)=D3*STP(N)**4-FLAI(N)/FLAU-FAKTS(N)*FEU*D3*TA1(N)**4
     1        -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2        .623/PA1(N)+(STP(N)-TB)/HICE1(N)*CON
        FDIFF=FP(N)-FPP(N)
        T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1        MAX(ABS(FDIFF),1.E-10)*SIGN(1.,FDIFF)
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
       FEU=0.601+5.95*1.E-7*EA*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       FLAI(N)=(1.-ALB1(N))*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FLSE1(N)=D1*UG1(N)*(TA1(N)-T1(N))
       FLLA1(N)=D2I*UG1(N)*(EA-ESTI(N))*0.623/PA1(N)
       Q1=D3*T1(N)**4
       Q2=FLAI(N)/FLAU
       Q3=FAKTS(N)*FEU*D3*TA1(N)**4
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
       IF(OM(I,J).EQ.0.)GOTO84
       IF(A(I,J,LRHS).EQ.0.)GOTO84
       K=K+1
       FH(I,J)=FH1(K)
       T(I,J)=T1(K)-TMELT
       TT1(I,J)=TT(K)
       IF(TT1(I,J).LE.0.)WRITE(16,701)IIC
       IF(KG.NE.4)GOTO84
       TA(I,J)=TA1(K)
       FLSE(I,J)=FLSE1(K)
       FLLA(I,J)=FLLA1(K)
   84 CONTINUE
   87 RETURN
  701 FORMAT(1X,I4,'ITERATION EXCEEDED')
      END
C=======================================================================
      SUBROUTINE VAPOR(T,EST,K1,K)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      DIMENSION T(LMDP),EST(LMDP)
C=======================================================================
      GOTO(1,2,3),K1
    1 DO 11 N=1,K
   11  EST(N)=611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),200.)-273.15+257.87))
      RETURN
    2 DO 22 N=1,K
   22  EST(N)=611.15*EXP((23.036-(MIN(T(N),273.15)-273.15)/333.7)*
     1        (MIN(T(N),273.15)-273.15)/(MAX(T(N),200.)-273.15+279.82))
      RETURN
    3 DO 33 N=1,K
   33  EST(N)=0.9815*611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),260.)-273.15+257.87))
      RETURN
      END
C=======================================================================
      SUBROUTINE ECMBUDO(FH,QT)
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
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -RWSTAB: CALC.THE STAB.FUNCTIONS WITH VARIABLE ROUGHNESS LENGTH
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/TAU/CD(0:L,0:M),SINBET(0:L,0:M),COSBET(0:L,0:M),
     1BETA(0:L,0:M),TAUX(L,M),TAUY(L,M)
      COMMON/THFOR/TAIR(0:L,0:M),TD (0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/FLUX/FLSE(0:L,0:M),FLLA(0:L,0:M),WMUE1(0:L,0:M)
     1,UST1(0:L,0:M),TMPL1(0:L,0:M)
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      COMMON/WORK/WRK(0:L,0:M,6),FLAW(LMDP),FAKTS(LMDP),TRK(LMDP),
     1THETG(LMDP)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -TRK:   DUMMY ARRAY
C     -THETG: POTENTIAL TEMPERATURE
C=======================================================================
      DIMENSION FH(0:L,0:M),QT(0:L,0:M),ESTA(LMDP),ESTW(LMDP),EA1(LMDP),
     1ZA(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -QT:   SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW: SATURATION VAPOR PRESSURE OVER WATER
C     -EA1:  RELATIVE HUMIDITY
C     -ZA:   HEIGHT OF FORCING LEVEL
C=======================================================================
      DIMENSION TA1(LMDP),TD1(LMDP),ACL1(LMDP),PA1(LMDP),UG1(LMDP),
     1COSZ1(LMDP),FLSE1(LMDP),FLLA1(LMDP),FH1(LMDP),QT1(LMDP),ZOW1(LMDP)
     2,WUST(LMDP),CD1(LMDP),TMPL(LMDP)
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
       IF(OM(I,J).EQ.0.)GOTO93
       K=K+1
       ZOW1(K)=ZOW(I,J)
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
   93 CONTINUE
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 112 N=1,K
       FLAW(N)=(1.-ALBW)*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FAKTS(N)=1.+0.3*ACL1(N)**2
       EA1(N)=TD1(N)*ESTA(N)
       FLAG=.5*(1.-SIGN(1.,PA1(N)-1.E5))
       THETG(N)=FLAG*TA1(N)+(1.-FLAG)*(TA1(N)+6.5E-3*287.05
     1          *TA1(N)*LOG(1.E5/PA1(N))/9.81)*(PA1(N)/1.E5)**.287
       ZA(N)=MAX(30.,((PA1(N)-100000.)*.08))
       TMPL(N)=9.81*ZA(N)*(THETG(N)-QT1(N)+.61*QT1(N)*(EA1(N)-ESTW(N))
     1         *.623/PA1(N))/QT1(N)/UG1(N)**2
  112 CONTINUE
C-----------------------------------------------------------------------
C  GET THE STABILITY FUNCTIONS
C-----------------------------------------------------------------------
      CALL RWSTAB(TMPL,ZOW1,K,PA1)
C-----------------------------------------------------------------------
C  CALCULATE HEAT FLUXES AND GROWTH RATES
C-----------------------------------------------------------------------
      DO 25 N=1,K
       FEU=0.605+5.95*1.E-7*EA1(N)*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA1(N)*1.E-5+.0455
       FLSE1(N)=UG1(N)*(THETG(N)-QT1(N))*RHOAIR*1004.*(.4/LOG(ZA(N)/
     1          ZOW1(N)))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTW(N))*.623/PA1(N)*RHOAIR*VAPL*
     1          (.4/LOG(ZA(N)/ZOW1(N)))**2*PH1(N)/.74
       Q1=D3*QT1(N)**4
       Q2=FLAW(N)/FLAU
       Q3=FAKTS(N)*FEU*D3*TA1(N)**4
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
       IF(OM(I,J).EQ.0.)GOTO81
       K=K+1
       ZOW(I,J)=MAX(1.5E-5,.032*WUST(K)**2/9.81)
       CD(I,J)=CD1(K)
       FH(I,J)=FH1(K)
   81 CONTINUE
   87 RETURN
      END
C=======================================================================
      SUBROUTINE RWSTAB(RI,ZO,K,PA1)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      DIMENSION RI(LMDP),PA1(LMDP),ZO(LMDP)
      DO 2 N=1,K
       IF(RI(N).GE.0.)GOTO1
       ZA=MAX(30.,((PA1(N)-100000.)*8.))
       FAKT=(.4/LOG(ZA/ZO(N)))**2*9.4*SQRT(ZA/ZO(N))
       CM=7.4*FAKT
       CH=5.3*FAKT
       FM=1.-9.4*RI(N)/(1.+CM*SQRT(ABS(RI(N))))
       FH=1.-9.4*RI(N)/(1.+CH*SQRT(ABS(RI(N))))
       GOTO3
    1  FM=1./(1.+2.*4.7*RI(N))**2
       FH=FM
    3  PH1(N)=FH
       PH2(N)=FM
    2 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE ECMBUDI(FH,T,LRHS,KG)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/TAU/CD(0:L,0:M),SINBET(0:L,0:M),COSBET(0:L,0:M),
     1BETA(0:L,0:M),TAUX(L,M),TAUY(L,M)
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/STP/TX,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/THFOR/TAIR(0:L,0:M),TD(0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M),FLLA(0:L,0:M),WMUE1(0:L,0:M)
     1,UST1(0:L,0:M),TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M),WRK(0:L,0:M,2),ALB(0:L,0:M),TRK(0:L,0:M)
     1,A2(0:L,0:M),FLAI(LMDP),FAKTS(LMDP),QG(LMDP),THETG(LMDP)
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
      DIMENSION FH(0:L,0:M),T(0:L,0:M),ESTA(LMDP),ESTI(LMDP),EA1(LMDP),
     1ZA(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI: SATURATION VAPOR PRESSURE OVER ICE
C     -EA1:  RELATIVE HUMIDITY
C     -ZA:   HEIGHT OF FORCING LEVEL
C=======================================================================
      DIMENSION TA1(LMDP),TD1(LMDP),ACL1(LMDP),PA1(LMDP),UG1(LMDP),
     1COSZ1(LMDP),FLSE1(LMDP),FLLA1(LMDP),FH1(LMDP),T1(LMDP),ALB1(LMDP),
     2A21(LMDP),HICE1(LMDP),UST(LMDP),CD1(LMDP),FM1(LMDP),TMPL(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      DIMENSION STP(LMDP),STPP(LMDP),FP(LMDP),FPP(LMDP),TT(LMDP),
     1TMYUS1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
      DIMENSION SINBET1(LMDP),COSBET1(LMDP),BETA1(LMDP),WMUE(LMDP)
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
       IF(A(I,J,LRHS).EQ.0.)TMYUS1(I,J)=2.
   79 CONTINUE
      TB=TFREZ+TMELT
C-----------------------------------------------------------------------
C  STORE EXTERNAL VARIABLES INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 82 J=1,MM
      DO 82 I=0,L
       IF(TMYUS1(I,J).EQ.2.)GOTO82
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
C**NEXT STATEMENT FOR ADDITIONAL WIND TURNING:
C      FM1(K)=FM(J)
   82 CONTINUE
      IF(K.EQ.0)GOTO87
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(T1,ESTI,2,K)
      DO 31 N=1,K
       FLAI(N)=(1.-ALB1(N))*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FAKTS(N)=1.+0.3*ACL1(N)**2
       EA1(N)=TD1(N)*ESTA(N)
       FLAG=.5*(1.-SIGN(1.,PA1(N)-1.E5))
       THETG(N)=FLAG*TA1(N)+(1.-FLAG)*(TA1(N)+6.5E-3*287.05
     1          *TA1(N)*LOG(1.E5/PA1(N))/9.81)*(PA1(N)/1.E5)**.287
       ZA(N)=MAX(30.,((PA1(N)-100000.)*.08))
       TMPL(N)=9.81*ZA(N)*(THETG(N)- T1(N)+.61* T1(N)*(EA1(N)-ESTI(N))
     1         *.623/PA1(N))/ T1(N)/UG1(N)**2
   31 CONTINUE
      CALL RISTAB(TMPL,ZOI,K,PA1)
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 33 N=1,K
       STP(N)=T1(N)
       FEU=0.601+5.95*1.E-7*EA1(N)*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA1(N)*1.E-5+.0455
       FLSE1(N)=UG1(N)*(THETG(N)-STP(N))*RHOAIR*1004.
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*.623/PA1(N)*RHOAIR*VAPL
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FP(N)=D3*STP(N)**4-FLAI(N)/FLAU-FAKTS(N)*FEU*D3*TA1(N)**4
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
        TMPL(N)=9.81*ZA(N)*(THETG(N)-T1(N)+.61*T1(N)*(EA1(N)-ESTI(N))
     1          *.623/PA1(N))/T1(N)/UG1(N)**2
   32  CONTINUE
       CALL RISTAB(TMPL,ZOI,K,PA1)
       DO 34 N=1,K
        STPP(N)=STP(N)
        FPP(N)=FP(N)
        STP(N)=T1(N)
        FEU=0.601+5.95*1.E-7*EA1(N)*EXP(1500./TA1(N))
        FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA1(N)*1.E-5+.0455
        FLSE1(N)=UG1(N)*(THETG(N)-STP(N))*RHOAIR*1004.
     1           *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
        FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*.623/PA1(N)*RHOAIR*VAPL
     1           *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
        FP(N)=D3*STP(N)**4-FLAI(N)/FLAU-FAKTS(N)*FEU*D3*TA1(N)**4
     1        -FLSE1(N)-FLLA1(N)+(STP(N)-TB)/HICE1(N)*CON
        FDIFF=FP(N)-FPP(N)
        T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)
     1        /MAX(ABS(FDIFF),1.E-10)*SIGN(1.,FDIFF)
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
       TMPL(N)=9.81*ZA(N)*(THETG(N)-T1(N)+.61*T1(N)*(EA1(N)-ESTI(N))
     1         *.623/PA1(N))/T1(N)/UG1(N)**2
   52 CONTINUE
      CALL RISTAB(TMPL,ZOI,K,PA1)
      DO 85 N=1,K
       FEU=0.601+5.95*1.E-7*EA1(N)*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA1(N)*1.E-5+.0455
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       FLAI(N)=(1.-ALB1(N))*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FLSE1(N)=UG1(N)*(THETG(N)-T1(N))*RHOAIR*1004.
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*.623/PA1(N)*RHOAIR*VAPL
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       Q1=D3*T1(N)**4
       Q2=FLAI(N)/FLAU
       Q3=FAKTS(N)*FEU*D3*TA1(N)**4
       UST(N)=(.4/LOG(ZA(N)/ZOI))*UG1(N)*SQRT(PH2(N))
       CD1(N)=(UST(N)/UG1(N))**2
C**NEXT STATEMENT FOR ADDITIONAL WIND TURNING:
C      WMUE(N)=.4*UST(N)/ABS(FM1(N))/ZA(N)/PH1(N)*PH2(N)**2*TMPL(N)
       FHI=A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB-T1(N))/HICE1(N)*CON)/CLO
       FHB=((TB-T1(N))/HICE1(N)*CON)/CLB
       FH1(N)=FHI+FHB
   85 CONTINUE
C**NEXT CALL AND LOOP FOR ADDITIONAL WIND TURNING:
C     CALL RESIST(WMUE,K)
C     DO 86 N=1,K
C      SINBET1(N)=-BF(N)/0.4*UST(N)/UG1(N)
C      FLAG=SIGN(1.,SINBET1(N))
C      SINBET1(N)=MIN(ABS(SINBET1(N)),1.)*FLAG
C      COSBET1(N)=SQRT(1.-SINBET1(N)*SINBET1(N))
C      BETA1(N)=ACOS(COSBET1(N))/RAD
C  86 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 84 J=1,MM
      DO 84 I=0,L
       IF(OM(I,J).EQ.0.)GOTO84
       IF(A(I,J,LRHS).EQ.0.)GOTO84
       K=K+1
       FH(I,J)=FH1(K)
       T(I,J)=T1(K)-TMELT
C      IF(TT(K).LE.0.)WRITE(16,701)IIC
       IF(KG.NE.4)GOTO84
       CD(I,J)=CD1(K)
C**NEXT FOUR STATEMENTS FOR ADDITIONAL WIND TURNING:
C      WMUE1(I,J)=WMUE(K)
C      SINBET(I,J)=SINBET1(K)
C      COSBET(I,J)=COSBET1(K)
C      BETA(I,J)=BETA1(K)
   84 CONTINUE
   87 RETURN
  701 FORMAT(1X,I4,'ITERATION EXCEEDED')
      END
C=======================================================================
      SUBROUTINE RISTAB(RI,ZO,K,PA1)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      DIMENSION RI(LMDP),PA1(LMDP)
C=======================================================================
      DO 2 N=1,K
       IF(RI(N).GE.0.)GOTO1
       ZA=MAX(30.,((PA1(N)-100000.)*8.))
       FAKT=(.4/LOG(ZA/ZO))**2*9.4*SQRT(ZA/ZO)
       CM=7.4*FAKT
       CH=5.3*FAKT
       FM=1.-9.4*RI(N)/(1.+CM*SQRT(ABS(RI(N))))
       FH=1.-9.4*RI(N)/(1.+CH*SQRT(ABS(RI(N))))
       GOTO3
    1  FM=1./(1.+2.*4.7*RI(N))**2
       FH=FM
    3  PH1(N)=FH
       PH2(N)=FM
    2 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE EKMAO(FH,QT)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/TAU/CD(0:L,0:M),SINBET(0:L,0:M),COSBET(0:L,0:M),
     1BETA(0:L,0:M),TAUX (L,M),TAUY (L,M)
      COMMON/THFOR/TAIR(0:L,0:M),TD (0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M),FLLA(0:L,0:M),WMUE1(0:L,0:M)
     1,WUST1(0:L,0:M),TMPL1(0:L,0:M)
      COMMON/WORK/WRK(0:L,0:M,6),FLAW(LMDP),FAKTS(LMDP),QG(LMDP),
     1THETG(LMDP)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -QG:    SPECIFIC HUMIDITY AT 850 HPA
C     -THETG: POTENTIAL TEMPERATURE AT 850 HPA
C=======================================================================
      DIMENSION FH(0:L,0:M),QT(0:L,0:M),ESTA(LMDP),ESTW(LMDP),DELT(LMDP)
     1,SUM1(LMDP),SUM2(LMDP),FAKT1(LMDP),QAL(LMDP)
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
      DIMENSION TA1(LMDP),TD1(LMDP),ACL1(LMDP),PA1(LMDP),UG1(LMDP),
     1COSZ1(LMDP),FLSE1(LMDP),FLLA1(LMDP),FH1(LMDP),QT1(LMDP),ZOW1(LMDP)
     2,WUST(LMDP),CD1(LMDP),WMUE(LMDP),FM1(LMDP),SINBET1(LMDP),
     3COSBET1(LMDP),BETA1(LMDP),TMPL(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE NECESSARY FOR THE OPTIMIZATION OF THE
C    ITERATION PROCEDURES
C=======================================================================
      DIMENSION STP(LMDP),STPP(LMDP),FP(LMDP),FPP(LMDP),UPAST(LMDP),
     1TWMYUS1(0:L,0:M),TWMYUS(LMDP),TWMUE(LMDP),TWUST(LMDP),PAST(LMDP)
     2,TWMUE1(0:L,0:M),TWUST1(0:L,0:M)
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
       IF(TWMYUS1(I,J).EQ.2.)GOTO80
       K=K+1
       ZOW1(K)=ZOW(I,J)
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
       FM1(K)=FM(J)
       IF(ITERW.EQ.0)GOTO80
       WUST(K)=WUST1(I,J)
       WMUE(K)=WMUE1(I,J)
   80 CONTINUE
      IF(K.EQ.0)GOTO91
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATIONS
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 11 N=1,K
       FTHET=(PA1(N)/8.5E4)**0.287
       THETG(N)=FTHET*TA1(N)
       QG(N)=0.622/(85000./(TD1(N)*ESTA(N))-0.378)
       FLAW(N)=(1.-ALBW)*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FAKTS(N)=1.+0.3*ACL1(N)**2
       DELT(N)=THETG(N)-QT1(N)
       IF(ITERW.GT.0)GOTO11
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
        SUM2(N)=0.61*(QG(N)-0.623/PA1(N)*ESTW(N))
        FAKT1(N)=9.8134*0.064/ABS(FM1(N))/WUST(N)
        STP(N)=WMUE(N)
   12  CONTINUE
       CALL STAB(TMPL,ZOW(0,0),K)
       CALL RESIST(STP,K)
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
        CALL STAB(TMPL,ZOW(0,0),K)
        CALL RESIST(STP,K)
        DO 15 N=1,K
         FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
         SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
         FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1         /(QT1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
         FDIFF=FP(N)-FPP(N)
         WMUE(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF),1.E-10)*SIGN(1.,FDIFF)
         DIFF=WMUE(N)-STP(N)
         TWMUE(N)=SIGN(1.,10.-ABS(DIFF))
   15   CONTINUE
    1  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE FRICTION VELOCITY
C-----------------------------------------------------------------------
C**FOR CYCLE 7 SKIP UST-ITERATION:
       IF(SURFWIN.EQ.1)THEN
        DO 66 N=1,K
   66   WUST(N)=.4*UG1(N)/PH2(N)
       ELSE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        CALL RESIST(WMUE,K)
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
     1            MAX(ABS(FDIFF),1.E-10)*SIGN(1.,FDIFF)
          DIFF=WUST(N)-STP(N)
          TWUST(N)=SIGN(1.,.0001-ABS(DIFF))
   23    CONTINUE
    2   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  DETERMINE WHETHER WE SUCCEEDED IN FINDING ANY SOLUTION
C-----------------------------------------------------------------------
       DO 24 N=1,K
        WUST(N)=MAX(WUST(N),0.014)
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
       IF(TWMYUS1(I,J).EQ.2.)GOTO92
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
      IF(ITERW.GT.IWMAX)GOTO91
      GOTO88
C  90 WRITE(16,701)IIC
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED STABILITY AND FRICTION VELOCITY
C-----------------------------------------------------------------------
   91 CONTINUE
      K=0
       DO 93 J=1,MM
       DO 93 I=0,L
       IF(OM(I,J).EQ.0.)GOTO93
       K=K+1
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
       FM1(K)=FM(J)
       TMPL(K)=TMPL1(I,J)
       WMUE(K)=WMUE1(I,J)
       WUST(K)=WUST1(I,J)
   93 CONTINUE
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 112 N=1,K
       FTHET=(PA1(N)/8.5E4)**0.287
       THETG(N)=FTHET*TA1(N)
       QG(N)=0.622/(85000./(TD1(N)*ESTA(N))-0.378)
       FLAW(N)=(1.-ALBW)*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FAKTS(N)=1.+0.3*ACL1(N)**2
  112 CONTINUE
      CALL STAB(TMPL,ZOW(0,0),K)
      CALL RESIST(WMUE,K)
      DO 25 N=1,K
       FAKT=1./(LOG(WUST(N)/ABS(FM1(N))/ZOW1(N))-CF(N))
       FAKT1(N)=FAKTH*WUST(N)*FAKT
       QAL(N)=PH1(N)*FAKT
       TA1(N)=QT1(N)*(1.-QAL(N))+QAL(N)*THETG(N)
       EA=MAX(0.,ESTW(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/.623)
       FEU=0.605+5.95*1.E-7*EA*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
       ALPHE=FAKTH*WUST(N)/(SUM1(N)-CF(N))
       ALPHA=FAKTH*WUST(N)/PH1(N)
       STRE=WUST(N)**2*RHOAIR
       FLSE1(N)=ALPHA*(TA1(N)-QT1(N))
       FLLA1(N)=ALPHA*(EA-ESTW(N))/1004.*VAPL*0.623/PA1(N)
       ZL=5.*ABS(FM1(N))*WMUE(N)/WUST(N)
       Q1=D3*QT1(N)**4
       Q2=FLAW(N)/FLAU
       Q3=FAKTS(N)*FEU*D3*TA1(N)**4
       SINBET1(N)=-BF(N)/0.4*WUST(N)/UG1(N)
       FLAG=SIGN(1.,SINBET1(N))
       SINBET1(N)=MIN(ABS(SINBET1(N)),1.)*FLAG
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
       IF(OM(I,J).EQ.0.)GOTO81
       K=K+1
       FH(I,J)=FH1(K)
       CD(I,J)=CD1(K)
       SINBET(I,J)=SINBET1(K)
       COSBET(I,J)=COSBET1(K)
C**ITERATION-INFO CANCELLED:
C      IF(TWMUE1(I,J).LE.0..OR.TWUST1(I,J).LE.0.)WRITE(16,701)IIC
   81 CONTINUE
   87 RETURN
  701 FORMAT(1X,I4,'ITERATION EXCEEDED')
      END
C=======================================================================
      SUBROUTINE EKMAH(FH,T,LRHS,KG)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/CORR/FM(0:M),F(M),COSPHI(0:M),SINPHI(0:M)
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/THCK/H(0:L,0:M,2),A(0:L,0:M,2),HSN(0:L,0:M,2)
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/STP/TX,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/TAU/CD(0:L,0:M),SINBET(0:L,0:M),COSBET(0:L,0:M),
     1BETA(0:L,0:M),TAUX (L,M),TAUY (L,M)
      COMMON/THFOR/TAIR(0:L,0:M),TD (0:L,0:M),ACL(0:M),PA(0:L,0:M)
     1,UG(0:L,0:M),TA(0:L,0:M),RPREC(0:L,0:M)
      COMMON/FRWND/RHOAIR,CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M),FLLA(0:L,0:M),WMUE1(0:L,0:M)
     1,UST1(0:L,0:M),TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M),WRK(0:L,0:M,2),ALB(0:L,0:M),TRK(0:L,0:M)
     1,A2(0:L,0:M),FLAI(LMDP),FAKTS(LMDP),QG(LMDP),THETG(LMDP)
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
      DIMENSION FH(0:L,0:M),T(0:L,0:M),ESTA(LMDP),ESTI(LMDP),DELT(LMDP),
     1SUM1(LMDP),SUM2(LMDP),FAKT1(LMDP),QAL(LMDP)
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
      DIMENSION TA1(LMDP),TD1(LMDP),ACL1(LMDP),PA1(LMDP),UG1(LMDP),
     1COSZ1(LMDP),FLSE1(LMDP),FLLA1(LMDP),FH1(LMDP),T1(LMDP),ALB1(LMDP),
     2A21(LMDP),HICE1(LMDP),UST(LMDP),CD1(LMDP),WMUE(LMDP),FM1(LMDP),
     3SINBET1(LMDP),COSBET1(LMDP),BETA1(LMDP),TMPL(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE NECESSARY FOR THE OPTIMIZATION OF THE
C    ITERATION PROCEDURES
C=======================================================================
      DIMENSION STP(LMDP),STPP(LMDP),FP(LMDP),FPP(LMDP),UPAST(LMDP),
     1TMYUS1(0:L,0:M),TMYUS(LMDP),TMUE(LMDP),TUST(LMDP),PAST(LMDP)
     2,TT(LMDP),TT1(0:L,0:M),TMUE1(0:L,0:M),TUST1(0:L,0:M)
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
       IF(A(I,J,LRHS).EQ.0.)TMYUS1(I,J)=2.
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
       IF(TMYUS1(I,J).EQ.2.)GOTO82
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
       FM1(K)=FM(J)
       IF(ITERW.EQ.0)GOTO82
       WMUE(K)=WMUE1(I,J)
       UST(K)=UST1(I,J)
   82 CONTINUE
      IF(K.EQ.0)GOTO91
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATIONS
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      DO 31 N=1,K
       FTHET=(PA1(N)/8.5E4)**0.287
       THETG(N)=FTHET*TA1(N)
       QG(N)=0.622/(85000./(TD1(N)*ESTA(N))-0.378)
       FLAI(N)=(1.-ALB1(N))*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       FAKTS(N)=1.+0.3*ACL1(N)**2
       DELT(N)=THETG(N)-T1(N)
       IF(ITERW.GT.0)GOTO31
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
       CALL RESIST(WMUE,K)
       CALL STAB(TMPL,ZOI,K)
       CALL VAPOR(T1,ESTI,2,K)
       CALL VAPOR(T1,ESTA,1,K)
       DO 33 N=1,K
        FAKT=1./(LOG(UST(N)/ABS(FM1(N))/ZOI)-CF(N))
        FAKT1(N)=FAKTH*UST(N)*FAKT
        QAL(N)=PH1(N)*FAKT
        STP(N)=T1(N)
        EA=MAX(0.,ESTA(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/.623)
        TA1(N)=MAX(200.,STP(N)*(1.-QAL(N))+QAL(N)*THETG(N))
        FEU=0.601+5.95*1.E-7*EA*EXP(1500./TA1(N))
        FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
        FP(N)=D3*STP(N)**4-FLAI(N)/FLAU-FAKTS(N)*FEU*D3*TA1(N)**4
     1        -FAKT1(N)*(THETG(N)-STP(N)+SUBL/1004.*(QG(N)-0.623
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
         FEU=0.601+5.95*1.E-7*EA*EXP(1500./TA1(N))
         FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
         FP(N)=D3*STP(N)**4-FLAI(N)/FLAU-FAKTS(N)*FEU*D3*TA1(N)**4
     1         -FAKT1(N)*(THETG(N)-STP(N)+SUBL/1004.*(QG(N)-0.623
     2         /PA1(N)*ESTI(N)))+(STP(N)-TB)/HICE1(N)*CON
         FDIFF=FP(N)-FPP(N)
         T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1         MAX(ABS(FDIFF),1.E-10)*SIGN(1.,FDIFF)
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
        FAKT1(N)=9.8134*0.064/ABS(FM1(N))/UST(N)
        STP(N)=WMUE(N)
        TMPL(N)=5.*ABS(FM1(N))*STP(N)/UST(N)
   42  CONTINUE
       CALL STAB(TMPL,ZOI,K)
       CALL RESIST(STP,K)
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
        CALL STAB(TMPL,ZOI,K)
        CALL RESIST(STP,K)
        DO 45 N=1,K
         FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
         SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
         FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1         /(T1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
         FDIFF=FP(N)-FPP(N)
         WMUE(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF),1.E-10)*SIGN(1.,FDIFF)
         DIFF=WMUE(N)-STP(N)
         TMUE(N)=SIGN(1.,10.-ABS(DIFF))
   45   CONTINUE
    4  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE FRICTION VELOCITY
C-----------------------------------------------------------------------
C**FOR CYCLE 7 SKIP UST-ITERATION:
       IF(SURFWIN.EQ.1)THEN
        DO 22 N=1,K
   22   UST(N)=.4*UG1(N)/PH2(N)
       ELSE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        CALL RESIST(WMUE,K)
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
     1           MAX(ABS(FDIFF),1.E-10)*SIGN(1.,FDIFF)
          DIFF= UST(N)-STP(N)
          TUST(N)=SIGN(1.,.0001-ABS(DIFF))
   53    CONTINUE
    5   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  DETERMINE WHETHER WE SUCCEEDED IN FINDING ANY SOLUTION
C-----------------------------------------------------------------------
       DO 54 N=1,K
        UST(N)=MAX(UST(N),0.014)
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
       IF(TMYUS1(I,J).EQ.2.)GOTO92
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
      IF(ITERW.GT.IWMAX)GOTO91
      GOTO88
C  90 WRITE(16,700)IIC
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED T, WMUE AND UST
C-----------------------------------------------------------------------
   91 CONTINUE
      K=0
      DO 93 J=1,MM
      DO 93 I=0,L
       IF(OM(I,J).EQ.0.)GOTO93
       IF(A(I,J,LRHS).EQ.0.)GOTO93
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       ACL1(K)=ACL(J)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J),2.)
       COSZ1(K)=COSZ(J)
       FM1(K)=FM(J)
       TMPL(K)=TMPL1(I,J)
       WMUE(K)=WMUE1(I,J)
       UST(K)=UST1(I,J)
   93 CONTINUE
      CALL VAPOR(TA1,ESTA,1,K)
      DO 112 N=1,K
       FTHET=(PA1(N)/8.5E4)**0.287
       THETG(N)=FTHET*TA1(N)
       QG(N)=0.622/(85000./(TD1(N)*ESTA(N))-0.378)
       FAKTS(N)=1.+0.3*ACL1(N)**2
  112 CONTINUE
      CALL STAB(TMPL,ZOI,K)
      CALL RESIST(WMUE,K)
      DO 55 N=1,K
       FAKT=1./(LOG(UST(N)/ABS(FM1(N))/ZOI)-CF(N))
       FAKT1(N)=FAKTH*UST(N)*FAKT
       QAL(N)=PH1(N)*FAKT
       TA1(N)=T1(N)*(1.-QAL(N))+QAL(N)*THETG(N)
       EA=MAX(0.,ESTI(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/.623)
       FEU=0.605+5.95*1.E-7*EA*EXP(1500./TA1(N))
       FLAU=1.2*COSZ1(N)+(1.+COSZ1(N))*EA*1.E-5+.0455
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       FLAI(N)=(1.-ALB1(N))*(1.-.6*ACL1(N)**3)*SOL*COSZ1(N)**2
       ALPHA=FAKTH*UST(N)/PH1(N)
       STRE=UST(N)**2*RHOAIR
       FLSE1(N)=ALPHA*(TA1(N)-T1(N))
       FLLA1(N)=ALPHA*(EA-ESTI(N))/1004.*VAPL*0.623/PA1(N)
       ZL=5.*ABS(FM1(N))*WMUE(N)/UST(N)
       Q1=D3*T1(N)**4
       Q2=FLAI(N)/FLAU
       Q3=FAKTS(N)*FEU*D3*TA1(N)**4
       SINBET1(N)=-BF(N)/0.4*UST(N)/UG1(N)
       FLAG=SIGN(1.,SINBET1(N))
       SINBET1(N)=MIN(ABS(SINBET1(N)),1.)*FLAG
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
       IF(OM(I,J).EQ.0.)GOTO84
       IF(A(I,J,LRHS).EQ.0.)GOTO84
       K=K+1
       FH(I,J)=FH1(K)
       IF(KG.NE.4)GOTO84
C**ITERATION-INFO CANCELLED:
C     IF(TT1(I,J).LE.0..OR.TMUE1(I,J).LE.0..OR.TUST1(I,J).LE.0.)
C    1   WRITE(16,701)IIC
      CD(I,J)=CD1(K)
      SINBET(I,J)=SINBET1(K)
      COSBET(I,J)=COSBET1(K)
   84 CONTINUE
   87 RETURN
  701 FORMAT(1X,I4,'ITERATION EXCEEDED')
      END
C=======================================================================
      SUBROUTINE STAB(OL,ZO,K)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      DIMENSION OL(LMDP)
C=======================================================================
      R=SQRT(2.)
      CRI=-1./16.
C-----------------------------------------------------------------------
C  FIRST DETERMINE THE STABILITY FUNCTION FOR THE HEAT FLUXES
C-----------------------------------------------------------------------
      DO 2 N=1,K
       ZOL=OL(N)*ZO/2.
       IF(ZOL.GT.-(1.E-6))GOTO 1
       CS=(1.-16.*OL(N))**0.5
       DS=(ABS(1.+16.*ZOL))**0.5
       IF(ZOL.GT.CRI+1.E-6)GOTO10
       IF(ZOL.LT.CRI-1.E-6)GOTO11
       PH1(N)=2.*(1.+1./CS)
       GOTO2
C  FOR UNSTABLE STRATIFICATION:
   10  PH1(N)=LOG((CS-DS)*(1.+DS)/(CS+DS)/(1.-DS))/DS
       GOTO 2
   11  PH1(N)=2./DS*(ATAN(CS/DS)-ATAN(1./DS))
       GOTO 2
C  FOR NEUTRAL AND STABLE STRATIFICATION:
    1  ZLOG=LOG((2.+ZO)/ZO)
       PH1(N)=ZLOG+5.*(OL(N)+ZOL/ZLOG)
    2 CONTINUE
C-----------------------------------------------------------------------
C  NEXT DETERMINE THE STABILITY FUNCTION FOR THE MOMENTUM FLUX
C-----------------------------------------------------------------------
      DO 3 N=1,K
       OL(N)=5.*OL(N)
       ZOL=OL(N)*ZO/10.
       IF(ZOL.GT.-(1.E-6))GOTO4
       AS=(1.-16.*OL(N))**0.25
       BS=(ABS(1.+16.*ZOL))**0.25
       IF(ZOL.GT.CRI+1.E-6)GOTO20
       IF(ZOL.LT.CRI-1.E-6)GOTO21
       PH2(N)=4.*(1.-1./AS)
       GOTO3
C  FOR UNSTABLE STRATIFICATION:
   20  PH2(N)=2./BS*(-0.5*LOG((BS+AS)*(BS-1.)/(BS-AS)/(1.+BS))
     1        +ATAN(AS/BS)-ATAN(1./BS))
       GOTO 3
   21  PH2(N)=(1.-FLAG)*(LOG(SQRT(AS**4+BS**4)*(1.+R*BS+BS*BS)
     1        /SQRT(1.+BS**4)/(AS*AS+R*AS*BS+BS*BS))
     2        +ATAN(AS*BS*R/(BS*BS-AS*AS))-ATAN(BS*R/(BS*BS-1.)))/BS/R
       GOTO 3
C  FOR NEUTRAL AND STABLE STRATIFICATION:
    4  ZLOG=LOG((10.+ZO)/ZO)
       PH2(N)=ZLOG+5.*(OL(N)+ZOL/ZLOG)
    3 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE RESIST(X,K)
C=======================================================================
C  PURPOSE:
C     -DETERMINATION OF STABILITY FUNCTIONS A,B,AND C (AF,BF,CF)
C  METHOD:
C     -ROSSBY NUMBER SIMILARITY THEORY (KOCH, 1986: 93)
C  INTERFACE:
C     -X: STABILITY PARAMETER (WMUE)
C     -K: MAXIMUM NUMBER OF GRID POINTS TO BE TREATED
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LMDP=(L+1)*(M+1))
C=======================================================================
      COMMON/RES/AF(LMDP),BF(LMDP),CF(LMDP),PH1(LMDP),PH2(LMDP)
      DIMENSION X(LMDP)
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
C=======================================================================
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LP=L+1,   MP=M+1)
C=======================================================================
      COMMON/PML/QS(0:L,0:M),QT(0:L,0:M),QH(0:L,0:M),QSB(0:L,0:M),
     1QTB(0:L,0:M),QHB(0:L,0:M),QDS(0:L,0:M),QDT(0:L,0:M),
     2QHSTO(0:L,0:M),HS(0:L,0:M),HT(0:L,0:M),QV(0:L,0:M),
     3QRHO(0:L,0:M),QW(0:L,0:M),IEN(0:L,0:M),FW(0:L,0:M),MLFIX
      COMMON/STP/T,DT,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/IPARM/RHOICE,H0,HNU,HNU2,ARMIN,ARMAX,HMIN,RHOSNO,CC
      COMMON/FRWAT/RHOWAT,CDWAT,SINWAT,COSWAT,UWAT(L,M),VWAT(L,M)
      COMMON/PMLPARM/SICE,QHW,QHS,DCVM,ENTMAX,WUP,CW,COSGAM,BETAS,BETAT,
     1EPSAA,RTC,STC,QTOC
      COMMON/GEO/PI,RAD,SOL,COSZ(0:M)
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
C=======================================================================
      COMMON/WORK/TMP(0:L,0:M),WRK(0:L,0:M,2),TMP2(0:L,0:M),
     1TMP3(0:L,0:M),TMP4(0:L,0:M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -WRK:  DUMMY ARRAYS
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C     -TMP4: TEMPORARY ARRAY
C=======================================================================
      DIMENSION DCV(0:L,0:M),QWT(0:L,0:M),RET(0:L,0:M),ENT(0:L,0:M),
     1HOLD(0:L,0:M),QSS(0:L,0:M),FLAGI(0:L,0:M)
C=======================================================================
C     -DCV:   DISSIPATION OF CONVECTIVE ENERGY
C     -QWT:   KINETIC ENERGY * DISSIPATION OF MECHANICAL ENERGY
C     -RET:   AMOUNT OF MIXED LAYER RETREAT
C     -ENT:   ENTRAINMENT VELOCITY
C     -HOLD:  OLD MIXED LAYER DEPTH
C     -QSS:   ADJUSTED MIXED LAYER SALINITY DUE TO ADVECTIVE EFFECTS
C     -FLAGI: FLAG FIELD FOR PRESENCE OF ICE AND/OR SNOW
C=======================================================================
      DIMENSION QHST(0:L,0:M),SNOW(0:L,0:M),QFM(0:L,0:M),QPR(0:L,0:M),
     1QTM(0:L,0:M)
C=======================================================================
C-----------------------------------------------------------------------
C  ARTIFICIAL RESTRICTIONS FOR OML SIMULATION
C-----------------------------------------------------------------------
C  TURBULENT LENGTH SCALE:
      DELTAD=8.0
C  PYCNOCLINE LIMITATIONS FOR NEAR NEUTRAL STRATIFICATIONS:
      SSMIN=0.01
      TTMIN=0.8
C  MODIFICATION OF TIME CONSTANT FOR NEWTONIAN ADJUSTMENT OF SALINITY:
      HSTC=120.0
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
      IF(MLFIX.NE.1)GOTO30
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
   30 CALL VECMAX(TMP,DCVM,DCV)
      DO 40 J=1,MM
      DO 40 I=0,L
       FLAG=(1.+SIGN(1.,QRHO(I,J)))/2.
       DCV(I,J)=1.-FLAG +DCV(I,J)*FLAG
       ENT(I,J)=2.*QWT(I,J)+9.81*QRHO(I,J)*DCV(I,J)*QH(I,J)
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
       F0=2.*QW(I,J)*A1+9.81*QRHO(I,J)*TMP(I,J)
       F1=-2.*QW(I,J)*A1/QHW+9.81*QRHO(I,J)
       TMP(I,J)=TMP(I,J)-F0*FLAG/(F1*FLAG+1.-FLAG)
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
       DETA=9.81*QH(I,J)*(BETAS*SSS-BETAT*TTT)
       TMP(I,J)=DETA-9.81*QH(I,J)*TTT*DCV(I,J)
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
       TMP3(I,J)=500.0
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
  200 CALL ADJUEX(QHST,SNOW,FLAGI)
C-----------------------------------------------------------------------
C  STABILITY ADJUSTMENT (CONVECTION)
C-----------------------------------------------------------------------
      IF(MLFIX.EQ.1) GO TO 300
      DO 270 J=1,MM
      DO 270 I=0,L
       STAB=BETAS*(QSB(I,J)-QS(I,J))-BETAT*(QTB(I,J)-QT(I,J))
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
       FLAGS=(1.0-SIGN(1.,STAB-EPSAA))/2.
       TMP(I,J)=QH(I,J)
       TMP3(I,J)=510.0
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
  300 CALL VECMAX(QHSTO,0.,TMP)
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
      CALL VECMIN(QDS,500.0,QDS)
      CALL VECMIN(QDT,500.0,QDT)
      CALL VECMIN(QH,500.,QH)
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
C=======================================================================
      SUBROUTINE ADJUEX(QHST,SNOW,FLAGI)
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
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LP=L+1,   MP=M+1)
C=======================================================================
      COMMON/IPARM/RHOICE,H0,HNU,HNU2,ARMIN,ARMAX,HMIN,RHOSNO,CC
      COMMON/THPAR/SUBL,VAPL,D3,CON,ALBI,ALBM,ALBW,ALBSN,ALBSNM,TMELT,
     1TFREZ,CONSN
      COMMON/PML/QS(0:L,0:M),QT(0:L,0:M),QH(0:L,0:M),QSB(0:L,0:M),
     1QTB(0:L,0:M),QHB(0:L,0:M),QDS(0:L,0:M),QDT(0:L,0:M),
     2QHSTO(0:L,0:M),HS(0:L,0:M),HT(0:L,0:M),QV(0:L,0:M),
     3QRHO(0:L,0:M),QW(0:L,0:M),IEN(0:L,0:M),FW(0:L,0:M),MLFIX
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
      COMMON/FRWAT/RHOWAT,CDWAT,SINWAT,COSWAT,UWAT(L,M),VWAT(L,M)
      COMMON/PMLPARM/SICE,QHW,QHS,DCVM,ENTMAX,WUP,CW,COSGAM,BETAS,BETAT,
     1EPSAA,RTC,STC,QTOC
      COMMON/ABLM/ZOW(0:L,0:M),ZOI,CLB,CLO,FAKTH,ABLFIX,SURFWIN,ECMTYP
      COMMON/WORK/TMP(0:L,0:M),WRK(0:L,0:M,2),TMP2(0:L,0:M),
     1TMP3(0:L,0:M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -WRK:  DUMMY ARRAYS
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C=======================================================================
      DIMENSION QHST(0:L,0:M),SNOW(0:L,0:M),FLAGI(0:L,0:M),TMP4(0:L,0:M)
     1,TMP5(0:L,0:M)
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
       QT(I,J)=QT(I,J)-(QT(I,J)-TFREZ+TMP3(I,J)*CL/CC/QH(I,J))
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
      IF(MLFIX.EQ.1)GOTO300
      CALL VERDIF
  300 RETURN
      END
C=======================================================================
      SUBROUTINE VERDIF
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C  PURPOSE:
C     -SIMULATION OF VERTICAL DIFFUSION
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C=======================================================================
      PARAMETER(L=74,   M=14,   LM=L-1,   MM=M-1,   LP=L+1,   MP=M+1)
C=======================================================================
      COMMON/PML/QS(0:L,0:M),QT(0:L,0:M),QH(0:L,0:M),QSB(0:L,0:M),
     +QTB(0:L,0:M),QHB(0:L,0:M),QDS(0:L,0:M),QDT(0:L,0:M),
     +QHSTO(0:L,0:M),HS(0:L,0:M),HT(0:L,0:M),QV(0:L,0:M),
     +QRHO(0:L,0:M),QW(0:L,0:M),IEN(0:L,0:M),FW(0:L,0:M),MLFIX
      COMMON/MASK/VM(L,M),HM(0:L,0:M),OM(0:L,0:M),FLM(0:L,0:M,2)
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
C=======================================================================
      SUBROUTINE DRUCKF(FELD,FLG,FAK,ADD,HEADER,ICOUNT,NX,NY)
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
      DIMENSION FELD(NX,NY),FLG(NX,NY)
      CHARACTER*3 TEXT(44)
      CHARACTER HEADER*(*)
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
        WRITE(TEXT(INDEX),200)INT(XP)
        IF(FLG(I,JJ).EQ.0.0)THEN
         TEXT(INDEX)='   '
        ELSE IF(XP.LT.0..AND.XP.GT.-1.)THEN
         TEXT(INDEX)=' -0'
        END IF
    2  CONTINUE
       WRITE(16,300)(TEXT(I),I=1,INDEX)
    1 CONTINUE
      RETURN
  100 FORMAT(1H ,5X,' DAY ',I5,'  ARRAY : ',A50)
  101 FORMAT(1H )
  200 FORMAT(I3)
  300 FORMAT(1X,44A3)
      END
C=======================================================================
      SUBROUTINE VECMAX(F1,VALUE,F2)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING A CONSTANT
C=======================================================================
      PARAMETER(L=74,   M=14)
C=======================================================================
      DIMENSION F1(0:L,0:M),F2(0:L,0:M)
C=======================================================================
      DO 1 J=0,M
      DO 1 I=0,L
       FLAG=SIGN(1.,F1(I,J)-VALUE)
       F2(I,J)=0.5*(F1(I,J)*(1.+FLAG)+VALUE*(1.-FLAG))
    1 CONTINUE
      RETURN
C=======================================================================
      ENTRY VECMIN(F1,VALUE,F2)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MINIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING A CONSTANT
C=======================================================================
      DO 2 J=0,M
      DO 2 I=0,L
       FLAG=SIGN(1.,F1(I,J)-VALUE)
       F2(I,J)=0.5*(F1(I,J)*(1.-FLAG)+VALUE*(1.+FLAG))
    2 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE VECMAXC(F1,F2,F3)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING AN ARRAY VARIABLE
C=======================================================================
      PARAMETER(L=74,   M=14)
C=======================================================================
      DIMENSION F1(0:L,0:M),F2(0:L,0:M),F3(0:L,0:M)
C=======================================================================
      DO 1 J=0,M
      DO 1 I=0,L
       FLAG=SIGN(1.,F1(I,J)-F2(I,J))
       F3(I,J)=0.5*(F1(I,J)*(1.+FLAG)+F2(I,J)*(1.-FLAG))
    1 CONTINUE
      RETURN
C=======================================================================
      ENTRY VECMINC(F1,F2,F3)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING AN ARRAY VARIABLE
C=======================================================================
      DO 2 J=0,M
      DO 2 I=0,L
       FLAG=SIGN(1.,F1(I,J)-F2(I,J))
       F3(I,J)=0.5*(F1(I,J)*(1.-FLAG)+F2(I,J)*(1.+FLAG))
    2 CONTINUE
      RETURN
      END
