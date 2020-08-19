C Author: Robert Grumbine
C LAST MODIFIED: 26 August 1994.

C     Physical Parameters
      REAL F0, RHOICE, RHOSNO, CON, CONSN, TMELT, TFREZ, CC
      REAL CLO, CLB, RHOAIR, RHOWAT, CDWAT, VAPL, SUBL
      REAL EPSILW, D3, SIGMA, ALBI, ALBM, ALBW, ALBSN, ALBSNM
      REAL ZOI, ZOWI
      REAL CPAIR, KAPPA, CSENS, CLAT, EPSI, GRAV
      REAL RGAS, VONKAR, ATMLEV, OMEGA, LFUSE

C     Parameters known to high precision
      PARAMETER (CPAIR  = 1004.6)     !NMC Handbook
      PARAMETER (KAPPA  = 2. / 7.)
      PARAMETER (RGAS   = 8.31436E3 / 28.964)
      PARAMETER (EPSI   = 0.6219886)  !CRC #72
      PARAMETER (GRAV   = 9.8062 )    !NMC Handbook
      PARAMETER (OMEGA  = 7.2921E-5)  !NMC Handbook
      PARAMETER (TMELT  = 273.16)     !NMC Handbook
      PARAMETER (VAPL   = 2.5008E6 )  !Gill 0C
      PARAMETER (SUBL   = 2.83459E6)  !Gill 0C
      PARAMETER (SIGMA  = 5.67051E-8 )   !Stefan-Boltzmann constant
      PARAMETER (LFUSE  = SUBL-VAPL)
      PARAMETER (F0     = 2.*OMEGA)

C     Parameters constrained, but not high precision
      PARAMETER (VONKAR = 0.4)
      PARAMETER (RHOICE = 9.1E2)
      PARAMETER (RHOSNO = 3.3E2)
      PARAMETER (CON    = 2.1656)  ! Thermal Conductivity of ice
      PARAMETER (CONSN  = 0.31)    ! Thermal Conductivity of snow
      PARAMETER (TFREZ  = -1.84)   ! Tf for water at 34.5 psu, Gill
      PARAMETER (CC     = 4.217E6) ! Specific heat of pure water, Gill
      PARAMETER (CLO    = LFUSE*RHOICE)
      PARAMETER (CLB    = 0.9*LFUSE*RHOICE)
      PARAMETER (RHOAIR = 1.29)
      PARAMETER (RHOWAT = 1.028E3)
      PARAMETER (EPSILW = 0.97   )
      PARAMETER (D3     = EPSILW*SIGMA)

C     Parameters not well known at all, or model-selected
      PARAMETER (CDWAT  = 5.5E-3)
      PARAMETER (CSENS  = 1.75E-3)
      PARAMETER (CLAT   = 1.75E-3)
      PARAMETER (ALBI   = 0.75)  ! Relict.  Albedo subroutine used now.
      PARAMETER (ALBM   = 0.66)  ! Relict.  Albedo subroutine used now.
      PARAMETER (ALBW   = 0.10)  ! Relict.  Albedo subroutine used now.
      PARAMETER (ALBSN  = 0.85)  ! Relict.  Albedo subroutine used now.
      PARAMETER (ALBSNM = 0.75)  ! Relict.  Albedo subroutine used now.
      PARAMETER (ZOI    = 1.E-3)
      PARAMETER (ZOWI   = 1.E-4)
      PARAMETER (ATMLEV = 8.5E4)
