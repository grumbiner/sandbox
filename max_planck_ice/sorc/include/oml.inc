C Author: Robert Grumbine
C LAST MODIFIED: 28 September 1994.

C     Ocean mixed layer model parameters
      REAL QHW, QHS, ENTMAX, SICE, CW, BETAS, BETAT, GAMMAT
      REAL EPSAA, minmix
      PARAMETER (minmix = 1.0)    ! Minimum mixed layer thickness
      PARAMETER (QHW    = 7.0)
      PARAMETER (QHS    = 50.)
      PARAMETER (ENTMAX = 30.)
      PARAMETER (SICE   = 5.0)
      PARAMETER (CW     = 0.005)
      PARAMETER (BETAS  = 8.E-4)  ! Specific density change due to salinity
      PARAMETER (BETAT  = 4.E-5)  ! Specific density change due to temperature
      PARAMETER (GAMMAT  = BETAT/8.) ! Quadratic term for density as fn. of temp
      PARAMETER (EPSAA  = 2.E-6)
