      SUBROUTINE SKTGRND0 ( JD , GMT )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SKTGRND0    CALLS SRS  SKSKINT0, & RDSTEP1
C   PRGMMR: J TUCCILLO       ORG: W/NMC4     DATE: 90-06-17
C
C ABSTRACT: CALLS THE SRS WHICH COMPUTE THE INITIAL
C   .       SURFACE AND SUBSOIL TEMPERATURES BEFORE THE
C   .       FORECAST STARTS. ALSO THE INITIAL FREE
C   .       ATMOSPHERIC HEATING RATE IS CALCULATED.
C   .
C
C PROGRAM HISTORY LOG:
C   90-06-17  J TUCCILLO
C
C USAGE:  CALL SKTGRND0 ( JD , GMT )
C   INPUT ARGUMENT LIST:
C     JD       - NUMBER OF DAYS SINCE 1 JANUARY
C                INCLUSIVE
C     GMT      - CURRENT TIME
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - SKSKINT0
C                  RDSTEP1
C     LIBRARY:
C       COMMON   - COMCONST
C                  COMBLANK
C
C REMARKS:
C   - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C   .
C   NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----       ----------------------------------        ---------
C   .
C   -------     INITIAL TS AND THE RADIATIVE         COMMON
C   .           FLUXES AND HEATING RATES ARE PLACED IN
C   .           THE APPROPRIATE ARRAY LOCATIONS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C
      LOGICAL LPR, LSUBSOIL
C
C     COMMON BLOCK /COMCONST/ CONTAINS GRID-RELATED PARAMETERS,
C          AND A FEW OTHER COMMON CONSTANTS.
      COMMON /COMCONST/ IJMAX, KM, LVLTOT, IJRAD, LOFCLDS(2,4),
     1                  ICALLRAD, IPHYSPL, NGRDUSE, NH,
     2                  NTIME, ITIME, NSTEPS,
     3                  IMG(INGRDUSE), JMG(INGRDUSE),
     4                  IAG(INGRDUS1), JAG(INGRDUS1),
     5                  IBG(INGRDUS1), JBG(INGRDUS1),
     6                  IADDRG(INIADDRS, INGRDUSE),
     7                  NPTSFH(2, INGRDUSE),
     8                  KUMULUS, LGRIDPPT, KLIFT1, KLIFT2, IBUCKET,
     9                  XPOLEH(INGRDUSE), YPOLEH(INGRDUSE), RADIUS,
     1                  DELSIG(IKM), PR(IKM), PRESS(IKM),
     2                  SIGINT(IKMP1),
     3                  DTOVDX, ANGVEL,
     4                  SIGMACC, SIGMAGSP, SIGMADHQ, CRITCONV,
     5                  SATDEL, RHFACTOR, QBOUND,
     6                  ANEM, BLKDR, CHARN, CONAUST, DDORF, PKATO,
     7                  SCALEHT, SIGDOT, DLAMNGM
C
      COMMON            SCR     (IIJMAX,  INSCR),
     1                  SCRGEOG (IIJMAX,  INSCRGEO),
     2                  SCR3    (IIJKMAX, INSCR3),
     3                  FILLER  (INFILLER),
     4                  VBL     (INVBL),
     5                  BITGRDH (IIJMAX, 2, INGRDUSE),
     6                  BITGRDU (IIJMAX, 2, INGRDUSE),
     7                  BITGRDV (IIJMAX, 2, INGRDUSE),
     8                  BITSEA  (IIJMAX, INGRDUSE),
     9                  BITSNO  (IIJMAX, INGRDUSE),
     1                  BITWVL  (IIJMAX, INGRDUSE)
C
      COMMON/GOLDBERG/ ICRAY
C
      LOGICAL BITGRDH, BITGRDU, BITGRDV, BITSEA, BITSNO, BITWVL
C
C
      IT     = 40
      LPR    = .FALSE.
      IF ( ICRAY .EQ. 1 ) THEN
         LSUBSOIL = .FALSE.
      ELSE
         LSUBSOIL = .TRUE.
      END IF
C
C     COMPUTE RADIATION AT T-3 HOURS FOR COMPUTATION OF INITIAL TS
      GMTTEMP = GMT - 3.5E0
C
      IF ( GMTTEMP .LT. 0.E0 ) THEN
         GMTTEMP = GMTTEMP + 24.E0
      END IF
      DO 900 NG = 1,NGRDUSE
         NGG = NG
         IM = IMG(NGG)
         JM = JMG(NGG)
         IADJ1 = 2*IM+1
         LEN = (JM -3 ) * IM
         IF ( LSUBSOIL) THEN
            CALL RDSTEP1 ( 1, NGG, IADJ1, LEN, JD, GMT, 1, 3)
            CALL SKSBSOIL( 1, NGG, IADJ1, LEN, IT, LPR    )
         END IF
         CALL RDSTEP1 ( 1, NGG, IADJ1, LEN, JD, GMTTEMP, 0, 3)
         CALL SKSKINT0( 1, NGG, IADJ1, LEN, IT, LPR    )
         CALL RDSTEP1 ( 1, NGG, IADJ1, LEN, JD, GMT, 0, 3)
  900 CONTINUE
C
      RETURN
      END
