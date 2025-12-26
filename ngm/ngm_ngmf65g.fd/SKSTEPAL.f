      SUBROUTINE SKSTEPAL ( DTTS )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SKSTEPAL    CALLS SKSTEP1 FOR THE NGRDUSE GRIDS
C   PRGMMR: J TUCCILLO       ORG: W/NMC4     DATE: 90-06-17
C
C ABSTRACT: CALLS THE SR WHICH TIMEMARCHES THE
C   .       SURFACE TEMPERATURE FOR THE NGRDUSE
C   .       GRIDS.
C   .
C
C PROGRAM HISTORY LOG:
C   90-06-17  J TUCCILLO
C
C USAGE:  CALL SKSTEPAL ( DTTS )
C   INPUT ARGUMENT LIST:
C     DTTS     - TIMESTEP FOR TS
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - SKSTEP1
C     LIBRARY:
C       COMMON   - COMCONST
C                  COMBLANK
C
C REMARKS:
C   .
C   - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C   .
C   NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----       ----------------------------------        ---------
C   .
C   VBL         UPDATED SURFACE TEMPERATURE IS REPLACED    COMMON
C   .           IN VBL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C
      LOGICAL LPR
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
      LOGICAL BITGRDH, BITGRDU, BITGRDV, BITSEA, BITSNO, BITWVL
C
      DTTT=DTTS
      LPR =.FALSE.
C
      DO 900 NG = 1 , NGRDUSE
         IM = IMG(NG)
         JM = JMG(NG)
         IADJ1 = 2*IM+1
         LEN = (JM -3 ) * IM
         CALL SKSTEP1 ( 1, NG, IADJ1, LEN, DTTT, LPR )
  900 CONTINUE
C
      RETURN
      END
