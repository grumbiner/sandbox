      SUBROUTINE RDSTEPAL ( JD, GMT, ISW )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDSTEPAL    CALLS THE SR THAT COMPUTES RADIATION
C   PRGMMR: J TUCCILLO       ORG: W/NMC40    DATE: 90-06-19
C
C ABSTRACT: CALLS THE SR WHICH COMPUTES THE
C   .       RADIATION FOR THE NGRDUSE GRIDS
C   .
C PROGRAM HISTORY LOG:
C   90-06-19  J TUCCILLO
C
C USAGE:  CALL RDSTEPAL ( JD, GMT, ISW )
C   INPUT ARGUMENT LIST:
C    JD        - JULIAN DAY
C    GMT       - CURRENT TIME
C    ISW       - SWITCH FOR WHICH RADIATION CALCULATIONS
C                TO PERFORM
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - RDSTEP1
C     LIBRARY:
C       COMMON   - COMCONST
C                  COMBLANK
C REMARKS:
C   .
C   - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C   .
C   NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----       ----------------------------------        ---------
C   .
C   -------     RESULTS OF SR RDSTEP1 ARE PUT INTO        COMMON
C                 ARRAY VBL
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
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
      DO 900 NG = 1,NGRDUSE
         NGG= NG
         IM = IMG(NGG)
         JM = JMG(NGG)
         IADJ1 = 2*IM+1
         LEN = (JM -3 ) * IM
         CALL RDSTEP1( 1, NGG, IADJ1, LEN, JD, GMT, 0, ISW )
  900 CONTINUE
C
      RETURN
      END
