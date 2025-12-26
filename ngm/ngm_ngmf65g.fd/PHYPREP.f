      SUBROUTINE PHYPREP ( DT, KHINTERP )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PHYPREP     CALLS THE PHYSICS SUBROUTINE
C   PRGMMR: J TUCCILLO       ORG: W/NMC40    DATE: 90-06-21
C
C ABSTRACT:   STAGES THE ACCUMULATED HQ INTO SCR3(1,4) FOR SUBROUTINE
C   .         PHYSICS, CALLS PHYSICS AND BOUNDARY CONDITION ROUTINES.
C
C PROGRAM HISTORY LOG:
C   90-06-21  J. TUCCILLO
C
C USAGE:    CALL PHYPREP ( DT, KHINTERP )
C   INPUT ARGUMENT LIST:
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     DT          PHYSICS TIME STEP                         ARGUMENT
C
C     KHINTERP    NUMBER OF H FIELDS TO INTERPOLATE         ARGUMENT
C                 FOR THE HOR. BNDY. CONDITIONS
C
C   OUTPUT ARGUMENT LIST:
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     NONE
C
C   SUBPROGRAMS CALLED:
C         NAMES                    LIBRARY
C       --------                 -----------
C
C        PHYSICS                  FORECAST
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C...TRANSLATED BY FPP 3.00Z36 11/09/90  14:51:01  
C...SWITCHES: OPTON=I47,OPTOFF=VAE0
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
      COMMON /COM1WAY/ ITIMSTEP, ITBOUND, INGLB
C
      NGRDM1X = NGRDUSE - 1
C
      DO 200 NG = NGRDUSE,1,-1
C
         NGG=NG
         IM = IMG(NGG)
         JM = JMG(NGG)
         IJ = IM * JM
C
         IF ( NGG .EQ. 1 .OR. NGG .EQ. INGLB ) THEN
            IADJ1 = 2 * IM + 1
            LEN = ( JM - 3 ) * IM
            NPAIR = 1
         ELSE
            IADJ1 = 4 * IM + 1
            LEN = ( JM - 7 ) * IM
            NPAIR = 2
         END IF
C
         JAD1   = 1 - IJ
         JADHQ  = IADDRG(4, NGG) - IJ
         JADDHQ = IADDRG(23,NGG) - IJ
C
CMIC$ DO ALL SHARED(KM,JAD1,IJ,JADHQ,JADDHQ,VBL,SCR3)PRIVATE(K,IQ2W6E)
      DO 100 K = 1, KM
         DO 77001 IQ2W6E = 1, IJ
            SCR3(JAD1+IQ2W6E-1+K*IJ,4) = VBL(JADHQ+IQ2W6E-1+K*IJ) - VBL(
     1         JADDHQ+IQ2W6E-1+K*IJ)
77001    CONTINUE
  100 CONTINUE
C
         CALL PHYSICS ( NGG, NPAIR, 1, IADJ1, LEN, DT )
C
         IF ( NGG .LE. NGRDM1X ) CALL LATBND ( NGG, KHINTERP )
C
200   CONTINUE
C
C     APPLY EQUATORIAL BOUNDARY CONDITION
C
      CALL SHEM ( KHINTERP )
C
C     RESET DHQ
C
      DO 400 NG = 1, NGRDUSE
         IJ = IMG (NG) * JMG (NG)
         JADHQ  = IADDRG(4, NG) - IJ
         JADDHQ = IADDRG(23,NG) - IJ
         DO 300 K=1, KM
            JADHQ  = JADHQ  + IJ
            JADDHQ = JADDHQ + IJ
CMIC$ DO ALL VECTOR IF (ABS(JADHQ-JADDHQ).GE.IJ .OR. JADHQ-JADDHQ.EQ.0)
CMIC$1    SHARED(IJ, JADHQ, JADDHQ, VBL) PRIVATE(IQ2W6E)
      DO 88900 IQ2W6E=1,IJ
         VBL(JADDHQ+IQ2W6E-1)=VBL(JADHQ+IQ2W6E-1)
88900 CONTINUE
300      CONTINUE
400   CONTINUE
C
C
      RETURN
      END
