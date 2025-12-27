      SUBROUTINE MISCLN(IMOUT,JMOUT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    MISCLN      POSTS MISCELLANEOUS FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-20
C     
C ABSTRACT:
C     THIS ROUTINE HAS BECOME THE CATCH-ALL FOR MISCELLANEOUS
C     OUTPUT FIELDS POSTED BY THE ETA POST PROCESSOR.  
C     CURRENTLY THIS ROUTINE POSTS THE FOLLOWING FIELDS:
C        (1) TROPOPAUSE LEVEL Z,P, T, U, V, AND VERTICAL WIND SHEAR,
C        (2) MAX WIND LEVEL Z, P, U, AND V,
C        (3) FD LEVEL T, U, AND V,
C        (4) FREEZING LEVEL Z AND RH,
C        (5) CONSTANT MASS (BOUNDARY) FIELDS,
C        (6) LFM LOOK-ALIKE FIELDS, AND
C        (7) NGM LOOK-ALIKE FIELDS.
C
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-20  RUSS TREADON
C   93-06-19  RUSS TREADON - ADDED TYPE 2 CAPE POSTING.
C   94-11-07  MIKE BALDWIN - ADDED HELICITY POSTING.
C   96-03-26  MIKE BALDWIN - CHANGE ETA BOUNDARY LAYER LABELS FOR GRIB
C   96-11-19  MIKE BALDWIN - BACK OUT PREVIOUS CHANGE 
C   97-04-25  MIKE BALDWIN - CHANGE ETA BOUNDARY LAYER LABELS FOR GRIB
C   97-04-29  GEOFF MANIKIN - ADDED TROPOPAUSE HEIGHT AND
C                             MAX WIND LEVEL FIELDS
C   98-06-15  T BLACK       - CONVERSION FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN - REMOVED LABL84
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL MISCLN(IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     STDOUT   - RUN-TIME MESSAGES (FOR DEBUGGING, NOW TURNED OFF)
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       TRPAUS  - COMPUTE TROPOPAUSE LEVEL FIELDS.
C       CALMXW  - COMPUTE MAX WIND LEVEL FIELDS.
C       E2OUT   - INTERPOLATE STAGGERED E-GRID TO OUTPUT GRID.
C       SCLFLD  - SCALE ARRAY ELEMENTS BY CONSTANT.
C       OUTPUT  - DRIVER FOR TYPE OF FILE OUTPUT.
C       CALPOT2 - CALCULATE POTENTIAL TEMPERATURE.
C       FDLVL   - COMPUTE FD LEVEL DATA (AGL OR MSL).
C       FRZLVL  - COMPUTE FREEZING LEVEL DATA.
C       BOUND   - BOUND ARRAY ELEMENTS BETWEEN MINIMUM AND MAXIMUM VALUES.
C       BNDLYR2 - COMPUTE BOUNDARY LAYER FIELDS.
C       CALDWP2 - CALCULATE DEWPOINT TEMPERATURE.
C       CALMCVG - CALCULATE MOISTURE CONVERGENCE.
C       OTLFT2  - COMPUTE LIFTED INDEX AT 500MB.
C       CALLCL  - COMPUTE LCL DATA.
C       LFMFLD  - COMPUTE LFM LOOK-ALIKE FIELDS.
C       NGMFLD2 - COMPUTE NGM LOOK-ALIKE FIELDS.
C       CALTHTE - COMPUTE THETA-E.
C       CALHEL  - COMPUTE HELICITY AND STORM MOTION.
C
C     LIBRARY:
C       COMMON - RQSTFLD
C                MASKS
C                IOUNIT
C                OPTIONS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C
C     INCLUDE GRID DIMENSIONS.  DERIVE DEPENDENT PARAMETERS.
C
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "params"
C     
C     SET LOCAL PARAMETERS.  MAKE SURE NFD AND NBND AGREE
C     WITH THE VALUES SET IN SUBROUTINES FDLVL AND BNDLYR2,
C     RESPECTIVELY.
      PARAMETER (NFD=6,NBND=6,SMALL=1.E-12)
      PARAMETER (C2K=273.15)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL NORTH
      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STDRD
      LOGICAL FIELD1,FIELD2
      INTEGER LVLBND(IM,JM,NBND),LB2(IM,JM)
      REAL P1D(IM,JM),T1D(IM,JM),Q1D(IM,JM),U1D(IM,JM),V1D(IM,JM)
      REAL SHR1D(IM,JM),Z1D(IM,JM),RH1D(IM,JM)
      REAL OMGBND(IM,JM,NBND),PWTBND(IM,JM,NBND)
      REAL PBND(IM,JM,NBND),TBND(IM,JM,NBND),QBND(IM,JM,NBND)
      REAL UBND(IM,JM,NBND),VBND(IM,JM,NBND),RHBND(IM,JM,NBND)
      REAL T78483(IM,JM),T89671(IM,JM),P78483(IM,JM),P89671(IM,JM)
      REAL QM8510(IM,JM),RH4710(IM,JM),RH8498(IM,JM)
      REAL RH4796(IM,JM),RH1847(IM,JM),UST(IM,JM),VST(IM,JM)
      REAL RH3310(IM,JM),RH6610(IM,JM),RH3366(IM,JM),PW3310(IM,JM)
      REAL HTFD(NFD),T6D(IM,JM,NFD),U6D(IM,JM,NFD),V6D(IM,JM,NFD)
      REAL PETABND(NBND),SIGBND(NBND),HELI(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM)
      REAL GRID1(IMOUT,JMOUT),GRID2(IMOUT,JMOUT)
      REAL MAXWP(IM,JM),MAXWZ(IM,JM),MAXWU(IM,JM), MAXWV(IM,JM)
      
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "RQSTFLD.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "DYNAM.comm"
      INCLUDE "CTLBLK.comm"
C     
C
C     EQUIVALENCES FOR SUBROUTINE MISCLN.
      EQUIVALENCE (TBND(1,1,1),T6D(1,1,1))
      EQUIVALENCE (TBND(1,1,2),T6D(1,1,2))
      EQUIVALENCE (TBND(1,1,3),T6D(1,1,3))
      EQUIVALENCE (TBND(1,1,4),T6D(1,1,4))
      EQUIVALENCE (TBND(1,1,5),T6D(1,1,5))
      EQUIVALENCE (TBND(1,1,6),T6D(1,1,6))
      EQUIVALENCE (UBND(1,1,1),U6D(1,1,1))
      EQUIVALENCE (UBND(1,1,2),U6D(1,1,2))
      EQUIVALENCE (UBND(1,1,3),U6D(1,1,3))
      EQUIVALENCE (UBND(1,1,4),U6D(1,1,4))
      EQUIVALENCE (UBND(1,1,5),U6D(1,1,5))
      EQUIVALENCE (UBND(1,1,6),U6D(1,1,6))
      EQUIVALENCE (VBND(1,1,1),V6D(1,1,1))
      EQUIVALENCE (VBND(1,1,2),V6D(1,1,2))
      EQUIVALENCE (VBND(1,1,3),V6D(1,1,3))
      EQUIVALENCE (VBND(1,1,4),V6D(1,1,4))
      EQUIVALENCE (VBND(1,1,5),V6D(1,1,5))
      EQUIVALENCE (VBND(1,1,6),V6D(1,1,6))
C     
C     SET FD LEVEL HEIGHTS IN GEOPOTENTAL METERS.
      DATA HTFD  / 914.E0,1524.E0,1829.E0,
     X     2134.E0,2743.E0,3658.E0/
C     
C     SET MIDPOINT "SIGMA" VALUES FOR ETA BOUNDARY LAYERS.
      DATA SIGBND / 0.985,0.955,0.925,0.895,0.865,0.835 /
      DATA PETABND / 15.,45.,75.,105.,135.,165. /
C     
C****************************************************************************
C     START MISCLN HERE.
C     
C        HELICITY AND STORM MOTION.
       IF (IGET(162).GT.0.OR.IGET(163).GT.0.OR.IGET(164).GT.0) THEN
         CALL CALHEL(UST,VST,HELI)
         IF (IGET(162).GT.0) THEN
            CALL E2OUT(162,000,HELI,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            ID(10)   = 30
            ID(11)   = 0
            CALL OUTPUT(IOUTYP,IGET(162),LVLS(1,IGET(162)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
         IF ((IGET(163).GT.0).OR.(IGET(164).GT.0)) THEN
            CALL E2OUT(163,164,UST,VST,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            ID(10)   = 60
            ID(11)   = 0 
            IF (IGET(163).GT.0) CALL OUTPUT(IOUTYP,IGET(163),
     X           LVLS(1,IGET(163)),GRID1,IMOUT,JMOUT)
            ID(1:25) = 0
            ID(10)   = 60
            ID(11)   = 0 
            IF (IGET(164).GT.0) CALL OUTPUT(IOUTYP,IGET(164),
     X           LVLS(1,IGET(164)),GRID2,IMOUT,JMOUT)
         ENDIF
       ENDIF
C     
C
C
C     ***BLOCK 1:  TROPOPAUSE P, Z, T, U, V, AND WIND SHEAR.
C    
      IF ( (IGET(054).GT.0).OR.(IGET(055).GT.0).OR.
     X     (IGET(056).GT.0).OR.(IGET(057).GT.0).OR.
     X     (IGET(177).GT.0).OR.
     X     (IGET(058).GT.0).OR.(IGET(108).GT.0) ) THEN
         CALL TRPAUS(P1D,T1D,Z1D,U1D,V1D,SHR1D) 
C
C        TROPOPAUSE PRESSURE.
         IF (IGET(054).GT.0) THEN
            CALL E2OUT(054,000,P1D,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(054),LVLS(1,IGET(054)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF

C        TROPOPAUSE HEIGHT.
         IF (IGET(177).GT.0) THEN
            CALL E2OUT(177,000,Z1D,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(177),LVLS(1,IGET(177)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C        TROPOPAUSE TEMPERATURE.
         IF (IGET(055).GT.0) THEN
            CALL E2OUT(055,000,T1D,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(055),LVLS(1,IGET(055)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C        TROPOPAUSE POTENTIAL TEMPERATURE.
         IF (IGET(108).GT.0) THEN
            CALL CALPOT2(P1D,T1D,EGRID1,IM,JM)
            CALL E2OUT(108,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(108),LVLS(1,IGET(108)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        TROPOPAUSE U WIND AND/OR V WIND.
         IF ((IGET(056).GT.0).OR.(IGET(057).GT.0)) THEN
            CALL E2OUT(056,057,U1D,V1D,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IF (IGET(056).GT.0) CALL OUTPUT(IOUTYP,IGET(056),
     X           LVLS(1,IGET(056)),GRID1,IMOUT,JMOUT)
            ID(1:25) = 0
            IF (IGET(057).GT.0) CALL OUTPUT(IOUTYP,IGET(057),
     X           LVLS(1,IGET(057)),GRID2,IMOUT,JMOUT)
         ENDIF
C
C        TROPOPAUSE WIND SHEAR.
         IF (IGET(058).GT.0) THEN
            CALL E2OUT(058,000,SHR1D,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(058),LVLS(1,IGET(058)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
      ENDIF
C
C
C
C     ***BLOCK 2:  MAX WIND LEVEL  P, Z, U, AND V
C
C        MAX WIND LEVEL CALCULATIONS
         IF ((IGET(173).GT.0) .OR. (IGET(174).GT.0) .OR.
     X      (IGET(175).GT.0) .OR. (IGET(176).GT.0)) THEN
            CALL CALMXW(MAXWP,MAXWZ,MAXWU,MAXWV)
         ENDIF
C        PRESSURE OF MAX WIND LEVEL
         IF (IGET(173).GT.0) THEN
            CALL E2OUT(173,000,MAXWP,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(173),LVLS(1,IGET(173)),
     X           GRID1,IMOUT,JMOUT)
          ENDIF
C        HEIGHT OF MAX WIND LEVEL
         IF (IGET(174).GT.0) THEN
            CALL E2OUT(174,000,MAXWZ,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(174),LVLS(1,IGET(174)),
     X           GRID1,IMOUT,JMOUT)
          ENDIF

C        MAX WIND LEVEL U WIND AND/OR V WIND.
         IF ((IGET(175).GT.0).OR.(IGET(176).GT.0)) THEN
            CALL E2OUT(175,176,MAXWU,MAXWV,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IF (IGET(175).GT.0) CALL OUTPUT(IOUTYP,IGET(175),
     X           LVLS(1,IGET(175)),GRID1,IMOUT,JMOUT)
            ID(1:25) = 0
            IF (IGET(176).GT.0) CALL OUTPUT(IOUTYP,IGET(176),
     X           LVLS(1,IGET(176)),GRID2,IMOUT,JMOUT)
         ENDIF
C
C
C
C     ***BLOCK 3:  FD LEVEL T, U, AND V.
C     
      IF ( (IGET(059).GT.0).OR.(IGET(060).GT.0).OR.
     X     (IGET(061).GT.0) ) THEN
C
C     DETERMINE WHETHER TO DO MSL OR AGL FD LEVELS
C
         ITYPE=1
         DO IFD = 1,NFD
           IF (IGET(059).GT.0) THEN
            IF (LVLS(IFD,IGET(059)).GT.1) ITYPE=2
           ENDIF
           IF (IGET(060).GT.0) THEN
            IF (LVLS(IFD,IGET(060)).GT.1) ITYPE=2
           ENDIF
           IF (IGET(061).GT.0) THEN
            IF (LVLS(IFD,IGET(061)).GT.1) ITYPE=2
           ENDIF
         ENDDO
         CALL FDLVL(ITYPE,T6D,U6D,V6D)
C     
         DO 10 IFD = 1,NFD
            ID(1:25) = 0
            ISVALUE = NINT(HTFD(IFD))
            ID(11) = ISVALUE
C
C           FD LEVEL TEMPERATURE.
            IF (IGET(059).GT.0) THEN
              IF (LVLS(IFD,IGET(059)).GT.0) THEN
               CALL E2OUT(059,000,T6D(1,1,IFD),EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(059),
     X              LVLS(IFD,IGET(059)),GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C
C           FD LEVEL U WIND AND/OR V WIND.
            IF ((IGET(060).GT.0).OR.(IGET(061).GT.0)) THEN
               CALL E2OUT(060,061,U6D(1,1,IFD),V6D(1,1,IFD),
     X              GRID1,GRID2,IMOUT,JMOUT)
               IF (IGET(060).GT.0) THEN
                 IF (LVLS(IFD,IGET(060)).GT.0) CALL OUTPUT(IOUTYP,
     X              IGET(060),LVLS(IFD,IGET(060)),GRID1,IMOUT,JMOUT)
               ENDIF
               IF (IGET(061).GT.0) THEN
                 IF (LVLS(IFD,IGET(061)).GT.0) CALL OUTPUT(IOUTYP,
     X              IGET(061),LVLS(IFD,IGET(061)),GRID2,IMOUT,JMOUT)
               ENDIF
            ENDIF
 10      CONTINUE
      ENDIF
C     
C
C
C     ***BLOCK 4:  FREEZING LEVEL Z AND RH.
C     
      IF ( (IGET(062).GT.0).OR.(IGET(063).GT.0) ) THEN
         CALL FRZLVL(Z1D,RH1D)
C
C        FREEZING LEVEL HEIGHT.
         IF (IGET(062).GT.0) THEN
            CALL E2OUT(062,000,Z1D,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL BOUND (GRID1,D00,H99999,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(062),LVLS(1,IGET(062)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C        FREEZING LEVEL RELATIVE HUMIDITY.
         IF (IGET(063).GT.0) THEN
            CALL E2OUT(063,000,RH1D,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(063),LVLS(1,IGET(063)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
      ENDIF
      IF (IGET(165).GT.0) THEN
         CALL FRZLVL2(Z1D,RH1D)
C
C        HIGHEST FREEZING LEVEL HEIGHT.
            CALL E2OUT(165,000,Z1D,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL BOUND (GRID1,D00,H99999,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(165),LVLS(1,IGET(165)),
     X           GRID1,IMOUT,JMOUT)
      ENDIF
C     
C
C
C     ***BLOCK 5:  BOUNDARY LAYER FIELDS.
C     
      IF ( (IGET(067).GT.0).OR.(IGET(068).GT.0).OR.
     X     (IGET(069).GT.0).OR.(IGET(070).GT.0).OR.
     X     (IGET(071).GT.0).OR.(IGET(072).GT.0).OR.
     X     (IGET(073).GT.0).OR.(IGET(074).GT.0).OR.
     X     (IGET(088).GT.0).OR.(IGET(089).GT.0).OR.
     X     (IGET(090).GT.0).OR.(IGET(075).GT.0).OR.
     X     (IGET(109).GT.0).OR.(IGET(110).GT.0).OR.
     X     (IGET(031).GT.0).OR.(IGET(032).GT.0).OR.
     X     (IGET(107).GT.0).OR.(IGET(091).GT.0).OR.
     X     (IGET(092).GT.0).OR.(IGET(093).GT.0).OR.
     X     (IGET(094).GT.0).OR.(IGET(095).GT.0).OR.
     X     (IGET(096).GT.0).OR.(IGET(097).GT.0).OR.
     X     (IGET(098).GT.0) ) THEN
C
C        COMPUTE ETA BOUNDARY LAYER FIELDS.
         CALL BNDLYR2(PBND,TBND,QBND,RHBND,UBND,VBND,
     X        OMGBND,PWTBND,LVLBND)
C     
C        LOOP OVER NBND BOUNDARY LAYERS.
         DO 20 LBND = 1,NBND
            ID(1:25) = 0
            ID(10)   = NINT(PETABND(LBND)+15.)
            ID(11)   = NINT(PETABND(LBND)-15.)
C     
C           BOUNDARY LAYER PRESSURE.
            IF (IGET(067).GT.0) THEN
              IF (LVLS(LBND,IGET(067)).GT.0) THEN
               CALL E2OUT(067,000,PBND(1,1,LBND),EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(067),
     X              LVLS(LBND,IGET(067)),GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER TEMPERATURE.
            IF (IGET(068).GT.0) THEN
              IF (LVLS(LBND,IGET(068)).GT.0) THEN
               CALL E2OUT(068,000,TBND(1,1,LBND),EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(068),LVLS(LBND,IGET(068)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER POTENTIAL TEMPERATURE.
            IF (IGET(069).GT.0) THEN
              IF (LVLS(LBND,IGET(069)).GT.0) THEN
               CALL CALPOT2(PBND(1,1,LBND),TBND(1,1,LBND),EGRID1,IM,JM)
               CALL E2OUT(069,000,EGRID1,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(069),LVLS(LBND,IGET(069)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER RELATIVE HUMIDITY.
            IF (IGET(072).GT.0) THEN
              IF (LVLS(LBND,IGET(072)).GT.0) THEN
               CALL E2OUT(072,000,RHBND(1,1,LBND),EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(072),LVLS(LBND,IGET(072)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER DEWPOINT TEMPERATURE.
            IF (IGET(070).GT.0) THEN
              IF (LVLS(LBND,IGET(070)).GT.0) THEN
               CALL CALDWP2(PBND(1,1,LBND),QBND(1,1,LBND),EGRID1,
     X              TBND(1,1,LBND))
               CALL E2OUT(070,000,EGRID1,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(070),LVLS(LBND,IGET(070)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER SPECIFIC HUMIDITY.
            IF (IGET(071).GT.0) THEN
              IF (LVLS(LBND,IGET(071)).GT.0) THEN
               CALL E2OUT(071,000,QBND(1,1,LBND),EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1M12,H99999,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(071),LVLS(LBND,IGET(071)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER MOISTURE CONVERGENCE.
            IF (IGET(088).GT.0) THEN
              IF (LVLS(LBND,IGET(088)).GT.0) THEN
               CALL CALMCVG(QBND(1,1,LBND),UBND(1,1,LBND),
     X              VBND(1,1,LBND),-1,EGRID1)
C          CONVERT TO DIVERGENCE FOR GRIB
               DO J=JSTA,JEND
               DO I=1,IM
                 EGRID1(I,J)=-1.0*EGRID1(I,J)
               ENDDO
               ENDDO
               CALL E2OUT(088,000,EGRID1,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(088),LVLS(LBND,IGET(088)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER U WIND AND/OR V WIND.
C
            FIELD1=.FALSE.
            FIELD2=.FALSE.
C
            IF(IGET(073).GT.0)THEN
              IF(LVLS(LBND,IGET(073)).GT.0)FIELD1=.TRUE.
            ENDIF
            IF(IGET(074).GT.0)THEN
              IF(LVLS(LBND,IGET(074)).GT.0)FIELD2=.TRUE.
            ENDIF
C
            IF(FIELD1.OR.FIELD2)THEN
              CALL E2OUT(073,074,UBND(1,1,LBND),VBND(1,1,LBND),
     X                   GRID1,GRID2,IMOUT,JMOUT)
C
C              TWO ROUTINES WERE WRITTEN DURING POST PROCESSOR
C              DEVELOPMENT TO MIMIC LFM BOUNDARY LAYER WINDS.
C              BOTH APPLY AN EKMAN SPIRAL ROTATION TO AN ARTIFICIALLY
C              REDUCED GEOSTROPHIC WIND FIELD.  SUBROUTINE CALEKM
C              BASES THE GEOSTROPHIC WIND ON 1000MB HEIGHTS WHILE
C              CALEKM2 USES THE SEA LEVEL PRESSURE FIELD.  IF
C              LBND EQUALS ONE AND LVLS IS 8 POST THE CALEKM WINDS
C              AS BOUNDARY LAYER ONE WINDS.  IF LBND EQUALS ONE AND
C              LVLS IS 9 POST CALEKM2 WINDS AS BOUNDARY LAYER ONE
C              WINDS.  OTHERWISE, POST THE "TRUE" BOUNDARY LAYER 
C              ONE WINDS.
C                              - 19 OCTOBER 1993, RUSS TREADON
C                     
               IF (IGET(073).GT.0) THEN
               IF ((LBND.EQ.1).AND.(LVLS(LBND,IGET(073)).EQ.8)) THEN
                  WRITE(STDOUT,*)'MISCLN:  BNDLYR ',LBND,
     X                 ' UV USING EKMAN-GEO Z1000 UV'
                  CALL CALEKM(U1D,V1D)
                  CALL E2OUT(073,074,U1D,V1D,GRID1,GRID2,IMOUT,JMOUT)
               ENDIF
               ENDIF
C
               IF (IGET(073).GT.0) THEN
               IF ((LBND.EQ.1).AND.(LVLS(LBND,IGET(073)).EQ.9)) THEN
                  WRITE(STDOUT,*)'MISCLN:  BNDLYR ',LBND,
     X                 ' UV USING EKMAN-GEO PMSL UV'
                  CALL CALEKM2(U1D,V1D)
                  CALL E2OUT(073,074,U1D,V1D,GRID1,GRID2,IMOUT,JMOUT)
               ENDIF
               ENDIF
C
               IF (IGET(073).GT.0) THEN
                 IF (LVLS(LBND,IGET(073)).GT.0)
     X              CALL OUTPUT(IOUTYP,IGET(073),
     X              LVLS(LBND,IGET(073)),GRID1,IMOUT,JMOUT)
               ENDIF
               IF (IGET(074).GT.0) THEN
                 IF (LVLS(LBND,IGET(074)).GT.0) 
     X              CALL OUTPUT(IOUTYP,IGET(074),
     X              LVLS(LBND,IGET(074)),GRID2,IMOUT,JMOUT)
               ENDIF
            ENDIF
C     
C           BOUNDARY LAYER OMEGA.
            IF (IGET(090).GT.0) THEN
              IF (LVLS(LBND,IGET(090)).GT.0) THEN
               CALL E2OUT(090,000,OMGBND(1,1,LBND),EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(090),LVLS(LBND,IGET(090)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER PRECIPITBLE WATER.
            IF (IGET(089).GT.0) THEN
              IF (LVLS(LBND,IGET(089)).GT.0) THEN
               CALL E2OUT(089,000,PWTBND(1,1,LBND),EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(089),LVLS(LBND,IGET(089)),
     X              GRID1,IMOUT,JMOUT)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER LIFTED INDEX.
            IF (IGET(075).GT.0) THEN
              IF (LVLS(LBND,IGET(075)).GT.0) THEN
               CALL OTLFT2(PBND(1,1,LBND),TBND(1,1,LBND),
     X              QBND(1,1,LBND),EGRID1)
               CALL E2OUT(075,000,EGRID1,EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
C     
C           23 SEPTEMBER 1993, RUSS TREADON.
C             ON LFM FORECAST GRID 026 WE POST THE FIRST ETA 
C             BOUNDARY LAYER LI AS THE SURFACE TO 500MB LI.
C             THE CALL TO E2OUT INTERPOLATES THE LI FROM
C             THE E-GRID TO THE OUTPUT GRID.  IF WE ARE POSTING
C             DATA THE THE LFM GRID WE NEED TO SAVE THIS 
C             INTERPOLATED FIELD FOR USE BELOW.  WHY?  WHEN
C             PACKING THE DATA IN GRIB1 THE VALUES IN GRID1
C             ARE MULTIPLIED BY 10.  IF WE SEND GRID1 TO THE
C             GRIB PACKER A SECOND TIME THE LI'S ARE 10 TIMES 
C             TOO BIG.
C
               CALL E2OUT(075,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               IF (KGTYPE.EQ.026) THEN
                  DO J=JSTA,JEND
                  DO I=1,IMOUT
                     GRID2(I,J) = GRID1(I,J)
                  ENDDO
                  ENDDO
               ENDIF
               
               
               CALL OUTPUT(IOUTYP,IGET(075),LVLS(LBND,IGET(075)),
     X              GRID1,IMOUT,JMOUT)
C
C              25 JUNE 1993, RUSS TREADON.
C                 ON THE LFM FORECAST GRID (026) WE POST THE FIRST
C                 BOUNDARY LAYER LIFTED INDEX AS THE SURFACE LI.
C     
               IF (KGTYPE.EQ.026) THEN
                  DO J=JSTA,JEND
                  DO I=1,IMOUT
                    GRID2(I,J) = GRID2(I,J) + C2K
                  ENDDO
                  ENDDO
C
                  ID(1:25) = 0
                  ID(9) = 101
                  ID(10) = 50
                  ID(11) = 0
                  CALL OUTPUT(IOUTYP,IGET(030),LVLS(1,IGET(030)),
     X                 GRID2,IMOUT,JMOUT)
               ENDIF
              ENDIF
            ENDIF
C
C        END OF ETA BOUNDARY LAYER LOOP.
 20      CONTINUE
C     
C        BEST LIFTED INDEX FROM BOUNDARY LAYER FIELDS.
C     
         IF (IGET(031).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = H99999
              EGRID2(I,J) = H99999
            ENDDO
            ENDDO
C
            DO 50 LBND = 1,NBND
               CALL OTLFT2(PBND(1,1,LBND),TBND(1,1,LBND),
     X              QBND(1,1,LBND),EGRID2)
               DO J=JSTA,JEND
               DO I=1,IM
                 EGRID1(I,J)=AMIN1(EGRID1(I,J),EGRID2(I,J))
               ENDDO
               ENDDO
 50         CONTINUE
            CALL E2OUT(031,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            ID(10)   = PETABND(NBND)+15.
            ID(11)   = PETABND(1)-15.
            CALL OUTPUT(IOUTYP,IGET(031),LVLS(1,IGET(031)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        BEST BOUNDARY LAYER CAPE AND CINS.
C     
         FIELD1=.FALSE.
         FIELD2=.FALSE.
C
         IF(IGET(032).GT.0)THEN
           IF(LVLS(2,IGET(032)).GT.0)FIELD1=.TRUE.
         ENDIF
         IF(IGET(107).GT.0)THEN
           IF(LVLS(2,IGET(107)).GT.0)FIELD2=.TRUE.
         ENDIF
C
         IF(FIELD1.OR.FIELD2)THEN
           ITYPE = 2
C
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = -H99999
             EGRID2(I,J) = -H99999
           ENDDO
           ENDDO
C
           DO 80 LBND = 1,NBND
           CALL CALTHTE(PBND(1,1,LBND),TBND(1,1,LBND),
     X                  QBND(1,1,LBND),EGRID1)
           DO J=JSTA,JEND
           DO I=1,IM
             IF (EGRID1(I,J).GT.EGRID2(I,J)) THEN
               EGRID2(I,J) = EGRID1(I,J)
               LB2(I,J)  = LVLBND(I,J,LBND)
               P1D(I,J)  = PBND(I,J,LBND)
               T1D(I,J)  = TBND(I,J,LBND)
               Q1D(I,J)  = QBND(I,J,LBND)
             ENDIF
           ENDDO
           ENDDO
 80        CONTINUE
C
           CALL CALCAPE(ITYPE,P1D,T1D,Q1D,LB2,EGRID1,EGRID2)
C
           IF (IGET(032).GT.0) THEN
             CALL E2OUT(032,000,EGRID1,EGRID2,
     X                  GRID1,GRID2,IMOUT,JMOUT)
             CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
             ID(1:25) = 0
             ID(09)   = 116
             ID(10)   = PETABND(NBND)+15.
             ID(11)   = PETABND(1)-15.
             CALL OUTPUT(IOUTYP,IGET(032),LVLS(1,IGET(032)),
     X                   GRID1,IMOUT,JMOUT)
           ENDIF
C
           IF (IGET(107).GT.0) THEN
             CALL E2OUT(107,000,EGRID2,EGRID1,
     X                  GRID1,GRID2,IMOUT,JMOUT)
             DO J=JSTA,JEND
             DO I=1,IMOUT
               GRID1(I,J) = -1.*GRID1(I,J)
             ENDDO
             ENDDO
C
             CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
C
             DO J=JSTA,JEND
             DO I=1,IMOUT
               GRID1(I,J) = -1.*GRID1(I,J)
             ENDDO
             ENDDO
C
             ID(1:25) = 0
             ID(09)   = 116
             ID(10)   = PETABND(NBND)+15.
             ID(11)   = PETABND(1)-15.
             CALL OUTPUT(IOUTYP,IGET(107),LVLS(1,IGET(107)),
     X                   GRID1,IMOUT,JMOUT)
           ENDIF
         ENDIF
C
C        BOUNDARY LAYER LIFTING CONDENSATION PRESSURE AND HEIGHT.
C        EGRID1 IS LCL PRESSURE.  EGRID2 IS LCL HEIGHT.
C
         IF ( (IGET(109).GT.0).OR.(IGET(110).GT.0) ) THEN
            CALL CALLCL(PBND(1,1,1),TBND(1,1,1),
     X           QBND(1,1,1),EGRID1,EGRID2)
            IF (IGET(109).GT.0) THEN
               CALL E2OUT(109,000,EGRID2,EGRID1,
     X              GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               CALL OUTPUT(IOUTYP,IGET(109),ILVL,
     X              GRID1,IMOUT,JMOUT)
            ENDIF
            IF (IGET(110).GT.0) THEN
               CALL E2OUT(110,000,EGRID1,EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               CALL OUTPUT(IOUTYP,IGET(110),ILVL,
     X              GRID1,IMOUT,JMOUT)
            ENDIF
         ENDIF
C     
C        NGM BOUNDARY LAYER FIELDS.
C     
         IF ( (IGET(091).GT.0).OR.(IGET(092).GT.0).OR.
     X        (IGET(093).GT.0).OR.(IGET(094).GT.0).OR.
     X        (IGET(095).GT.0).OR.(IGET(095).GT.0).OR.
     X        (IGET(096).GT.0).OR.(IGET(097).GT.0).OR.
     X        (IGET(098).GT.0) ) THEN
C
C  COMPUTE SIGMA 0.89671 AND 0.78483 TEMPERATURES
C    INTERPOLATE LINEAR IN LOG P
            IF (IGET(097).GT.0.OR.IGET(098).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 P78483(I,J)=ALOG((PD(I,J)+PT)*0.78483)
                 P89671(I,J)=ALOG((PD(I,J)+PT)*0.89671)
               ENDDO
               ENDDO
!$omp  parallel do
!$omp& private(fac1,fac2,pkl1,pku1,t78483,t89671)
               DO L=2,LM
                DO J=JSTA,JEND
                DO I=1,IM
                  PKL1=0.5*(ALPINT(I,J,L)+ALPINT(I,J,L+1))
                  PKU1=0.5*(ALPINT(I,J,L)+ALPINT(I,J,L-1))
                  IF(P78483(I,J).LT.PKL1.AND.P78483(I,J).GT.PKU1)THEN
                    FAC1=(PKL1-P78483(I,J))/(PKL1-PKU1)
                    FAC2=(P78483(I,J)-PKU1)/(PKL1-PKU1)
                    T78483(I,J)=T(I,J,L)*FAC2+T(I,J,L-1)*FAC1
                  ENDIF
                  IF(P89671(I,J).LT.PKL1.AND.P89671(I,J).GT.PKU1)THEN
                    FAC1=(PKL1-P89671(I,J))/(PKL1-PKU1)
                    FAC2=(P89671(I,J)-PKU1)/(PKL1-PKU1)
                    T89671(I,J)=T(I,J,L)*FAC2+T(I,J,L-1)*FAC1
                  ENDIF
                ENDDO
                ENDDO
               ENDDO
C     
C           SIGMA 0.89671 TEMPERATURE
             IF (IGET(097).GT.0) THEN
               ID(1:25) = 0
               ISVALUE = 8967
               ID(11) = ISVALUE
               CALL E2OUT(097,000,T89671,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(097),LVLS(1,IGET(097)),
     X              GRID1,IMOUT,JMOUT)
             ENDIF
C     
C           SIGMA 0.78483 TEMPERATURE
             IF (IGET(098).GT.0) THEN
               ID(1:25) = 0
               ISVALUE = 7848
               ID(11) = ISVALUE
               CALL E2OUT(098,000,T78483,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(098),LVLS(1,IGET(098)),
     X              GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           NGM SIGMA LAYER 0.98230 FIELDS.  THESE FIELDS ARE 
C           THE FIRST ETA LAYER BOUNDARY LAYER FIELDS.  AN
C           EXCEPTION IS MADE FOR RELATIVE HUMIDITY ON THE LFM
C           FORECAST GRID (026).  ON THIS OUTPUT GRID THE SIGMA
C           0.98230 RELATIVE HUMIDITY IS THE MEAN RELATIVE HUMIDITY
C           COMPUTED BY AVERAGING ETA BOUNDARY LAYER 1, 2, AND 3
C           RELATIVE HUMIDITIES.  THIS AD HOC APPROACH WAS TAKEN 
C           TO PRODUCE A MORE "LFM-LIKE" BOUNDARY LAYER RELATIVE
C           HUMIDITY FIELD FOR THE AVIATION BRANCH OF MOD.
C
C           THIS IS NOT A SATISFACTORY SOLUTION.  THE SIGMA 
C           0.98230 RELATIVE HUMIDITY SHOULD BE CONSISTENT
C           WITH THE OTHER SIGMA 0.98230 FIELDS.  
C     
C     
            IF ( (IGET(091).GT.0).OR.(IGET(092).GT.0).OR.
     X           (IGET(093).GT.0).OR.(IGET(094).GT.0).OR.
     X           (IGET(095).GT.0).OR.(IGET(095).GT.0).OR.
     X           (IGET(096).GT.0) ) THEN
C     
               ID(1:25) = 0
               ISVALUE = 9823
               ID(11) = ISVALUE
C     
C              PRESSURE.
               IF (IGET(091).GT.0) THEN
                  CALL E2OUT(091,000,PBND(1,1,1),EGRID2,
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  CALL OUTPUT(IOUTYP,IGET(091),LVLS(1,IGET(091)),
     X                 GRID1,IMOUT,JMOUT)
               ENDIF
C     
C              TEMPERATURE.
               IF (IGET(092).GT.0) THEN
                  CALL E2OUT(092,000,TBND(1,1,1),EGRID2,
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  CALL OUTPUT(IOUTYP,IGET(092),LVLS(1,IGET(092)),
     X                 GRID1,IMOUT,JMOUT)
               ENDIF
C     
C              SPECIFIC HUMIDITY.
               IF (IGET(093).GT.0) THEN
                  CALL E2OUT(093,000,QBND(1,1,1),EGRID2,
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  CALL BOUND(GRID1,H1M12,H99999,IMOUT,JMOUT)
                  CALL OUTPUT(IOUTYP,IGET(093),LVLS(1,IGET(093)),
     X                 GRID1,IMOUT,JMOUT)
               ENDIF
C     
C              RELATIVE HUMIDITY.
               IF (IGET(094).GT.0) THEN
                  IF (KGTYPE.EQ.026) THEN
                     DO J=JSTA,JEND
                     DO I=1,IM
                       RH1D(I,J)=(RHBND(I,J,1)+RHBND(I,J,2)+
     &                            RHBND(I,J,3))*0.33333333
                     ENDDO
                     ENDDO
                  ELSE
                     DO J=JSTA,JEND
                     DO I=1,IM
                       RH1D(I,J)=RHBND(I,J,1)
                     ENDDO
                     ENDDO
                  ENDIF
                  CALL E2OUT(094,000,RH1D,EGRID2,GRID1,GRID2,
     X                 IMOUT,JMOUT)
                  CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
                  CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
                  CALL OUTPUT(IOUTYP,IGET(094),LVLS(1,IGET(094)),
     X                 GRID1,IMOUT,JMOUT)
               ENDIF
C     
C              U AND/OR V WIND.
               IF ((IGET(095).GT.0).OR.(IGET(096).GT.0)) THEN
                  CALL E2OUT(095,096,UBND(1,1,1),VBND(1,1,1),
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  IF (IGET(095).GT.0) CALL OUTPUT(IOUTYP,IGET(095),
     X                 LVLS(1,IGET(095)),GRID1,IMOUT,JMOUT)
                  IF (IGET(096).GT.0) CALL OUTPUT(IOUTYP,IGET(096),
     X                 LVLS(1,IGET(096)),GRID2,IMOUT,JMOUT)
               ENDIF
            ENDIF
         ENDIF
C     
C     ENDIF FOR BOUNDARY LAYER BLOCK.
C
      ENDIF
C     
C
C
C     ***BLOCK 6:  MISCELLANEOUS LAYER MEAN LFM AND NGM FIELDS.
C     
      IF ( (IGET(066).GT.0).OR.(IGET(081).GT.0).OR.
     X     (IGET(082).GT.0).OR.(IGET(104).GT.0).OR.
     X     (IGET(099).GT.0).OR.(IGET(100).GT.0).OR.
     X     (IGET(101).GT.0).OR.(IGET(102).GT.0).OR.
     X     (IGET(103).GT.0) ) THEN
C     
C        LFM "MEAN" RELATIVE HUMIDITIES AND PRECIPITABLE WATER.
C     
         IF ( (IGET(066).GT.0).OR.(IGET(081).GT.0).OR.
     X        (IGET(082).GT.0).OR.(IGET(104).GT.0) ) THEN
            CALL LFMFLD(RH3310,RH6610,RH3366,PW3310)
            ID(1:25) = 0
C     
C           SIGMA 0.33-1.00 MEAN RELATIVE HUMIIDITY.
            IF (IGET(066).GT.0) THEN
               CALL E2OUT(066,000,RH3310,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10) = 33
               ID(11) = 100
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(066),LVLS(1,IGET(066)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C     
C           SIGMA 0.66-1.00 MEAN RELATIVE HUMIIDITY.
            IF (IGET(081).GT.0) THEN
               CALL E2OUT(081,000,RH6610,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10) = 67
               ID(11) = 100
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(081),LVLS(1,IGET(081)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C     
C           SIGMA 0.33-0.66 MEAN RELATIVE HUMIIDITY.
            IF (IGET(082).GT.0) THEN
               CALL E2OUT(082,000,RH3366,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10) = 33
               ID(11) = 67
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(082),LVLS(1,IGET(082)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C     
C           SIGMA 0.33-1.00 PRECIPITABLE WATER.
            IF (IGET(104).GT.0) THEN
               CALL E2OUT(104,000,PW3310,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10) = 33
               ID(11) = 100
               CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(104),LVLS(1,IGET(104)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
         ENDIF
C     
C        VARIOUS LAYER MEAN NGM SIGMA FIELDS.
C     
         IF ( (IGET(099).GT.0).OR.(IGET(100).GT.0).OR.
     X        (IGET(101).GT.0).OR.(IGET(102).GT.0).OR.
     X        (IGET(103).GT.0) ) THEN
            CALL NGMFLD2(RH4710,RH4796,RH1847,RH8498,QM8510)
C     
C           SIGMA 0.47191-1.00000 RELATIVE HUMIDITY.
            IF (IGET(099).GT.0) THEN
               CALL E2OUT(099,000,RH4710,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10)   = 47
               ID(11)   = 100
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(099),LVLS(1,IGET(099)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C     
C           SIGMA 0.47191-0.96470 RELATIVE HUMIDITY.
            IF (IGET(100).GT.0) THEN
               CALL E2OUT(100,000,RH4796,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10)   = 47
               ID(11)   = 96
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(100),LVLS(1,IGET(100)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C     
C           SIGMA 0.18019-0.47191 RELATIVE HUMIDITY.
            IF (IGET(101).GT.0) THEN
               CALL E2OUT(101,000,RH1847,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10)   = 18
               ID(11)   = 47
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(101),LVLS(1,IGET(101)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C     
C           SIGMA 0.84368-0.98230 RELATIVE HUMIDITY.
            IF (IGET(102).GT.0) THEN
               CALL E2OUT(102,000,RH8498,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
               ID(1:25) = 0
               ID(10)   = 84
               ID(11)   = 98
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(102),LVLS(1,IGET(102)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C     
C           SIGMA 0.85000-1.00000 MOISTURE CONVERGENCE.
            IF (IGET(103).GT.0) THEN
               CALL E2OUT(103,000,QM8510,EGRID2,GRID1,GRID2,
     X              IMOUT,JMOUT)
C           CONVERT TO DIVERGENCE FOR GRIB
               DO J = JSTA,JEND
               DO I = 1,IMOUT
                GRID1(I,J) = -1.0 * GRID1(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10)   = 85
               ID(11)   = 100
               CALL OUTPUT(IOUTYP,IGET(103),LVLS(1,IGET(103)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
         ENDIF
      ENDIF
C     
C     END OF ROUTINE.
C     
      RETURN
      END
