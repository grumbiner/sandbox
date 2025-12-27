      SUBROUTINE SURFCE2(IMOUT,JMOUT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    SURFCE2     POST SURFACE BASED FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-21       
C     
C ABSTRACT:
C     THIS ROUTINE POSTS SURFACE BASED FIELDS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-21  RUSS TREADON
C   94-08-04  MICHAEL BALDWIN - ADDED OUTPUT OF SFC FLUXES OF
C                               SENS AND LATENT HEAT AND THETA AT Z0
C   94-11-04  MICHAEL BALDWIN - ADDED INSTANTANEOUS PRECIP TYPE
C   96-03-19  MICHAEL BALDWIN - CHANGE SOIL PARAMETERS
C   96-09-25  MICHAEL BALDWIN - ADDED SNOW RATIO FROM EXPLICIT SCHEME
C   96-10-17  MICHAEL BALDWIN - CHANGED SFCEVP,POTEVP TO ACCUM.  TOOK
C                               OUT -PTRACE FOR ACSNOW,SSROFF,BGROFF.
C   97-04-23  MICHAEL BALDWIN - TOOK OUT -PTRACE FOR ALL PRECIP FIELDS
C   98-06-12  T BLACK         - CONVERSION FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN - REMOVED LABL84
C   98-08-18  MIKE BALDWIN - COMPUTE RH OVER ICE
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   03-01-15  H CHUANG     - ADDED VEG AND SOIL TYPES
C   03-01-17  CHUANG AND EK- ADDED CANOPY CONDUCTANCE, SNOW COVER AND DEPTH,
C                            AND OTHER LAND SURFACE VARIABLES
C   03-02-20  DUSAN JOVIC - ADDED 30m T,Q,WIND AND INTEGRATED MOISTURE FLUXES
C     
C USAGE:    CALL SURFCE2(IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       E2OUT    - INTERPOLATION/SMOOTHING ROUTINE.
C       OUTPUT   - DRIVER FOR OUTPUT ROUTINES.
C       BOUND    - ENFORCE LOWER AND UPPER LIMITS ON ARRAY ELEMENTS.
C       SCLFLD   - SCALE ARRAY ELEMENTS BY CONSTANT.
C       SHELTR2  - COMPUTE 2M TEMPERATURE AND SPECIFIC HUMIDITY.
C       ANEMLV6  - COMPUTE 10M U AND V WINDS.
C       DEWPOINT - COMPUTE DEWPOINT TEMPERATURE.
C       CALDRG   - COMPUTE SURFACE LAYER DRAG COEFFICENT
C       CALTAU   - COMPUTE SURFACE LAYER U AND V WIND STRESSES.
C
C     LIBRARY:
C       COMMON   - CTLBLK
C                  RQSTFLD
C                  EXTRA
C                  VRBLS
C                  MAPOT
C                  MASKS
C                  PVRBLS
C                  CLDWTR
C                  LOOPS
C                  PHYS2
C                  SRFDSP
C                  CNVCLD
C                  LLGRDS
C                  SOIL
C                  ACMSFC
C                  ACMPRE
C                  IOUNIT
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C     
C     INCLUDE GRID DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "params"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
C     
C     IN NGM SUBROUTINE OUTPUT WE FIND THE FOLLOWING COMMENT.
C     "IF THE FOLLOWING THRESHOLD VALUES ARE CHANGED, CONTACT
C     TDL/SYNOPTIC-SCALE TECHNIQUES BRANCH (PAUL DALLAVALLE
C     AND JOHN JENSENIUS).  THEY MAY BE USING IT IN ONE OF 
C     THEIR PACKING CODES."  THE THRESHOLD VALUE IS 0.01 INCH
C     OR 2.54E-4 METER.  PRECIPITATION VALUES LESS THAN THIS
C     THRESHOLD ARE SET TO MINUS ONE TIMES THIS THRESHOLD.
      PARAMETER (PTRACE = 0.000254E0)
C     
C     SET CELCIUS TO KELVIN AND SECOND TO HOUR CONVERSION.
      PARAMETER (C2K    = 273.15)
      PARAMETER (SEC2HR = 1./3600.)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STDRD
      INTEGER IWX1(IM,JM),NROOTS(IM,JM)
      REAL PSFC(IM,JM),TSFC(IM,JM),QSFC(IM,JM),RHSFC(IM,JM)
      REAL ZSFC(IM,JM),THSFC(IM,JM),DWPSFC(IM,JM),EVP(IM,JM)
      REAL ANCPRC(IM,JM),P1D(IM,JM),T1D(IM,JM),Q1D(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM),UA(IM,JM),VA(IM,JM)
      REAL GC(IM,JM)
      REAL GRID1(IMOUT,JMOUT),GRID2(IMOUT,JMOUT),IW(IM,JM),IWM1
      REAL SLEET(IM,JM),RAIN(IM,JM),FREEZR(IM,JM),SNOW(IM,JM)
      REAL ECAN(IM,JM),EDIR(IM,JM),ETRANS(IM,JM),ESNOW(IM,JM)
     &,SMCDRY(IM,JM),SMCMAX(IM,JM)
      REAL RSMIN(IM,JM),SMCWLT(IM,JM),SMCREF(IM,JM)
     & ,RCS(IM,JM),RCQ(IM,JM),RCT(IM,JM),RCSOIL(IM,JM)
      REAL P10(IM,JM),P30(IM,JM),T10(IM,JM),T30(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "RQSTFLD.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "SRFDSP.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "LLGRDS.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "QFLX.comm"
      INCLUDE "PPTASM.comm"
C     
C****************************************************************************
C
C     START SURFCE.
C
C     COMPUTE IW AT SFC FOR SFC AND 2M RH
C
      IF ( (IGET(076).GT.0).OR.(IGET(114).GT.0) ) THEN
        CLIMIT =1.0E-20
        IW=0.
C
        DO L=2,LM 
        DO J=JSTA,JEND
        DO I=1,IM
         IF (L.LE.LMH(I,J)) THEN
           IWM1=IW(I,J)
           IF(CWM(I,J,L).GT.CLIMIT) THEN
             IF(T(I,J,L).LT.258.15)THEN
               IW(I,J)=1.
             ELSEIF(T(I,J,L).GE.273.15)THEN
               IW(I,J)=0.
             ELSE
               IF(IWM1.EQ.1.0)IW(I,J)=1.
             ENDIF
           ELSE
             IW(I,J)=0.
           ENDIF
         ENDIF
        ENDDO
        ENDDO
        ENDDO
C
      ENDIF
C     
C***  BLOCK 1.  SURFACE BASED FIELDS.
C
C     IF ANY OF THE FOLLOWING "SURFACE" FIELDS ARE REQUESTED,
C     WE NEED TO COMPUTE THE FIELDS FIRST.
C     
      IF ( (IGET(024).GT.0).OR.(IGET(025).GT.0).OR.
     X     (IGET(026).GT.0).OR.(IGET(027).GT.0).OR.
     X     (IGET(028).GT.0).OR.(IGET(029).GT.0).OR.
     X     (IGET(154).GT.0).OR.
     X     (IGET(034).GT.0).OR.(IGET(076).GT.0) ) THEN
C     
         DO 40 J=JSTA,JEND
         DO 40 I=1,IM
C
C           PRESSURE AND TEMPERATURE AT 10 AND 30 M.
            LMHK=LMH(I,J)
            P10(I,J)=(PD(I,J)+PT)*EXP(-10.0*G/(287.04*T(I,J,LMHK)))
            P30(I,J)=(PD(I,J)+PT)*EXP(-30.0*G/(287.04*T(I,J,LMHK)))
            T10(I,J)=TH10(I,J)*(P10(I,J)/P1000)**CAPA
            T30(I,J)=TH30(I,J)*(P30(I,J)/P1000)**CAPA
C
C           SCALE ARRAY FIS BY GI TO GET SURFACE HEIGHT.
            ZSFC(I,J)=FIS(I,J)*GI
C
C           SURFACE PRESSURE.
            PSFC(I,J)=PD(I,J)+PT
C     
C           SURFACE (SKIN) POTENTIAL TEMPERATURE AND TEMPERATURE.
            THSFC(I,J)=THS(I,J)
            TSFC(I,J) =THSFC(I,J)*(PSFC(I,J)/P1000)**CAPA 
C     
C           SURFACE SPECIFIC HUMIDITY, RELATIVE HUMIDITY,
C           AND DEWPOINT.  ADJUST SPECIFIC HUMIDITY IF
C           RELATIVE HUMIDITY EXCEEDS 0.1 OR 1.0.
C
            QSFC(I,J)=QS(I,J)
            QSFC(I,J)=AMAX1(H1M12,QSFC(I,J))
            TSFCK    =TSFC(I,J)
C     
            TMT0=TSFCK-273.16
            TMT15=AMIN1(TMT0,-15.)
            AI=0.008855
            BI=1.
            IF(TMT0.LT.-20.)THEN
              AI=0.007225
              BI=0.9674
            ENDIF
            QW=PQ0/PSFC(I,J)
     1          *EXP(A2*(TSFCK-A3)/(TSFCK-A4))
            QI=QW*(BI+AI*AMIN1(TMT0,0.))
            QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
            IF(TMT0.LT.-15.)THEN
                QSAT=QI
            ELSEIF(TMT0.GE.0.)THEN
                QSAT=QINT
            ELSE
              IF(IW(I,J).GT.0.0) THEN
                QSAT=QI
              ELSE
                QSAT=QINT
              ENDIF
            ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
            QSAT=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C
            RHSFC(I,J)=QSFC(I,J)/QSAT

            IF (RHSFC(I,J).GT.H1 ) RHSFC(I,J) = H1
            IF (RHSFC(I,J).LT.D00) RHSFC(I,J) = D01
            QSFC(I,J)  = RHSFC(I,J)*QSAT
            EVP(I,J)   = PSFC(I,J)*QSFC(I,J)/(EPS+ONEPS*QSFC(I,J))
            EVP(I,J)   = EVP(I,J)*D001
C     
C           ACCUMULATED NON-CONVECTIVE PRECIP.
            IF(IGET(034).GT.0)THEN
              IF(LVLS(1,IGET(034)).GT.0)THEN
                 ANCPRC(I,J)=ACPREC(I,J)-CUPREC(I,J)
              ENDIF
            ENDIF

 40      CONTINUE
C     
C        INTERPOLATE/OUTPUT REQUESTED SURFACE FIELDS.
C     
C        SURFACE PRESSURE.
         IF (IGET(024).GT.0) THEN
            CALL E2OUT(024,000,PSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(024),LVLS(1,IGET(024)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C        SURFACE PRESSURE NN.
         IF (IGET(247).GT.0) THEN
            CALL E2OUT(247,000,PSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(247),LVLS(1,IGET(247)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SURFACE HEIGHT.
         IF (IGET(025).GT.0) THEN
            CALL E2OUT(025,000,ZSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(025),LVLS(1,IGET(025)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C        SURFACE HEIGHT NN.
         IF (IGET(245).GT.0) THEN
            CALL E2OUT(245,000,ZSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(245),LVLS(1,IGET(245)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SURFACE (SKIN) TEMPERATURE.
         IF (IGET(026).GT.0) THEN
            CALL E2OUT(026,000,TSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(026),LVLS(1,IGET(026)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SURFACE (SKIN) POTENTIAL TEMPERATURE.
         IF (IGET(027).GT.0) THEN
            CALL E2OUT(027,000,THSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(027),LVLS(1,IGET(027)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SURFACE SPECIFIC HUMIDITY.
         IF (IGET(028).GT.0) THEN
            CALL E2OUT(028,000,QSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1M12,H99999,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(028),LVLS(1,IGET(028)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SURFACE DEWPOINT TEMPERATURE.
         IF (IGET(029).GT.0) THEN
            CALL DEWPOINT(EVP,DWPSFC,IM,JM)
            CALL E2OUT(029,000,DWPSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(029),LVLS(1,IGET(029)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SURFACE RELATIVE HUMIDITY.
         IF (IGET(076).GT.0) THEN
            CALL E2OUT(076,000,RHSFC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(076),LVLS(1,IGET(076)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
      ENDIF
C
C     ADDITIONAL SURFACE-SOIL LEVEL FIELDS.
C

      DO L=1,NSOIL
C     SOIL TEMPERATURE.
      IF (IGET(116).GT.0) THEN
        IF (LVLS(L,IGET(116)).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=STC(I,J,L)
         ENDDO
         ENDDO
         CALL E2OUT(116,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
          DTOP=0.
          DO LS=1,L-1
           DTOP=DTOP+SLDPTH(LS)
          ENDDO
          DBOT=DTOP+SLDPTH(L)
            ID(10) = NINT(DTOP*100.)
            ID(11) = NINT(DBOT*100.)
         CALL OUTPUT(IOUTYP,IGET(116),L,GRID1,IMOUT,JMOUT)
        ENDIF
      ENDIF
C
C     SOIL MOISTURE.
      IF (IGET(117).GT.0) THEN
        IF (LVLS(L,IGET(117)).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=SMC(I,J,L)
         ENDDO
         ENDDO
         CALL E2OUT(117,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
          DTOP=0.
          DO LS=1,L-1
           DTOP=DTOP+SLDPTH(LS)
          ENDDO
          DBOT=DTOP+SLDPTH(L)
            ID(10) = NINT(DTOP*100.)
            ID(11) = NINT(DBOT*100.)
         CALL OUTPUT(IOUTYP,IGET(117),L,GRID1,IMOUT,JMOUT)
        ENDIF
      ENDIF
! ADD LIQUID SOIL MOISTURE
      IF (IGET(225).GT.0) THEN
        IF (LVLS(L,IGET(225)).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=SH2O(I,J,L)
         ENDDO
         ENDDO
         CALL E2OUT(225,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
          DTOP=0.
          DO LS=1,L-1
           DTOP=DTOP+SLDPTH(LS)
          ENDDO
          DBOT=DTOP+SLDPTH(L)
            ID(10) = NINT(DTOP*100.)
            ID(11) = NINT(DBOT*100.)
            ID(02) = 130
         CALL OUTPUT(IOUTYP,IGET(225),L,GRID1,IMOUT,JMOUT)
        ENDIF
      ENDIF
! END OF NSOIL LOOP
      ENDDO
C
C     BOTTOM SOIL TEMPERATURE.
      IF (IGET(115).GT.0) THEN
         CALL E2OUT(115,000,SOILTB,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ISVALUE     = 800
         ID(11) = ISVALUE
         CALL OUTPUT(IOUTYP,IGET(115),LVLS(1,IGET(115)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     SOIL MOISTURE AVAILABILITY
      IF (IGET(171).GT.0) THEN
         CALL E2OUT(171,000,SMSTAV,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(10) = 0
         ID(11) = 100
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(171),LVLS(1,IGET(171)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     TOTAL SOIL MOISTURE
      IF (IGET(036).GT.0) THEN
         CALL E2OUT(036,000,SMSTOT,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(10) = 0
         ID(11) = 200
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(036),LVLS(1,IGET(036)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     PLANT CANOPY SURFACE WATER.
      IF ( IGET(118).GT.0 ) THEN
         CALL E2OUT(118,000,CMC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(118),LVLS(1,IGET(118)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     SNOW WATER EQUIVALENT.
      IF ( IGET(119).GT.0 ) THEN
         CALL E2OUT(119,000,SNO,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(119),LVLS(1,IGET(119)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     PERCENT SNOW COVER.
      IF ( IGET(120).GT.0 ) THEN
         DO J=JSTA,JEND
          DO I=1,IM
           SNEQV=SNO(I,J)
           IVEG=IVGTYP(I,J)
           CALL SNFRAC (SNEQV,IVEG,SNCOVR)
           PCTSNO(I,J)=SNCOVR
          END DO
         END DO
         CALL E2OUT(120,000,PCTSNO,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL BOUND(GRID1,D00,H1,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(120),LVLS(1,IGET(120)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
! ADD SNOW DEPTH
      IF ( IGET(224).GT.0 ) THEN
         CALL E2OUT(224,000,SI,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(224),LVLS(1,IGET(224)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
! ADD POTENTIAL EVAPORATION
C      IF ( IGET(242).GT.0 ) THEN
C         CALL E2OUT(242,000,POTEVP,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
C         ID(1:25) = 0
C         CALL OUTPUT(IOUTYP,IGET(242),LVLS(1,IGET(242)),
C     X        GRID1,IMOUT,JMOUT)
C      END IF
! ADD EC,EDIR,ETRANS,ESNOW,SMCDRY,SMCMAX
      IF ( IGET(228).GT.0 .OR. IGET(229).GT.0
     + .OR.IGET(230).GT.0 .OR. IGET(231).GT.0
     + .OR.IGET(232).GT.0 .OR. IGET(233).GT.0) THEN
        DO J=JSTA,JEND
         DO I=1,IM
C ----------------------------------------------------------------------
!          IF(QWBS(I,J).gt.0.001)print*,'NONZERO QWBS',i,j,QWBS(I,J)
!          IF(abs(SM(I,J)-0.).lt.1.0E-5)THEN
          IF( (abs(SM(I,J)-0.)   .lt. 1.0E-5) .AND.
     &        (abs(SICE(I,J)-0.) .lt. 1.0E-5) ) THEN
           CALL ETCALC(QWBS(I,J),POTEVP(I,J),SNO(I,J),VEGFRC(I,J)
     &  ,  ISLTYP(I,J),SH2O(I,J,1:1),CMC(I,J)
     &  ,  ECAN(I,J),EDIR(I,J),ETRANS(I,J),ESNOW(I,J),SMCDRY(I,J)
     &  ,  SMCMAX(I,J) )
          ELSE
           ECAN(I,J)=0.
           EDIR(I,J)=0.
           ETRANS(I,J)=0.
           ESNOW(I,J)=0.
           SMCDRY(I,J)=0.
           SMCMAX(I,J)=0.
          END IF
         END DO
        END DO
        IF ( IGET(228).GT.0 )THEN
         CALL E2OUT(228,000,ECAN,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(228),LVLS(1,IGET(228)),
     X        GRID1,IMOUT,JMOUT)
        END IF
!
        IF ( IGET(229).GT.0 )THEN
         CALL E2OUT(229,000,EDIR,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(229),LVLS(1,IGET(229)),
     X        GRID1,IMOUT,JMOUT)
        END IF
!
        IF ( IGET(230).GT.0 )THEN
         CALL E2OUT(230,000,ETRANS,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(230),LVLS(1,IGET(230)),
     X        GRID1,IMOUT,JMOUT)
        END IF
!
        IF ( IGET(231).GT.0 )THEN
         CALL E2OUT(231,000,ESNOW,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(02)= 130
         CALL OUTPUT(IOUTYP,IGET(231),LVLS(1,IGET(231)),
     X        GRID1,IMOUT,JMOUT)
        END IF
!
        IF ( IGET(232).GT.0 )THEN
         CALL E2OUT(232,000,SMCDRY,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(02)= 130
         CALL OUTPUT(IOUTYP,IGET(232),LVLS(1,IGET(232)),
     X        GRID1,IMOUT,JMOUT)
        END IF
!
        IF ( IGET(233).GT.0 )THEN
         CALL E2OUT(233,000,SMCMAX,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(02)= 130
         CALL OUTPUT(IOUTYP,IGET(233),LVLS(1,IGET(233)),
     X        GRID1,IMOUT,JMOUT)
        END IF
!
      ENDIF
C
C     
C
C***  BLOCK 2.  SHELTER (2M) LEVEL FIELDS.
C     
C     COMPUTE/POST SHELTER LEVEL FIELDS.
C     
      IF ( (IGET(106).GT.0).OR.(IGET(112).GT.0).OR.
     X     (IGET(113).GT.0).OR.(IGET(114).GT.0).OR.
     X     (IGET(138).GT.0) ) THEN
C
C        CALL SHELTR2(PSHLTR,QSHLTR,TSHLTR)
C
C        SHELTER LEVEL TEMPERATURE
         IF (IGET(106).GT.0) THEN
            CALL E2OUT(106,000,TSHLTR,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL OUTPUT(IOUTYP,IGET(106),LVLS(1,IGET(106)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C        SHELTER LEVEL SPECIFIC HUMIDITY.
         IF (IGET(112).GT.0) THEN       
            CALL E2OUT(112,000,QSHLTR,EGRID1,GRID1,GRID2,IMOUT,JMOUT)
            CALL BOUND (GRID1,H1M12,H99999,IMOUT,JMOUT)
            ID(1:25) = 0
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL OUTPUT(IOUTYP,IGET(112),LVLS(1,IGET(112)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SHELTER LEVEL DEWPOINT.
         IF (IGET(113).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
              EVP(I,J)=PSHLTR(I,J)*QSHLTR(I,J)/(EPS+ONEPS*QSHLTR(I,J))
	      EVP(I,J)=EVP(I,J)*D001
            ENDDO
            ENDDO
            CALL DEWPOINT(EVP,EGRID1,IM,JM)
            CALL E2OUT(113,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL OUTPUT(IOUTYP,IGET(113),LVLS(1,IGET(113)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SHELTER LEVEL RELATIVE HUMIDITY.
         IF (IGET(114).GT.0) THEN
            CALL CALRH2(PSHLTR,TSHLTR,QSHLTR,IW,EGRID1,IM,JM)
            CALL E2OUT(114,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
            ID(1:25) = 0
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL OUTPUT(IOUTYP,IGET(114),LVLS(1,IGET(114)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SHELTER LEVEL PRESSURE.
         IF (IGET(138).GT.0) THEN
            CALL E2OUT(138,000,PSHLTR,EGRID1,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL OUTPUT(IOUTYP,IGET(138),LVLS(1,IGET(138)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
      ENDIF
C
C
C     BLOCK 3.  ANEMOMETER LEVEL (10M) WINDS, THETA, AND Q.
C
      IF ( (IGET(064).GT.0).OR.(IGET(065).GT.0) ) THEN
C        CALL ANEMLV6(UA,VA)
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
C
C        ANEMOMETER LEVEL U WIND AND/OR V WIND.
         IF ((IGET(064).GT.0).OR.(IGET(065).GT.0)) THEN
            CALL E2OUT(064,065,U10,V10,GRID1,GRID2,IMOUT,JMOUT)
            IF (IGET(064).GT.0) CALL OUTPUT(IOUTYP,IGET(064),
     X           LVLS(1,IGET(064)),GRID1,IMOUT,JMOUT)
            IF (IGET(065).GT.0) CALL OUTPUT(IOUTYP,IGET(065),
     X           LVLS(1,IGET(065)),GRID2,IMOUT,JMOUT)
         ENDIF
      ENDIF
C
C        ANEMOMETER LEVEL (10 M) POTENTIAL TEMPERATURE.
C
      IF (IGET(158).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(158,000,TH10,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(158),
     X        LVLS(1,IGET(158)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C        ANEMOMETER LEVEL (10 M) TEMPERATURE.
C
      IF (IGET(185).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(185,000,T10,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(185),
     X        LVLS(1,IGET(185)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C        ANEMOMETER LEVEL (10 M) PRESSURE.
C
      IF (IGET(186).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(186,000,P10,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(186),
     X        LVLS(1,IGET(186)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C        ANEMOMETER LEVEL (10 M) SPECIFIC HUMIDITY.
C
      IF (IGET(159).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(159,000,Q10,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(159),
     X        LVLS(1,IGET(159)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C
C     BLOCK 3.1 30M WINDS, THETA, AND Q.
C
      IF ( (IGET(193).GT.0).OR.(IGET(194).GT.0) ) THEN
C        CALL ANEMLV6(UA,VA)
         ID(1:25) = 0
         ISVALUE = 30
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
C
C        ANEMOMETER LEVEL U WIND AND/OR V WIND.
         IF ((IGET(193).GT.0).OR.(IGET(194).GT.0)) THEN
            CALL E2OUT(193,194,U30,V30,GRID1,GRID2,IMOUT,JMOUT)
            IF (IGET(193).GT.0) CALL OUTPUT(IOUTYP,IGET(193),
     X           LVLS(1,IGET(193)),GRID1,IMOUT,JMOUT)
            IF (IGET(194).GT.0) CALL OUTPUT(IOUTYP,IGET(194),
     X           LVLS(1,IGET(194)),GRID2,IMOUT,JMOUT)
         ENDIF
      ENDIF
C
C        30 M POTENTIAL TEMPERATURE.
C
      IF (IGET(191).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 30
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(191,000,TH30,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(191),
     X        LVLS(1,IGET(191)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C        30 M TEMPERATURE.
C
      IF (IGET(189).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 30
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(189,000,T30,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(189),
     X        LVLS(1,IGET(189)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C        30 M PRESSURE.
C
      IF (IGET(190).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 30
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(190,000,P30,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(190),
     X        LVLS(1,IGET(190)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C        30 M SPECIFIC HUMIDITY.
C
      IF (IGET(192).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 30
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         CALL E2OUT(192,000,Q30,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(192),
     X        LVLS(1,IGET(192)),GRID1,IMOUT,JMOUT)
       ENDIF
C
C
C
C***  BLOCK 4.  PRECIPITATION RELATED FIELDS.
C     
C     SNOW FRACTION FROM EXPLICIT CLOUD SCHEME.  LABELLED AS
C      'PROB OF FROZEN PRECIP' IN GRIB, 
C      DIDN'T KNOW WHAT ELSE TO CALL IT
      IF (IGET(172).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=SR(I,J)*100.
         ENDDO
         ENDDO
         CALL E2OUT(172,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(172),LVLS(1,IGET(172)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     INSTANTANEOUS PRECIPITATION RATE.
      IF (IGET(167).GT.0) THEN
         RDTPHS=1./DTQ2
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=PREC(I,J)*RDTPHS
         ENDDO
         ENDDO
         CALL E2OUT(167,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(167),LVLS(1,IGET(167)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     ACCUMULATED TOTAL PRECIPITATION.
      IF (IGET(087).GT.0) THEN
         CALL E2OUT(087,000,ACPREC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(087),LVLS(1,IGET(087)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     ACCUMULATED TOTAL PRECIPITATION NN - NEAREST NEIGHBOR.
      IF (IGET(246).GT.0) THEN
         CALL E2OUT(246,000,ACPREC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(246),LVLS(1,IGET(246)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     ACCUMULATED CONVECTIVE PRECIPITATION.
      IF (IGET(033).GT.0) THEN
         CALL E2OUT(033,000,CUPREC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(033),LVLS(1,IGET(033)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     ACCUMULATED GRID-SCALE PRECIPITATION.
      IF (IGET(034).GT.0) THEN
         CALL E2OUT(034,000,ANCPRC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(034),LVLS(1,IGET(034)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     ACCUMULATED SNOWFALL.
         IF (IGET(035).GT.0) THEN
            CALL E2OUT(035,000,ACSNOW,EGRID2,
     x           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITPREC     = INT(TPREC)
            IFINCR     = MOD(IFHR,ITPREC)
            ID(19)     = IFHR
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITPREC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(035),LVLS(1,IGET(035)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     ACCUMULATED SNOW MELT.
         IF (IGET(121).GT.0) THEN
            CALL E2OUT(121,000,ACSNOM,EGRID2,
     x           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITPREC     = INT(TPREC)
            IFINCR     = MOD(IFHR,ITPREC)
            ID(19)     = IFHR
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITPREC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(121),LVLS(1,IGET(121)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     ACCUMULATED STORM SURFACE RUNOFF.
         IF (IGET(122).GT.0) THEN
            CALL E2OUT(122,000,SSROFF,EGRID2,
     x           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITPREC     = INT(TPREC)
            IFINCR     = MOD(IFHR,ITPREC)
            ID(19)     = IFHR
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITPREC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(122),LVLS(1,IGET(122)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     ACCUMULATED BASEFLOW-GROUNDWATER RUNOFF.
         IF (IGET(123).GT.0) THEN
            CALL E2OUT(123,000,BGROFF,EGRID2,
     x           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITPREC     = INT(TPREC)
            IFINCR     = MOD(IFHR,ITPREC)
            ID(19)     = IFHR
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITPREC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(123),LVLS(1,IGET(123)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     INSTANTANEOUS PRECIPITATION TYPE.
         IF (IGET(160).GT.0) THEN
            CALL CALWXT(T,Q,RES,PD,HTM,LMH,PREC,PT,AETA,ETA,IWX1)
            ID(1:25) = 0
C     
C     DECOMPOSE IWX1 ARRAY
C
            DO J=JSTA,JEND
            DO I=1,IM
              IWX=IWX1(I,J)
              ISNO=MOD(IWX,2)
              IIP=MOD(IWX,4)/2
              IZR=MOD(IWX,8)/4
              IRAIN=IWX/8
              SNOW(I,J)   = ISNO*1.0
              SLEET(I,J)  = IIP*1.0
              FREEZR(I,J) = IZR*1.0
              RAIN(I,J)   = IRAIN*1.0
            ENDDO
            ENDDO
C     
C     INTERPOLATE/OUTPUT REQUESTED SURFACE FIELDS.
C     
C     SNOW.
            ID(8) = 143
            CALL E2OUT(160,000,SNOW,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(160),LVLS(1,IGET(160)),
     X           GRID1,IMOUT,JMOUT)
C     ICE PELLETS.
            ID(8) = 142
            CALL E2OUT(160,000,SLEET,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(160),LVLS(1,IGET(160)),
     X           GRID1,IMOUT,JMOUT)
C     FREEZING RAIN.
            ID(8) = 141
            CALL E2OUT(160,000,FREEZR,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(160),LVLS(1,IGET(160)),
     X           GRID1,IMOUT,JMOUT)
C     RAIN.
            ID(8) = 140
            CALL E2OUT(160,000,RAIN,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(160),LVLS(1,IGET(160)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C
C
C***  BLOCK 5.  SURFACE EXCHANGE FIELDS.
C     
C     TIME AVERAGED SURFACE LATENT HEAT FLUX.
         IF (IGET(042).GT.0) THEN
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = SFCLHX(I,J)*RRNUM
            ENDDO
            ENDDO
            CALL E2OUT(042,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITSRFC     = INT(TSRFC)
            IFINCR     = MOD(IFHR,ITSRFC)
            ID(19)     = IFHR
            ID(20)     = 3
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITSRFC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(042),LVLS(1,IGET(042)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C     TIME AVERAGED SURFACE SENSIBLE HEAT FLUX.
         IF (IGET(043).GT.0) THEN
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = SFCSHX(I,J)*RRNUM
            ENDDO
            ENDDO
            CALL E2OUT(043,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITSRFC     = INT(TSRFC)
            IFINCR     = MOD(IFHR,ITSRFC)
            ID(19)     = IFHR
            ID(20)     = 3
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITSRFC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(043),LVLS(1,IGET(043)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     TIME AVERAGED SUB-SURFACE SENSIBLE HEAT FLUX.
         IF (IGET(135).GT.0) THEN
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = SUBSHX(I,J)*RRNUM
            ENDDO
            ENDDO
            CALL E2OUT(135,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITSRFC     = INT(TSRFC)
            IFINCR     = MOD(IFHR,ITSRFC)
            ID(19)     = IFHR
            ID(20)     = 3
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITSRFC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(135),LVLS(1,IGET(135)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     TIME AVERAGED SNOW PHASE CHANGE HEAT FLUX.
         IF (IGET(136).GT.0) THEN
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = SNOPCX(I,J)*RRNUM
            ENDDO
            ENDDO
            CALL E2OUT(136,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITSRFC     = INT(TSRFC)
            IFINCR     = MOD(IFHR,ITSRFC)
            ID(19)     = IFHR
            ID(20)     = 3
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITSRFC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(136),LVLS(1,IGET(136)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     TIME AVERAGED SURFACE MOMENTUM FLUX.
         IF (IGET(046).GT.0) THEN
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = SFCUVX(I,J)*RRNUM
            ENDDO
            ENDDO
            CALL E2OUT(046,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITSRFC     = INT(TSRFC)
            IFINCR     = MOD(IFHR,ITSRFC)
            ID(19)     = IFHR
            ID(20)     = 3
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITSRFC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(046),LVLS(1,IGET(046)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     ACCUMULATED SURFACE EVAPORATION
         IF (IGET(047).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = SFCEVP(I,J)
            ENDDO
            ENDDO
            CALL E2OUT(047,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITPREC     = INT(TPREC)
            IFINCR     = MOD(IFHR,ITPREC)
            ID(19)     = IFHR
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITPREC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(047),LVLS(1,IGET(047)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     ACCUMULATED POTENTIAL EVAPORATION
         IF (IGET(137).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = POTEVP(I,J)
            ENDDO
            ENDDO
            CALL E2OUT(137,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            IFHR       = NTSD/TSPH + 0.5
            ITPREC     = INT(TPREC)
            IFINCR     = MOD(IFHR,ITPREC)
            ID(19)     = IFHR
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITPREC
            ELSE
               ID(18) = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,1000.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(137),LVLS(1,IGET(137)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     ROUGHNESS LENGTH.
      IF (IGET(044).GT.0) THEN
         CALL E2OUT(044,000,Z0,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(044),LVLS(1,IGET(044)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     FRICTION VELOCITY.
      IF (IGET(045).GT.0) THEN
         CALL E2OUT(045,000,USTAR,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(045),LVLS(1,IGET(045)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     SURFACE DRAG COEFFICIENT.
      IF (IGET(132).GT.0) THEN
         CALL CALDRG(EGRID1)
         CALL E2OUT(132,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(132),LVLS(1,IGET(132)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     SURFACE U AND/OR V COMPONENT WIND STRESS
      IF ( (IGET(133).GT.0) .OR. (IGET(134).GT.0) ) THEN
         CALL CALTAU(EGRID1,EGRID2)
C     
C        SURFACE U COMPONENT WIND STRESS.
         IF (IGET(133).GT.0) THEN
            CALL E2OUT(133,000,EGRID1,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(133),LVLS(1,IGET(133)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C        SURFACE V COMPONENT WIND STRESS
         IF (IGET(134).GT.0) THEN
            CALL E2OUT(134,000,EGRID2,EGRID1,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(134),LVLS(1,IGET(134)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
      ENDIF
C     
C     INSTANTANEOUS SENSIBLE HEAT FLUX
      IF (IGET(154).GT.0) THEN
         CALL E2OUT(154,000,TWBS,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(154),LVLS(1,IGET(154)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     INSTANTANEOUS LATENT HEAT FLUX
      IF (IGET(155).GT.0) THEN
         CALL E2OUT(155,000,QWBS,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(155),LVLS(1,IGET(155)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     SURFACE EXCHANGE COEFF
      IF (IGET(169).GT.0) THEN
         CALL E2OUT(169,000,SFCEXC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(169),LVLS(1,IGET(169)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     GREEN VEG FRACTION
      IF (IGET(170).GT.0) THEN
         CALL E2OUT(170,000,VEGFRC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(170),LVLS(1,IGET(170)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     INSTANTANEOUS GROUND HEAT FLUX
      IF (IGET(152).GT.0) THEN
         CALL E2OUT(152,000,GRNFLX,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(152),LVLS(1,IGET(152)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
!
!    VEGETATION TYPE
      IF (IGET(218).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = FLOAT(IVGTYP(I,J))
           ENDDO
         ENDDO
         CALL E2OUT(218,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(218),LVLS(1,IGET(218)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
!
!    SOIL TYPE
      IF (IGET(219).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = FLOAT(ISLTYP(I,J))
           ENDDO
         ENDDO
         CALL E2OUT(219,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(219),LVLS(1,IGET(219)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
!
!    SLOPE TYPE
      IF (IGET(223).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = FLOAT(ISLOPE(I,J))
           ENDDO
         ENDDO
         CALL E2OUT(223,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(02)= 130
         CALL OUTPUT(IOUTYP,IGET(223),LVLS(1,IGET(223)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
!
! CANOPY CONDUCTANCE
      IF (IGET(220).GT.0 .OR. IGET(234).GT.0
     & .OR. IGET(235).GT.0 .OR. IGET(236).GT.0
     & .OR. IGET(237).GT.0 .OR. IGET(238).GT.0
     & .OR. IGET(239).GT.0 .OR. IGET(240).GT.0
     & .OR. IGET(241).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
!             IF(abs(SM(I,J)-0.).lt.1.0E-5)THEN
             IF( (abs(SM(I,J)-0.)   .lt. 1.0E-5) .AND.
     &           (abs(SICE(I,J)-0.) .lt. 1.0E-5) ) THEN
              IF(CZMEAN(I,J).GT.1.E-6) THEN
               FACTRS=CZEN(I,J)/CZMEAN(I,J)
              ELSE
               FACTRS=0.0
              ENDIF
              SOLAR=HBM2(I,J)*RSWIN(I,J)*FACTRS
              SFCTMP=T(I,J,LMH(I,J))
              SFCQ=Q(I,J,LMH(I,J))
              SFCPRS=PT+PDSL(I,J)*AETA(LMH(I,J))
!              IF(IVGTYP(I,J).EQ.0)PRINT*,'IVGTYP ZERO AT ',I,J
!     &        ,SM(I,J)
              IVG=IVGTYP(I,J)
!              IF(IVGTYP(I,J).EQ.0)IVG=7
!              CALL CANRES(SOLAR,SFCTMP,SFCQ,SFCPRS
!     &        ,SMC(I,J,1:NSOIL),GC(I,J),RC,IVG,ISLTYP(I,J))
!
              CALL CANRES(SOLAR,SFCTMP,SFCQ,SFCPRS
     &        ,SH2O(I,J,1:NSOIL),GC(I,J),RC,IVG,ISLTYP(I,J)
     &        ,RSMIN(I,J),NROOTS(I,J),SMCWLT(I,J),SMCREF(I,J)
     &        ,RCS(I,J),RCQ(I,J),RCT(I,J),RCSOIL(I,J))
               IF(abs(SMCWLT(I,J)-0.5).lt.1.e-5)print*,
     &       'LARGE SMCWLT',i,j,SM(I,J),ISLTYP(I,J),SMCWLT(I,J)
             ELSE
              GC(I,J)=0.
              RSMIN(I,J)=0.
              NROOTS(I,J)=0
              SMCWLT(I,J)=0.
              SMCREF(I,J)=0.
              RCS(I,J)=0.
              RCQ(I,J)=0.
              RCT(I,J)=0.
              RCSOIL(I,J)=0.
             END IF
           ENDDO
         ENDDO
         IF (IGET(220).GT.0 )THEN
          CALL E2OUT(220,000,GC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(220),LVLS(1,IGET(220)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
         IF (IGET(234).GT.0 )THEN
          CALL E2OUT(234,000,RSMIN,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(234),LVLS(1,IGET(234)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
!    NO OF ROOT LAYERS
      IF (IGET(235).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = FLOAT(NROOTS(I,J))
           ENDDO
         ENDDO
         CALL E2OUT(235,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(02)= 130
         CALL OUTPUT(IOUTYP,IGET(235),LVLS(1,IGET(235)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
!
         IF (IGET(236).GT.0 )THEN
          CALL E2OUT(236,000,SMCWLT,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(236),LVLS(1,IGET(236)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
         IF (IGET(237).GT.0 )THEN
          CALL E2OUT(237,000,SMCREF,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(237),LVLS(1,IGET(237)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
         IF (IGET(238).GT.0 )THEN
          CALL E2OUT(238,000,RCS,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(238),LVLS(1,IGET(238)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
         IF (IGET(239).GT.0 )THEN
          CALL E2OUT(239,000,RCT,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(239),LVLS(1,IGET(239)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
         IF (IGET(240).GT.0 )THEN
          CALL E2OUT(240,000,RCQ,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(240),LVLS(1,IGET(240)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
         IF (IGET(241).GT.0 )THEN
          CALL E2OUT(241,000,RCSOIL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25) = 0
          ID(02)= 130
          CALL OUTPUT(IOUTYP,IGET(241),LVLS(1,IGET(241)),
     X        GRID1,IMOUT,JMOUT)
         END IF
!
      ENDIF
C     
C     DQADV
      IF (IGET(200).GT.0) THEN
         CALL E2OUT(200,000,DQADV,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(200),LVLS(1,IGET(200)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     DQFLX / WVCONV
      IF (IGET(201).GT.0) THEN
         CALL E2OUT(201,000,DQFLX,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(201),LVLS(1,IGET(201)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=DQFLX(I,J)-DQFLX7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(201,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(201),LVLS(1,IGET(201)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     DCFLX / WCCONV
      IF (IGET(202).GT.0) THEN
         CALL E2OUT(202,000,DCFLX,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(202),LVLS(1,IGET(202)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=DCFLX(I,J)-DCFLX7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(202,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(202),LVLS(1,IGET(202)),
     X        GRID1,IMOUT,JMOUT)

      ENDIF
C
C     FQU / WVUFLX
      IF (IGET(203).GT.0) THEN
         CALL E2OUT(203,000,FQU,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(203),LVLS(1,IGET(203)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=FQU(I,J)-FQU7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(203,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(203),LVLS(1,IGET(203)),
     X        GRID1,IMOUT,JMOUT)

      ENDIF
C
C     FQV / WVVFLX
      IF (IGET(204).GT.0) THEN
         CALL E2OUT(204,000,FQV,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(204),LVLS(1,IGET(204)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=FQV(I,J)-FQV7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(204,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(204),LVLS(1,IGET(204)),
     X        GRID1,IMOUT,JMOUT)

      ENDIF
C
C     FCU / WCUFLX
      IF (IGET(205).GT.0) THEN
         CALL E2OUT(205,000,FCU,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(205),LVLS(1,IGET(205)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=FCU(I,J)-FCU7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(205,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(205),LVLS(1,IGET(205)),
     X        GRID1,IMOUT,JMOUT)

      ENDIF
C
C     FCV / WCVFLX
      IF (IGET(206).GT.0) THEN
         CALL E2OUT(206,000,FCV,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(206),LVLS(1,IGET(206)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=FCV(I,J)-FCV7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(206,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(206),LVLS(1,IGET(206)),
     X        GRID1,IMOUT,JMOUT)

      ENDIF


C
C     VAPINC / WVINC
      IF (IGET(207).GT.0) THEN
         CALL E2OUT(207,000,VAPINC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(207),LVLS(1,IGET(207)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=VAPINC(I,J)-VAPINC7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(207,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(207),LVLS(1,IGET(207)),
     X        GRID1,IMOUT,JMOUT)

      ENDIF
C
C     CLDINC / WCINC
      IF (IGET(208).GT.0) THEN
         CALL E2OUT(208,000,CLDINC,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         CALL OUTPUT(IOUTYP,IGET(208),LVLS(1,IGET(208)),
     X        GRID1,IMOUT,JMOUT)

         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=CLDINC(I,J)-CLDINC7(I,J)
         ENDDO
         ENDDO

         CALL E2OUT(208,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         IFHR       = NTSD/TSPH + 0.5
         ITPREC     = INT(TPREC)
         IFINCR     = MOD(IFHR,ITPREC)
         ID(19)     = IFHR
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
            ID(18) = IFHR-ITPREC
         ELSE
            ID(18) = IFHR-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(9) = 101
         ID(10) = 0
         ID(11) = 70
         CALL OUTPUT(IOUTYP,IGET(208),LVLS(1,IGET(208)),
     X        GRID1,IMOUT,JMOUT)

      ENDIF



C     END OF ROUTINE
      print*,'End of SURFCE2'
C     
      RETURN
      END
