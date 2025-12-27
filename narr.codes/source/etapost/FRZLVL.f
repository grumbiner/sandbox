      SUBROUTINE FRZLVL(ZFRZ,RHFRZ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    FRZLVL      COMPUTES FRZING LVL Z AND RH
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES THE FREEZING LEVEL HEIGHT AND RELATIVE
C     HUMIDITY AT THIS LEVEL FOR EACH MASS POINT ON THE ETA GRID.
C     THE COMPUTED FREEZING LEVEL HEIGHT IS THE MEAN SEA LEVEL
C     HEIGHT.  AT EACH MASS POINT WE MOVE UP FROM THE SURFACE TO 
C     FIND THE FIRST ETA LAYER WHERE THE TEMPERATURE IS LESS THAN
C     273.16K.  VERTICAL INTERPOLATION IN TEMPERATURE TO THE FREEZING
C     TEMPERATURE GIVES THE FREEZING LEVEL HEIGHT.  PRESSURE AND 
C     SPECIFIC HUMIDITY ARE INTERPOLATED TO THIS LEVEL AND ALONG WITH
C     THE TEMPERATURE PROVIDE THE FREEZING LEVEL RELATIVE HUMIDITY.
C     IF THE SURFACE (SKIN) TEMPERATURE IS BELOW FREEZING, THE ROUTINE
C     USES SURFACE BASED FIELDS TO COMPUTE THE RELATIVE HUMIDITY.
C     
C     NOTE THAT IN POSTING FREEZING LEVEL DATA THE LFM LOOK-ALIKE FILE
C     (IE, GRID 26), WE PACK 273.15K AS THE FREEZING TEMPERATURE.  ALL
C     OTHER OUTPUT GRIDS USE 273.16K
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   93-06-10  RUSS TREADON - CORRECTED FREEZING LEVEL HEIGHTS TO BE
C                            WITH REPSECT TO MEAN SEA LEVEL, NOT  
C                            ABOVE GROUND LEVEL.
C   98-06-15  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-17  MIKE BALDWIN - COMPUTE RH OVER ICE IF NECESSARY
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL FRZLVL(ZFRZ,RHFRZ)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     ZFRZ     - ABOVE GROUND LEVEL FREEZING HEIGHT.
C     RHFRZ    - RELATIVE HUMIDITY AT FREEZING LEVEL.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - VRBLS
C                  LOOPS
C                  EXTRA
C                  PVRBLS
C                  MASKS
C                  MAPOT
C                  POSTVAR
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE/SET PARAMETERS.
      INCLUDE "parmeta"
      INCLUDE "params"
C
C     DECLARE VARIABLES.
C     
      REAL RHFRZ(IM,JM),ZFRZ(IM,JM),IWSFC(IM,JM),IWM1(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "VRBLS.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "CTLBLK.comm"
C     
C*********************************************************************
C     START FRZLVL.
C
C
C     COMPUTE IW AT SURFACE IN CASE WE NEED TO COMPUTE RH OVER ICE
C

        CLIMIT =1.0E-20
        IWSFC=0.
C
        DO L=2,LM 
        DO J=JSTA,JEND
        DO I=1,IM
          IF (L.LE.LMH(I,J)) THEN
           IWM1(I,J)=IWSFC(I,J)
           IF(CWM(I,J,L).GT.CLIMIT) THEN
             IF(T(I,J,L).LT.258.15)THEN
               IWSFC(I,J)=1.
             ELSEIF(T(I,J,L).GE.273.15)THEN
               IWSFC(I,J)=0.
             ELSE
               IF(IWM1(I,J).EQ.1.0)IWSFC(I,J)=1.
             ENDIF
           ELSE
             IWSFC(I,J)=0.
           ENDIF
          ENDIF
        ENDDO
        ENDDO
        ENDDO
C     
C     LOOP OVER HORIZONTAL GRID.
C     
!$omp  parallel do
!$omp& private(ai,alpfrz,alph,alpl,bi,delalp,delq,delt,delz,
!$omp&         delzp,dzabv,dzfr,htsfc,l,llmh,psfc,qfrz,qi,qint,
!$omp&         qsat,qsfc,qsfrz,qw,rhsfc,rhz,tmt0,tmt15,tsfc,
!$omp&         zl,zu)
      DO 20 J=JSTA,JEND
      DO 20 I=1,IM
         HTSFC    = FIS(I,J)*GI
         LLMH     = LMH(I,J)
         RHFRZ(I,J) = D00
         ZFRZ(I,J)  = HTSFC
C     
C        CHECK IF FREEZING LEVEL IS AT THE GROUND.
C     
         TSFC = SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J)
         IF (TSFC.LE.TFRZ) THEN
            ZFRZ(I,J) = HTSFC
            PSFC    = PD(I,J)+PT
            QSFC    = SM(I,J)*QZ0(I,J)+(1.-SM(I,J))*QS(I,J)
C
            TMT0=TSFC-273.16
            TMT15=AMIN1(TMT0,-15.)
            AI=0.008855
            BI=1.
            IF(TMT0.LT.-20.)THEN
              AI=0.007225
              BI=0.9674
            ENDIF
            QW=PQ0/PSFC
     1          *EXP(A2*(TSFC-A3)/(TSFC-A4))
            QI=QW*(BI+AI*AMIN1(TMT0,0.))
            QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
            IF(TMT0.LT.-15.)THEN
                QSAT=QI
            ELSEIF(TMT0.GE.0.)THEN
                QSAT=QINT
            ELSE
              IF(IWSFC(I,J).GT.0.0) THEN
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
            RHSFC   = QSFC/QSAT
            RHSFC   = AMAX1(0.01,RHSFC)
            RHSFC   = AMIN1(RHSFC,1.0)
            RHFRZ(I,J)= RHSFC
            GOTO 20
         ENDIF
C     
C        OTHERWISE, LOCATE THE FREEZING LEVEL ALOFT.
C
         DO 10 L = LLMH,1,-1
            IF (T(I,J,L).LE.TFRZ) THEN
               IF (L.LT.LLMH-1) THEN
                  DELZ = D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
                  ZL   = D50*(ZINT(I,J,L+1)+ZINT(I,J,L+2))
                  DELT = T(I,J,L)-T(I,J,L+1)
                  ZFRZ(I,J) = ZL + (TFRZ-T(I,J,L+1))/DELT*DELZ
C     
                  DZABV = ZFRZ(I,J)-ZL
                  DELQ  = Q(I,J,L)-Q(I,J,L+1)
                  QFRZ  = Q(I,J,L+1) + DELQ/DELZ*DZABV
                  QFRZ  = AMAX1(0.0,QFRZ)
C     
C
                  ALPL   = ALPINT(I,J,L+2)
                  ALPH   = ALPINT(I,J,L)
                  DELALP = ALPH - ALPL
                  DELZP  = ZINT(I,J,L)-ZINT(I,J,L+2)
                  DZFR   = ZFRZ(I,J) - ZINT(I,J,L+2)
                  ALPFRZ = ALPL + DELALP/DELZP*DZFR
                  PFRZ   = EXP(ALPFRZ)
                  QSFRZ  = PQ0/PFRZ
C     
                  RHZ      = QFRZ/QSFRZ
                  RHZ      = AMAX1(0.01,RHZ)
                  RHZ      = AMIN1(RHZ,1.0)
                  RHFRZ(I,J) = RHZ
C     
               ELSE
                  ZU      = D50*(ZINT(I,J,L)+ZINT(I,J,L+1))
                  ZL      = HTSFC
                  DELZ    = ZU-ZL
                  TSFC    = SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J)
                  DELT    = T(I,J,L)-TSFC
                  ZFRZ(I,J) = ZL + (TFRZ-TSFC)/DELT*DELZ
C     
                  DZABV   = ZFRZ(I,J)-ZL
                  QSFC    = SM(I,J)*QZ0(I,J)+(1.-SM(I,J))*QS(I,J)
                  DELQ    = Q(I,J,L)-QSFC
                  QFRZ    = QSFC + DELQ/DELZ*DZABV
                  QFRZ    = AMAX1(0.0,QFRZ)
C     
                  ALPH    = ALPINT(I,J,L)
                  PSFC    = PD(I,J)+PT
                  ALPL    = ALOG(PSFC)
                  DELALP  = ALPH-ALPL
                  ALPFRZ  = ALPL + DELALP/DELZ*DZABV
                  PFRZ    = EXP(ALPFRZ)
                  QSFRZ   = PQ0/PFRZ
C
                  RHZ     = QFRZ/QSFRZ
                  RHZ     = AMAX1(0.01,RHZ)
                  RHZ     = AMIN1(RHZ,1.0)
                  RHFRZ(I,J)= RHZ
               ENDIF
C     
C              BOUND FREEZING LEVEL RH.  FREEZING LEVEL HEIGHT IS
C              MEASURED WITH RESPECT TO MEAN SEA LEVEL.
C
               RHFRZ(I,J) = AMAX1(0.01,RHFRZ(I,J))
               RHFRZ(I,J) = AMIN1(RHFRZ(I,J),1.00)
               ZFRZ(I,J)  = AMAX1(0.0,ZFRZ(I,J))
               GOTO 20
            ENDIF
 10      CONTINUE
 20   CONTINUE
C     
C     END OF ROUTINE.
C     
      RETURN
      END
