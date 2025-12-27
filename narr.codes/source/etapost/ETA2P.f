      SUBROUTINE ETA2P(IMOUT,JMOUT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    ETA2P       VERT INTRP OF ETA TO PRESSURE
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-21       
C     
C ABSTRACT:
C     FOR MOST APPLICATIONS THIS ROUTINE IS THE WORKHORSE
C     OF THE POST PROCESSOR.  IN A NUTSHELL IT INTERPOLATES
C     DATA FROM ETA TO PRESSURE SURFACES.  IT ORIGINATED
C     FROM THE VERTICAL INTERPOLATION CODE IN THE OLD ETA
C     POST PROCESSOR SUBROUTINE OUTMAP.  
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-21  RUSS TREADON
C   96-03-21  GEOFF MANIKIN - ADDED CLOUD ICE ON P
C   98-06-16  T BLACK       - CONVERSION FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN  - REMOVED LABL84
C   98-08-18  T BLACK       - REMOVED MOST 3-D ARRAYS FROM
C                             COMMON BLOCK JIMA
C   98-12-22  MIKE BALDWIN  - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO  - MPI VERSION
C     
C USAGE:    CALL ETA2P(IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID
C
C   OUTPUT ARGUMENT LIST: 
C     NONE       
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       SCLFLD   - SCALE ARRAY ELEMENTS BY CONSTANT.
C       E2OUT    - E-GRID TO OUTPUT GRID INTERPOLATION/SMOOTHING.
C       OUTPUT   - POST ARRAY TO OUTPUT FILE.
C       CALPOT2  - COMPUTE POTENTIAL TEMPERATURE.
C       CALRH2   - COMPUTE RELATIVE HUMIDITY.
C       CALDWP2  - COMPUTE DEWPOINT TEMPERATURE.
C       BOUND    - BOUND ARRAY ELEMENTS BETWEEN LOWER AND UPPER LIMITS.
C       CALMCVG  - COMPUTE MOISTURE CONVERGENCE.
C       CALVOR   - COMPUTE ABSOLUTE VORTICITY.
C       CALSTRM  - COMPUTE GEOSTROPHIC STREAMFUNCTION.
C
C     LIBRARY:
C       COMMON   - OMGAOT
C                  LOOPS
C                  MASKS
C                  MAPOT
C                  VRBLS
C                  PVRBLS
C                  RQSTFLD
C                  EXTRA
C                  CLDWTR
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$  
C
C
C     
C     INCLUDE ETA MODEL DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     GAMMA AND RGAMOG ARE USED IN THE EXTRAPOLATION OF VIRTUAL
C     TEMPERATURES BEYOND THE UPPER OF LOWER LIMITS OF ETA DATA.
C     
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "params"
C
      PARAMETER (GAMMA=6.5E-3,RGAMOG=RD*GAMMA/G)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STDRD
      LOGICAL IOOMG,IOALL
      REAL OSL(IM,JM),USL(IM,JM),VSL(IM,JM)
      REAL PRSI(IM,JM),QCSL(IM,JM),Q2SL(IM,JM)
      REAL ICE(IM,JM),IW(IM,JM,LM),IWU,IWL

      REAL EGRID1(IM,JM),EGRID2(IM,JM)
      REAL GRID1(IMOUT,JMOUT),GRID2(IMOUT,JMOUT)
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "OMGAOT.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "RQSTFLD.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "E2PFLG.comm"
C
      COMMON/JIMA/NL1X(IM,JM),ALPETUX(IM,JM),ALPET2X(IM,JM)

C     
C******************************************************************************
C
C     START ETA2P. 
C     
C     SET TOTAL NUMBER OF POINTS ON OUTPUT GRID.
C
C---------------------------------------------------------------
C
C     *** PART I ***
C
C     VERTICAL INTERPOLATION OF EVERYTHING ELSE.  EXECUTE ONLY
C     IF THERE'S SOMETHING WE WANT.
C
      IF((IGET(012).GT.0).OR.(IGET(013).GT.0).OR.
     X   (IGET(014).GT.0).OR.(IGET(015).GT.0).OR.
     X   (IGET(016).GT.0).OR.(IGET(017).GT.0).OR.
     X   (IGET(018).GT.0).OR.(IGET(019).GT.0).OR.
     X   (IGET(020).GT.0).OR.(IGET(030).GT.0).OR.
     X   (IGET(021).GT.0).OR.(IGET(022).GT.0).OR.
     X   (IGET(153).GT.0).OR.(IGET(166).GT.0))THEN
C
C  SET UP UTIM FOR THIS TIME STEP
C
          UTIM=1.
          CLIMIT =1.0E-20
C
          DO 975 L=1,LM
            IF(L.EQ.1)THEN
!$omp  parallel do
              DO J=JSTA,JEND
              DO I=1,IM
                IW(I,J,L)=0.
              ENDDO
              ENDDO
              GO TO 975
            ENDIF
C
!$omp  parallel do
!$omp& private(cwmkl,fiq,hh,lml,pp,qi,qkl,qw,tkl,tmt0,tmt15,u00kl)
            DO 970 J=JSTA,JEND
            DO 970 I=1,IM
              LML=LM-LMH(I,J)
              HH=HTM(I,J,L)*HBM2(I,J)
              TKL=T(I,J,L)
              QKL=Q(I,J,L)
              CWMKL=CWM(I,J,L)
              TMT0=(TKL-273.16)*HH
              TMT15=AMIN1(TMT0,-15.)*HH    
              PP=PDSL(I,J)*AETA(L)+PT
              QW=HH*PQ0/PP*EXP(HH*A2*(TKL-A3)/(TKL-A4))
              QI=QW*(1.+0.01*AMIN1(TMT0,0.))     
              U00KL=U00(I,J)+UL(L+LML)*(0.95-U00(I,J))*UTIM
C
              IF(TMT0.LT.-15.0)THEN
                FIQ=QKL-U00KL*QI
                IF(FIQ.GT.D00.OR.CWMKL.GT.CLIMIT) THEN
                  IW(I,J,L)=1.
                ELSE
                  IW(I,J,L)=0.
                ENDIF
              ENDIF
C
              IF(TMT0.GE.0.0)IW(I,J,L)=0.
              IF(TMT0.LT.0.0.AND.TMT0.GE.-15.0)THEN
                IW(I,J,L)=0.
                IF(IW(I,J,L-1).EQ.1.0.AND.CWMKL.GT.CLIMIT)IW(I,J,L)=1.
              ENDIF
  970       CONTINUE
  975     CONTINUE
C
C
C     VERTICAL INTERPOLATION OF GEOPOTENTIAL, SPECIFIC HUMIDITY,
C     AND TEMPERATURE.  START AT THE UPPERMOST TARGET PRESSURE LEVEL.
C
        ALPTH = ALOG(1.E5)
        DO 310 L=1,LSL
C
C
c         IF(IOALL)GO TO 225
C
          TRF=H2*ALSL(L)
C     
C       LOOP OVER HORIZONTAL GRID.
C
!$omp  parallel do
!$omp& private(lma,lmap1,ppdsl)
          DO 180 J=JSTA,JEND
          DO 180 I=1,IM
            LMA=LM
CX          IF(OLDRD)LMA=LMH(I,J)
            LMAP1=LMA+1
C
C           SET PRESSURE DEPTH IN THIS COLUMN.
C
            PPDSL=PDSL(I,J)
C     
C           LOCATE VERTICAL INDEX OF ETA INTERFACE PRESSURES BOUNDING
C           THE STANDARD PRESSURE LEVEL TO WHICH WE'RE INTERPOLATING.
C
            DO 170 IL=2,LMAP1
               IF((ALSL(L)-ALPINT(I,J,IL)).GT.D00)GO TO 170
               NL1X(I,J)=IL
               GO TO 180
  170       CONTINUE
            NL1X(I,J)=LMAP1
  180     CONTINUE

!$omp  parallel do
!$omp& private(ahf,ahfo,ahfq,ahfq2,ahfqc,ahfqi,ai,b,bi,bom,
!$omp&         bq,bq2,bqc,bqc_2,bqi_2,bqi,fac,gmiw,gmiw_2,iwl,iwu,
!$omp&         lma,lmap1,pl,pnl1,pu,q2a,q2b,qabv,qi,qint,ql,
!$omp&         qsat,qu,qw,rhu,tabv,tblo,tl,tmt0,tmt15,tu,
!$omp&         tvrabv,tvrblo,tvrl,tvru,zl,zu)
          DO 220 J=JSTA,JEND
          DO 220 I=1,IM
            LMA  =LM
            LMAP1=LMA+1
            IF((TRF-ALPINT(I,J,NL1X(I,J))
     1             -ALPINT(I,J,NL1X(I,J)-1)).LE.D00) 
     2          NL1X(I,J)=NL1X(I,J)-1
            PNL1=PINT(I,J,NL1X(I,J))
C     
C           BRANCH TO APPROPRIATE BLOCK TO COMPUTE COEFFICIENTS.
C
            IF(NL1X(I,J).EQ.1)THEN
              PU=PINT(I,J,2)
              ZU=ZINT(I,J,2)
              TU=D50*(T(I,J,1)+T(I,J,2))
              QU=D50*(Q(I,J,1)+Q(I,J,2))
C
              IWU=D50*(IW(I,J,1)+IW(I,J,2))
              TMT0=TU-273.16
              TMT15=AMIN1(TMT0,-15.)
              AI=0.008855
              BI=1.
              IF(TMT0.LT.-20.)THEN
                AI=0.007225
                BI=0.9674
              ENDIF
              QW=PQ0/PU
     1          *EXP(A2*(TU-A3)/(TU-A4))
              QI=QW*(BI+AI*AMIN1(TMT0,0.))
              QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
              IF(TMT0.LT.-15.)THEN
                  QSAT=QI
              ELSEIF(TMT0.GE.0.)THEN
                  QSAT=QINT
              ELSE
                IF(IWU.GT.0.0) THEN
                  QSAT=QI
                ELSE
                  QSAT=QINT
                ENDIF
              ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
              QSAT=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
              RHU =QU/QSAT
C
              IF(RHU.GT.H1)THEN
                RHU=H1
                QU =RHU*QSAT
              ENDIF
C
              IF(RHU.LT.D01)THEN
                RHU=D01
                QU =RHU*QSAT
              ENDIF
C
              TVRU=TU*(H1+D608*QU)
              TVRABV=TVRU*(SPL(L)/PU)**RGAMOG
              TABV=TVRABV/(H1+D608*QU)
C     
C
              TMT0=TABV-273.16
              TMT15=AMIN1(TMT0,-15.)
              AI=0.008855
              BI=1.
              IF(TMT0.LT.-20.)THEN
                AI=0.007225
                BI=0.9674
              ENDIF
              QW=PQ0/SPL(L)
     1          *EXP(A2*(TABV-A3)/(TABV-A4))
              QI=QW*(BI+AI*AMIN1(TMT0,0.))
              QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
              IF(TMT0.LT.-15.)THEN
                  QSAT=QI
              ELSEIF(TMT0.GE.0.)THEN
                  QSAT=QINT
              ELSE
                IF(IWU.GT.0.0) THEN
                  QSAT=QI
                ELSE
                  QSAT=QINT
                ENDIF
              ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
              QSAT=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
              QABV =RHU*QSAT
              QABV =AMAX1(H1M12,QABV)
              B    =TABV
              BQ   =QABV
              BOM  =OMGA(I,J,1)
              GMIW =IW(I,J,1) 
              BQC  =(1.-GMIW)*CWM(I,J,1)
              BQI  =GMIW*CWM(I,J,1)
              Q2A  =D50*(Q2(I,J,1)+Q2(I,J,2))
              BQ2  =Q2A
              AHF  =D00
              AHFQ =D00
              AHFO =D00
              AHFQC=D00
              AHFQI=D00
              AHFQ2=D00
              FAC  =D00
C
            ELSEIF(NL1X(I,J).EQ.LMAP1)THEN 
C     
C           EXTRAPOLATION AT LOWER BOUND.  THE LOWER BOUND IS
C           LM IF OLDRD=.FALSE.  IF OLDRD=.TRUE. THE LOWER 
C           BOUND IS THE FIRST ATMOSPHERIC ETA LAYER.
C
              PL=PINT(I,J,LMA-1)
              ZL=ZINT(I,J,LMA-1)
              TL=D50*(T(I,J,LMA-2)+T(I,J,LMA-1))
              QL=D50*(Q(I,J,LMA-2)+Q(I,J,LMA-1))
C     
              IWL=D50*(IW(I,J,LMA-2)+IW(I,J,LMA-1))

              TMT0=TL-273.16
              TMT15=AMIN1(TMT0,-15.)
              AI=0.008855
              BI=1.
              IF(TMT0.LT.-20.)THEN
                AI=0.007225
                BI=0.9674
              ENDIF
              QW=PQ0/PL
     1          *EXP(A2*(TL-A3)/(TL-A4))
              QI=QW*(BI+AI*AMIN1(TMT0,0.))
              QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
              IF(TMT0.LT.-15.)THEN
                  QSAT=QI
              ELSEIF(TMT0.GE.0.)THEN
                  QSAT=QINT
              ELSE
                IF(IWL.GT.0.0) THEN
                  QSAT=QI
                ELSE
                  QSAT=QINT
                ENDIF
              ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
              QSAT=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
              RHL=QL/QSAT
C
              IF(RHL.GT.H1)THEN
               RHL=H1
               QL =RHL*QSAT
              ENDIF
C
              IF(RHL.LT.D01)THEN
                RHL=D01
                QL =RHL*QSAT
              ENDIF
C
              TVRL  =TL*(H1+D608*QL)
              TVRBLO=TVRL*(SPL(L)/PL)**RGAMOG
              TBLO  =TVRBLO/(H1+D608*QL)
C     
              TMT0=TBLO-273.16
              TMT15=AMIN1(TMT0,-15.)
              AI=0.008855
              BI=1.
              IF(TMT0.LT.-20.)THEN
                AI=0.007225
                BI=0.9674
              ENDIF
              QW=PQ0/SPL(L)
     1          *EXP(A2*(TBLO-A3)/(TBLO-A4))
              QI=QW*(BI+AI*AMIN1(TMT0,0.))
              QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
              IF(TMT0.LT.-15.)THEN
                  QSAT=QI
              ELSEIF(TMT0.GE.0.)THEN
                  QSAT=QINT
              ELSE
                IF(IWL.GT.0.0) THEN
                  QSAT=QI
                ELSE
                  QSAT=QINT
                ENDIF
              ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
              QSAT=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
              QBLO =RHL*QSAT
              QBLO =AMAX1(H1M12,QBLO)
              B    =TBLO
              BQ   =QBLO
              BOM  =OMGA(I,J,LMA)
              GMIW =IW(I,J,LMH(I,J))
              BQC  =(1.-GMIW)*CWM(I,J,LMH(I,J))
              BQI  =GMIW*CWM(I,J,LMH(I,J)) 
              Q2A  =D50*(Q2(I,J,LMH(I,J)-1)+Q2(I,J,LMH(I,J)))
              BQ2  =Q2A
              AHF  =D00
              AHFQ =D00
              AHFO =D00
              AHFQC=D00
              AHFQI=D00
              AHFQ2=D00
              FAC  =D00
C
            ELSE
C     
C           INTERPOLATION BETWEEN LOWER AND UPPER BOUNDS.
C
              B     =T(I,J,NL1X(I,J))
              BQ    =Q(I,J,NL1X(I,J))
              BOM   =OMGA(I,J,NL1X(I,J))
              GMIW  =IW(I,J,NL1X(I,J))
              BQC   =(1.-GMIW)*CWM(I,J,NL1X(I,J))
              BQI   =GMIW*CWM(I,J,NL1X(I,J)) 
              GMIW_2=IW(I,J,NL1X(I,J)-1)
              BQC_2 =(1.-GMIW)*CWM(I,J,NL1X(I,J)-1)
              BQI_2 =GMIW*CWM(I,J,NL1X(I,J)-1)
              Q2B   =D50*(Q2(I,J,NL1X(I,J)-1)+Q2(I,J,NL1X(I,J)))
C
              IF(NL1X(I,J).GT.2)THEN
                Q2A=D50*(Q2(I,J,NL1X(I,J)-2)+Q2(I,J,NL1X(I,J)-1))
              ELSE
                Q2A=Q2B
              ENDIF
C
              BQ2=Q2B*HTM(I,J,NL1X(I,J))
              FAC  =H2*ALOG(PT+PDSL(I,J)*AETA(NL1X(I,J)))
              AHF  =(B-T(I,J,NL1X(I,J)-1))/
     1              (ALPINT(I,J,NL1X(I,J)+1)-ALPINT(I,J,NL1X(I,J)-1))
              AHFQ =(BQ-Q(I,J,NL1X(I,J)-1))/
     1              (ALPINT(I,J,NL1X(I,J)+1)-ALPINT(I,J,NL1X(I,J)-1))
              AHFO =(BOM-OMGA(I,J,NL1X(I,J)-1))/
     1              (ALPINT(I,J,NL1X(I,J)+1)-ALPINT(I,J,NL1X(I,J)-1))
              AHFQC=(BQC-BQC_2)/
     1              (ALPINT(I,J,NL1X(I,J)+1)-ALPINT(I,J,NL1X(I,J)-1))
              AHFQI=(BQI-BQI_2)/
     1              (ALPINT(I,J,NL1X(I,J)+1)-ALPINT(I,J,NL1X(I,J)-1))
              AHFQ2=(BQ2-Q2A*HTM(I,J,NL1X(I,J)-1))/
     1              (ALPINT(I,J,NL1X(I,J)+1)-ALPINT(I,J,NL1X(I,J)-1))
            ENDIF
C
            TSL(I,J)=B+AHF*(TRF-FAC)
            QSL(I,J)=BQ+AHFQ*(TRF-FAC)
            QSL(I,J)=AMAX1(QSL(I,J),H1M12)
            IF (QSL(I,J).LT.1.5E-12) QSL(I,J)=0.0     ! NARR
            OSL(I,J)=BOM+AHFO*(TRF-FAC)
            QCSL(I,J)=BQC+AHFQC*(TRF-FAC)
            QCSL(I,J)=AMAX1(QCSL(I,J),H1M12)
            IF (QCSL(I,J).LT.1.5E-12) QCSL(I,J)=0.0   ! NARR
            ICE(I,J)=BQI+AHFQI*(TRF-FAC)
            ICE(I,J)=AMAX1(ICE(I,J),H1M12)
            IF (ICE(I,J).LT.1.5E-12) ICE(I,J)=0.0     ! NARR
            Q2SL(I,J)=BQ2+AHFQ2*(TRF-FAC)
            Q2SL(I,J)=AMAX1(Q2SL(I,J),D00)
            FSL(I,J)=(PNL1-SPL(L))/(SPL(L)+PNL1)
     1           *((ALSL(L)+ALPINT(I,J,NL1X(I,J))-FAC)*AHF+B)*R*H2
     2           +ZINT(I,J,NL1X(I,J))*G
  220     CONTINUE
C
C        LOAD GEOPOTENTIAL AND TEMPERATURE INTO STANDARD LEVEL 
C        ARRAYS FOR THE NEXT PASS.
C
  225    CONTINUE
C     
C        SAVE 500MB TEMPERATURE FOR LIFTED INDEX.
C     
          IF((NINT(SPL(L)).EQ.50000).AND.
     1        ((IGET(030).GT.0).OR.(IGET(031).GT.0).OR.
     2         (IGET(075).GT.0)))THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              T500(I,J)=TSL(I,J)
            ENDDO
            ENDDO
          ENDIF
C     
C        CALCULATE 1000MB GEOPOTENTIALS CONSISTENT WITH SLP OBTAINED 
C        FROM THE MESINGER OR NWS SHUELL SLP REDUCTION.
C     
          IF(NINT(SPL(L)).EQ.100000)THEN
C     
C         MESINGER SLP
C
            IF(IGET(023).GT.0)THEN
!$omp  parallel do
              DO J=JSTA,JEND
              DO I=1,IM
                ALPSL=ALOG(PSLP(I,J))
                IF(FIS(I,J).GT.H1)THEN
                  FSL(I,J)=FIS(I,J)/(ALPSL-ALOG(PD(I,J)+PT))*
     1                              (ALPSL-ALPTH)
                ELSE
                  FSL(I,J)=R*T(I,J,LM)*(ALPSL-ALPTH)
                ENDIF
                Z1000(I,J)=FSL(I,J)*GI
              ENDDO
              ENDDO
C     
C           NWS SHUELL SLP.  NGMSLP2 COMPUTES 1000MB GEOPOTENTIAL.
C
            ELSE
!$omp  parallel do
              DO J=JSTA,JEND
              DO I=1,IM
                FSL(I,J)=Z1000(I,J)*G
              ENDDO
              ENDDO
            ENDIF
          ENDIF
C     
C        INTERPOLATE WIND COMPONENTS FROM ETA TO PRESSURE.
C     
          IF((IGET(018).GT.0).OR.(IGET(019).GT.0).OR.
     1       (IGET(021).GT.0).OR.(IGET(085).GT.0))THEN
C
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              USL(I,J)=D00
              VSL(I,J)=D00
            ENDDO
            ENDDO
C
!!$omp  parallel do
!!$omp& private(alpet2,alpetl,alpetu,lmb,petal,petau)
!!$omp& shared (alpet2x,alpetux,nl1x)
          DO 281 J=JSTA,JEND
          DO 281 I=1,IM
CNOTE 
CNOTE         29 JANUARY 1993, RUSS TREADON.
CNOTE          - AS FOR THE OTHER FIELDS WE INTERPOLATE ONLY
CNOTE            BETWEEN THE FAL AND THE MODEL TOP.  BELOW 
CNOTE            SURFACE VALUES ARE FAL VALUES.
C
            LMB = LMV(I,J)
C
              PETAU=PT+PDVP1(I,J)*ETA(1)
              ALPETU=ALOG(PETAU)
            DO 280 IL=2,LMB
              PETAL=PT+PDVP1(I,J)*ETA(IL)
c             PETAU=PT+PDVP1(I,J)*ETA(IL-1)
              ALPETL=ALOG(PETAL)
c             ALPETU=ALOG(PETAU)
              ALPET2=SQRT(0.5E0*(ALPETL*ALPETL+ALPETU*ALPETU))
C     
C          SEARCH FOR HIGHEST MID-LAYER ETA SURFACE (NOT SUBMERGED)
C          THAT IS BELOW THE GIVEN STANDARD PRESSURE LEVEL.
              IF(ALSL(L).LT.ALPET2)THEN
                NL1X(I,J)=IL-1
                ALPETUX(I,J)=ALPETU
                ALPET2X(I,J)=ALPET2
                GO TO 281
              ENDIF
C      If we arent on the last iterate of the 280 loop, reset  PETAU and ALPETU
            if ( il .eq. lmb ) goto 280
            PETAU=PETAL
            ALPETU=ALPETL
  280       CONTINUE
            NL1X(I,J)=LMB+1
            ALPETUX(I,J)=ALPETU
            ALPET2X(I,J)=ALPET2
  281     CONTINUE
C     
C         BELOW GROUND USE FAL WINDS.
C
!$omp  parallel do
!$omp& private(alpet1,alpetl,alpetu,fact,petau)
          DO 290 J=JSTA,JEND
          DO 290 I=1,IM
            IF(NL1X(I,J).GT.LMV(I,J))THEN
              USL(I,J)=U(I,J,LMV(I,J))
              VSL(I,J)=V(I,J,LMV(I,J))
C     
C          IF REQUESTED PRESSURE LEVEL IS NOT BELOW THE LOCAL GROUND
C          THEN WE HAVE TWO POSSIBILITIES.  IF THE REQUESTED PRESSURE
C          LEVEL IS BETWEEN THE LOCAL SURFACE PRESSURE AND TOP OF
C          MODEL PRESSURE, VERTICALLY INTERPOLATE BETWEEN NEAREST
C          BOUNDING ETA LEVELS TO GET THE WIND COMPONENTS.  IF THE   
C          REQUESTED PRESSURE LEVEL IS ABOVE THE MODEL TOP, USE
C          CONSTANT EXTRAPOLATION OF TOP ETA LAYER (L=1) WINDS.
C 
            ELSE
              IF(NL1X(I,J).GT.1)THEN
                ALPETL=ALPETUX(I,J)
                PETAU=PT+PDVP1(I,J)*ETA(NL1X(I,J)-1)
                ALPETU=ALOG(PETAU)
                ALPET1=SQRT(0.5*(ALPETL*ALPETL+ALPETU*ALPETU))
                FACT=(ALPET2X(I,J)-ALSL(L))/(ALPET2X(I,J)-ALPET1)
                USL(I,J)=U(I,J,NL1X(I,J))
     1                 +(U(I,J,NL1X(I,J)-1)-U(I,J,NL1X(I,J)))*FACT
                VSL(I,J)=V(I,J,NL1X(I,J))+(V(I,J,NL1X(I,J)-1)
     1                  -V(I,J,NL1X(I,J)))*FACT
              ELSE
                USL(I,J)=U(I,J,NL1X(I,J))
                VSL(I,J)=V(I,J,NL1X(I,J))
              ENDIF
C     
C            ALPET2 IS MID-LAYER ETA SURFACE JUST BELOW STANDARD PRESSURE
C            LEVEL AND ALPET1 IS DASHED ETA SURFACE JUST ABOVE.
C            NOTE THAT IF THE STANDARD PRESSURE SURFACE IS SUBMERGED, THEN
C            ALPET2 AND ALPET1 ARE THE LOWEST AND 2ND LOWEST MID-LAYER
C            ETA SURFACES ABOVE THE TOPOGRAPHY (WITH OLDRD=.TRUE., ZJ).
C
            ENDIF
  290     CONTINUE
        ENDIF
C
C
C        *** PART II ***
C
C        INTERPOLATE/OUTPUT SELECTED FIELDS.
C
C        GEOPOTENTIAL (SCALE BY GI)
         IF (IGET(012).GT.0) THEN
          IF (LVLS(L,IGET(012)).GT.0) THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J)=FSL(I,J)*GI
            ENDDO
            ENDDO
C
            CALL E2OUT(012,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(012),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        TEMPERATURE.
         IF (IGET(013).GT.0) THEN
          IF (LVLS(L,IGET(013)).GT.0) THEN
            CALL E2OUT(013,000,TSL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(013),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        POTENTIAL TEMPERATURE.
         IF (IGET(014).GT.0) THEN
          IF (LVLS(L,IGET(014)).GT.0) THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=SPL(L)
            ENDDO
            ENDDO
            CALL CALPOT2(EGRID2,TSL,EGRID1,IM,JM)
            CALL E2OUT(014,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(014),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        RELATIVE HUMIDITY.
         IF (IGET(017).GT.0) THEN
          IF (LVLS(L,IGET(017)).GT.0) THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=SPL(L)
            ENDDO
            ENDDO
            CALL CALRH2(EGRID2,TSL,QSL,ICE,EGRID1,IM,JM)
            CALL E2OUT(017,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(017),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        DEWPOINT TEMPERATURE.
         IF (IGET(015).GT.0) THEN
          IF (LVLS(L,IGET(015)).GT.0) THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=SPL(L)
            ENDDO
            ENDDO
            CALL CALDWP2(EGRID2,QSL,EGRID1,TSL)
            CALL E2OUT(015,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(015),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        SPECIFIC HUMIDITY.
         IF (IGET(016).GT.0) THEN
          IF (LVLS(L,IGET(016)).GT.0) THEN
            CALL E2OUT(016,000,QSL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1M12,H99999,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(016),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        OMEGA
         IF (IGET(020).GT.0) THEN
          IF (LVLS(L,IGET(020)).GT.0) THEN
            CALL E2OUT(020,000,OSL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(020),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C           MOISTURE CONVERGENCE.
            IF (IGET(085).GT.0) THEN
             IF (LVLS(L,IGET(085)).GT.0) THEN
               CALL CALMCVG(QSL,USL,VSL,-1,EGRID1)
               CALL E2OUT(085,000,EGRID1,EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
C           CONVERT TO DIVERGENCE FOR GRIB UNITS
               CALL SCLFLD(GRID1,-1.0,IMOUT,JMOUT)
               ID(1:25)=0
               CALL OUTPUT(IOUTYP,IGET(085),L,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C        U AND/OR V WIND.
         IF (IGET(018).GT.0.OR.IGET(019).GT.0) THEN
          IF (LVLS(L,IGET(018)).GT.0.OR.LVLS(L,IGET(019)).GT.0) THEN
            CALL E2OUT(018,019,USL,VSL,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IF (IGET(018).GT.0)then
                 CALL OUTPUT(IOUTYP,IGET(018),L,GRID1,IMOUT,JMOUT)
            endif
            ID(1:25)=0
            IF (IGET(019).GT.0) 
     X           CALL OUTPUT(IOUTYP,IGET(019),L,GRID2,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        ABSOLUTE VORTICITY.
         IF (IGET(021).GT.0) THEN
          IF (LVLS(L,IGET(021)).GT.0) THEN
            CALL CALVOR(USL,VSL,EGRID1)
            CALL E2OUT(021,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(021),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        GEOSTROPHIC STREAMFUNCTION.
         IF (IGET(086).GT.0) THEN
          IF (LVLS(L,IGET(086)).GT.0) THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=FSL(I,J)*GI
            ENDDO
            ENDDO
            CALL CALSTRM(EGRID2,EGRID1)
            CALL E2OUT(086,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(086),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        TURBULENT KINETIC ENERGY.
         IF (IGET(022).GT.0) THEN
          IF (LVLS(L,IGET(022)).GT.0) THEN
            CALL E2OUT(022,000,Q2SL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(022),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C     
C        TOTAL CLOUD WATER.
         IF (IGET(153).GT.0) THEN
          IF (LVLS(L,IGET(153)).GT.0) THEN
            CALL E2OUT(153,000,QCSL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1M12,H99999,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(153),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF
C
C        TOTAL CLOUD ICE 
         IF (IGET(166).GT.0) THEN
          IF (LVLS(L,IGET(166)).GT.0) THEN
            CALL E2OUT(166,000,ICE,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            CALL BOUND(GRID1,H1M12,H99999,IMOUT,JMOUT)
            ID(1:25)=0
            CALL OUTPUT(IOUTYP,IGET(166),L,GRID1,IMOUT,JMOUT)
          ENDIF
         ENDIF

C     
C     END OF MAIN VERTICAL LOOP.
C     
 310  CONTINUE
      IOALL=.TRUE.
C
C     ENDIF FOR IF TEST SEEING IF WE WANT ANY OTHER VARIABLES
      ENDIF
C     
C     END OF ROUTINE.
C
      RETURN
      END
