      SUBROUTINE NGMFLD2(RH4710,RH4796,RH1847,RH8498,QM8510)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    NGMFLD2     COMPUTES LAYER MEAN NGM FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES A HANDFUL OF NGM LAYER MEAN 
C     FIELDS.  THIS IS DONE TO PROVIDE A FULLY COMPLETE 
C     ETA NGM LOOK-ALIKE OUTPUT FILE.  THE SIGMA (LAYER)
C     FIELDS COMPUTED BY THIS ROUTINE ARE TABULATED BELOW.
C     
C           SIGMA (LAYER)         FIELD(S)
C          ---------------     --------------
C          0.47191-1.00000          RH
C          0.47171-0.96470          RH
C          0.18019-0.47191          RH
C          0.84368-0.98230          RH
C          0.85000-1.00000         MCONV
C     WHERE 
C          RH    = RELATIVE HUMIDITY
C          MCONV = MOISTURE CONVERGENCE
C
C     LAYER MEANS ARE A SUMMATION OVER ETA LAYERS MAPPING INTO
C     THE PRESSURE RANGE CORRESPONDING TO THE SIGMA RANGE ABOVE.
C     THE CALCULATION OF THESE BOUNDING PRESSURES IS DONE AT 
C     EACH HORIZONTAL GRID POINT BASED ON THE SURFACE PRESSURE.
C     EACH TERM IN THE SUMMATION IS WEIGHTED BY THE THICKNESS OF
C     THE ETA LAYER.  THE FINAL LAYER MEAN IS THIS SUM NORMALIZED
C     BY THE TOTAL DEPTH OF THE LAYER.

C
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   93-07-27  RUSS TREADON - MODIFIED SUMMATION LIMITS FROM
C                            0.66*PSFC TO 0.75*PSFC AND 0.33*PSFC 
C                            TO 0.50*PSFC, WHERE PSFC IS THE
C                            SURFACES PRESSURE.  THE REASON FOR
C                            THIS CHANGE WAS RECOGNITION THAT IN 
C                            THE LFM 0.33 AND 0.66 WERE MEASURED
C                            FROM THE SURFACE TO THE TROPOPAUSE,
C                            NOT THE TOP OF THE MODEL.
C   93-09-13  RUSS TREADON - RH CALCULATIONS WERE MADE INTERNAL
C                            TO THE ROUTINE.
C   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-18  MIKE BALDWIN - COMPUTE RH OVER ICE
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C     
C USAGE:    CALL NGMFLD2(RH4710,RH4796,RH1847,RH8498,QM8510)
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST: 
C     RH4710   - SIGMA LAYER 0.47-1.00 MEAN RELATIVE HUMIDITY.
C     RH4796   - SIGMA LAYER 0.47-0.96 MEAN RELATIVE HUMIDITY.
C     RH1847   - SIGMA LAYER 0.18-0.47 MEAN RELATIVE HUMIDITY.
C     RH8498   - SIGMA LAYER 0.84-0.98 MEAN RELATIVE HUMIDITY.
C     QM8510   - SIGMA LAYER 0.85-1.00 MEAN MOISTURE CONVERGENCE.
C     
C   OUTPUT FILES:
C     NONE
C     
C   LIBRARY:
C     COMMON   - VRBLS
C                MASKS
C                EXTRA
C                OPTIONS
C                LOOPS
C                MAPOT
C                DYNAMD
C                INDX
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE PARAMETERS
      INCLUDE "parmeta"
      INCLUDE "params"
C
      PARAMETER (SIG100=1.00000, SIG98=0.98230, SIG96=0.96470)
      PARAMETER (SIG89 =0.89671, SIG85=0.85000, SIG84=0.84368)
      PARAMETER (SIG78 =0.78483, SIG47=0.47191, SIG18=0.18018)
      PARAMETER (SMALL = 1.E-6)
C     
C     DECLARE VARIABLES.
      LOGICAL GOT8510,GOT4710,GOT4796,GOT1847,GOT8498
      LOGICAL OLDRD, STDRD
      REAL QM8510(IM,JM),RH4710(IM,JM),RH8498(IM,JM)
      REAL RH4796(IM,JM),RH1847(IM,JM),IWL(IM,JM),IWM1
      REAL Z8510(IM,JM),Z4710(IM,JM),Z8498(IM,JM)
      REAL Z4796(IM,JM),Z1847(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "VRBLS.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C
C********************************************************************
C     START NGMFLD HERE.
C     
      CLIMIT =1.0E-20
C
C     INITIALIZE ARRAYS.
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
         QM8510(I,J) = D00
         RH4710(I,J) = D00
         RH8498(I,J) = D00
         RH4796(I,J) = D00
         RH1847(I,J) = D00
         Z8510(I,J)  = D00
         Z8498(I,J)  = D00
         Z4710(I,J)  = D00
         Z4796(I,J)  = D00
         Z1847(I,J)  = D00
         IWL  (I,J)  = D00
      ENDDO
      ENDDO
C     
C     LOOP OVER HORIZONTAL GRID.
C     
!$omp  parallel do
!$omp& private(ai,bi,dz,ie,iw,iwl,iwm1,p100,p18,p47,p84,p85,
!$omp&         p96,p98,pm,qdiv,qi,qint,qk,qkhn,qkhs,qkm1,qm,qm8510,
!$omp&         qmcvg,qs,qudx,qvdy,qw,r2dx,r2dy,rh,rh1847,rh4710,
!$omp&         rh4796,rh8498,tm,tmt0,tmt15,z1847,z4710,z4796,
!$omp&         z8498,z8510)
      DO L=1,LM
      CALL EXCH2(Q(1,1,L))
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
C
C        SET TARGET PRESSURES.
         P100  = SIG100*(PD(I,J)+PT)
         P98   = SIG98*(PD(I,J)+PT)
         P96   = SIG96*(PD(I,J)+PT)
         P85   = SIG85*(PD(I,J)+PT)
         P84   = SIG84*(PD(I,J)+PT)
         P47   = SIG47*(PD(I,J)+PT)
         P18   = SIG18*(PD(I,J)+PT)
C     
C     
C        COMPUTE LAYER MEAN FIELDS AT THE GIVEN K.
C
C          COMPUTE P, Z, T, AND Q AT THE MIDPOINT OF THE CURRENT ETA LAYER.
           ALPM = D50*(ALPINT(I,J,L)+ALPINT(I,J,L+1))
           DZ   = ZINT(I,J,L)-ZINT(I,J,L+1)
           PM   = EXP(ALPM)
           TM   = T(I,J,L)
           QM   = Q(I,J,L)
           QM   = AMAX1(QM,H1M12)
C
           IWM1=IWL(I,J)
           IF(CWM(I,J,L).GT.CLIMIT) THEN
             IF(TM.LT.258.15)THEN
               IWL(I,J)=1.
             ELSEIF(TM.GE.273.15)THEN
               IWL(I,J)=0.
             ELSE
               IF(IWM1.EQ.1.0)IWL(I,J)=1.
             ENDIF
           ELSE
             IWL(I,J)=0.
           ENDIF
C
C     
C          COMPUTE RELATIVE HUMIDITY.
C
           TMT0=TM-273.16
           TMT15=AMIN1(TMT0,-15.)
           AI=0.008855
           BI=1.
           IF(TMT0.LT.-20.)THEN
             AI=0.007225
             BI=0.9674
           ENDIF
           QW=PQ0/PM
     1          *EXP(A2*(TM-A3)/(TM-A4))
           QI=QW*(BI+AI*AMIN1(TMT0,0.))
           QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
           IF(TMT0.LT.-15.)THEN
               QS=QI
           ELSEIF(TMT0.GE.0.)THEN
               QS=QINT
           ELSE
             IF(IWL(I,J).GT.0.0) THEN
               QS=QI
             ELSE
               QS=QINT
             ENDIF
           ENDIF
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT
C             DELETE THIS LINE TO SWITCH BACK TO RH VS ICE
            QS=QW
CMEB 12/22/98 SWITCH TO RH VS WATER NO MATTER WHAT

C
           RH   = QM/QS
           IF (RH.GT.H1) THEN
              RH = H1
              QM = RH*QS
           ENDIF
           IF (RH.LT.D01) THEN
              RH = D01
              QM = RH*QS
           ENDIF
C
C          COMPUTE MOISTURE CONVERGENCE.  WE NEED Q AT V POINTS.
           QMCVG = D00
           IE=I+IHE(J)
           IW=I+IHW(J)
           QK=D25*(Q(I+1,J,L)+Q(I,J,L)+Q(IE,J+1,L)+Q(IE,J-1,L))
           QKM1=D25*(Q(I,J,L)+Q(I-1,J,L)+Q(IW,J+1,L)+Q(IW,J-1,L))
           QKHN=D25*(Q(IW,J+1,L)+Q(IE,J+1,L)+Q(I,J+2,L)+Q(I,J,L))
           QKHS=D25*(Q(IW,J-1,L)+Q(IE,J-1,L)+Q(I,J,L)+Q(I,J-2,L))
           R2DX =1./(2.*DX(I,J))
           R2DY =1./(2.*DY)
           QUDX =(QK*U(IE,J,L)-QKM1*U(IW,J,L))*R2DX
           QVDY =(QKHN*V(I,J+1,L)-QKHS*V(I,J-1,L))*R2DY
           QDIV =QUDX+QVDY
           QMCVG=-1.*QDIV*HBM2(I,J)
C     
C          SIGMA 0.85-1.00 MOISTURE CONVERGENCE.
           IF ((PM.LE.P100).AND.(PM.GE.P85)) THEN
              Z8510(I,J)  = Z8510(I,J) + DZ
              QM8510(I,J) = QM8510(I,J) + QMCVG*DZ
           ENDIF
C    
C          SIGMA 0.47-1.00 RELATIVE HUMIDITY.
           IF ((PM.LE.P100).AND.(PM.GE.P47)) THEN
              Z4710(I,J)  = Z4710(I,J) + DZ
              RH4710(I,J) = RH4710(I,J) + RH*DZ
           ENDIF
C
C          SIGMA 0.84-0.98 RELATIVE HUMIDITY.
           IF ((PM.LE.P98).AND.(PM.GE.P84)) THEN
              Z8498(I,J)  = Z8498(I,J) + DZ
              RH8498(I,J) = RH8498(I,J) + RH*DZ
           ENDIF
C     
C          SIGMA 0.47-0.96 RELATIVE HUMIDITY.
           IF ((PM.LE.P96).AND.(PM.GE.P47)) THEN
              Z4796(I,J)  = Z4796(I,J) + DZ
              RH4796(I,J) = RH4796(I,J) + RH*DZ
           ENDIF
C     
C          SIGMA 0.18-0.47 RELATIVE HUMIDITY.
           IF ((PM.LE.P47).AND.(PM.GE.P18)) THEN
              Z1847(I,J)  = Z1847(I,J) + DZ
              RH1847(I,J) = RH1847(I,J) + RH*DZ
           ENDIF
C
      ENDDO
      ENDDO
      ENDDO
C     
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
C        NORMALIZE TO GET LAYER MEAN VALUES.
         IF (Z8510(I,J).GT.0) THEN
            QM8510(I,J) = QM8510(I,J)/Z8510(I,J)
         ELSE
            QM8510(I,J) = SPVAL
         ENDIF
         IF (ABS(QM8510(I,J)-SPVAL).LT.SMALL)QM8510(I,J)=H1M12
C
         IF (Z4710(I,J).GT.0) THEN
            RH4710(I,J) = RH4710(I,J)/Z4710(I,J)
         ELSE
            RH4710(I,J) = SPVAL
         ENDIF
C
         IF (Z8498(I,J).GT.0) THEN
            RH8498(I,J) = RH8498(I,J)/Z8498(I,J)
         ELSE
            RH8498(I,J) = SPVAL
         ENDIF
C
         IF (Z4796(I,J).GT.0) THEN
            RH4796(I,J) = RH4796(I,J)/Z4796(I,J)
         ELSE
            RH4796(I,J) = SPVAL
         ENDIF
C
         IF (Z1847(I,J).GT.0) THEN
            RH1847(I,J) = RH1847(I,J)/Z1847(I,J)
         ELSE
            RH1847(I,J) = SPVAL
         ENDIF
      ENDDO
      ENDDO
C
C     
C     END OF ROUTINE.
C     
      RETURN
      END

