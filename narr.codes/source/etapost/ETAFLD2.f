      SUBROUTINE ETAFLD2(IMOUT,JMOUT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    ETAFLD2     SLP AND ETA LEVEL POSTING
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-21       
C     
C ABSTRACT:
C     THIS ROUTINE DOES SEVERAL THINGS.  IT IS THE FIRST 
C     ROUTINE CALLED BY POST PROCESSOR SUBROUTINE PROCESS 
C     WHICH SETS THE ORDER IN WHICH FIELDS ARE POSTED.  THE
C     INTERNAL POST VERSION OF ETAFLD2 FIRST LOADS FUNDAMENTAL
C     MODEL VARIABLES (T,Q,U,V) INTO POST-ONLY ARRAYS. 
C     NEGATIVE SPECIFIC HUMIDITY IS CLIPPED.  NEXT PRESSURE AND
C     LOG PRESSURE ARE COMPUTED AT ALL MASS GRID POINTS.  THE
C     MODEL ITSELF COMPUTES THE MESINGER SEA LEVEL PRESSURE 
C     PRIOR TO POSTING THE PROFILE DATA.  THIS ROUTINE WILL
C     COMPUTE THE STANDARD NMC SEA LEVEL PRESSURE IF THIS OPTION
C     IS ACTIVATED.  BY EITHER METHOD WE COMPUTE BELOW SURFACE
C     TEMPERATURES.  AFTER COMPUTING OMEGA ON THE ETA LEVELS,
C     WE SET BELOW SURFACE FIELDS ON ETA LEVELS.  USING THE 
C     HYDROSTATIC EQUATION WE COMPUTE THE HEIGHT AT ETA LAYER
C     INTERFACES.  FINALLY WE COMPUTE/POST REQUESTED FIELDS ON
C     ETA LAYERS.
C
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-21  RUSS TREADON
C   93-09-01  RUSS TREADON - ADDED ADDITIONAL OUTPUT FIELDS.
C   96-03-20  MIKE BALDWIN - ADDED CLOUD TOP TEMPS, CHANGE CLOUD WATER
C                            TO CONTAIN WATER ONLY
C   97-04-29  GEOFF MANIKIN - MOVED CLOUD TOP TEMPS TO CLDRAD
C   98-06-01  T BLACK - CONVERSION FROM 1-D TO 2-D
C   98-07-20  MIKE BALDWIN - REMOVED LABL84
C   98-08-18  T BLACK - REMOVED EXCESS SPACE IN EXTRA.com
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   03-01-22  H CHUANG & M EK - ADDED PBL HEIGHT CALCULATION
C     
C USAGE:    CALL ETAFLD2(IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       BOUND    - BOUND ARRAY ELEMENTS BETWEEN LOWER AND UPPER LIMITS.
C       E2OUT    - INTERPOLATE/SMOOTH E-GRID TO OUTPUT GRID.
C       SCLFLD   - SCALE ARRAY ELEMENTS BY SCALAR CONSTANT.
C       OUTPUT   - POST DATA TO OUTPUT GRID IN SPECIFIED FORMAT.
C       NGMSLP2  - COMPUTE SLP USING STANDARD NMC REDUCTION METHOD.
C       BLOSFC2  - SET BELOW SURFACE ETA LEVEL DATA.
C       NETAL    - EXTRACT DATA ON CONSTANT ETA LAYER OR CONSTANT
C                     ATMOSPHERIC ETA LAYER.
C       CALPOT2  - COMPUTE POTENTIAL TEMPERATURE.
C       CALRH2   - COMPUTE RELATIVE HUMIDITY.
C       CALDWP2  - COMPUTE DEWPOINT TEMPERATURE.
C       CALMCVG  - COMPUTE MOISTURE CONVERGENCE.
C       CALVOR   - COMPUTE ABSOLUTE VORTICITY.
C       CALSTRM  - COMPUTE GEOSTROPHIC STREAMFUNCTION.
C     LIBRARY:
C       COMMON   - VRBLS
C                  PVRBLS
C                  EXTRA
C                  MASKS
C                  MAPOT
C                  DYNAMD
C                  OMGAOT
C                  RQSTFLD
C                  CTLBLK
C                  LOOPS
C                  ACMCLH
C                  ACMRDL
C                  ACMRDS
C                  CLDWTR
C                  IOUNIT
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C     
C     INCLUDE ETA MODEL DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "params"
      INCLUDE "parm.tbl"
      parameter(CPBLT=10.)
C
      PARAMETER (RAINCON=1.1787E4)
      PARAMETER (SNOCON=1.4594E5)
      PARAMETER (VCON1=1.66476,VCON2=0.55683)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STDRD
      LOGICAL NORTH,NEED(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM),EGRID3(IM,JM),EGRID4(IM,JM)
      REAL HGT(IM,JM),EL0(IM,JM),FI(IM,JM,2)
      REAL P1D(IM,JM),T1D(IM,JM),Q1D(IM,JM),EGRID5(IM,JM)
      REAL GRID1(IMOUT,JMOUT), GRID2(IMOUT,JMOUT)
      REAL PMID(IM,JM,LM),ZMID(IM,JM,LM),IW(IM,JM,LM)
      REAL EL(IM,JM,LM),RICHNO(IM,JM,LM)
      REAL QI(IM,JM),QINT(IM,JM)
      REAL TT(IM,JM),PPP(IM,JM),QV(IM,JM),QCD(IM,JM),QICE(IM,JM)
      REAL QRAIN(IM,JM),QSNO(IM,JM),VIS(IM,JM)
      REAL HPBL(IM,JM)
C     
C     INCLUDE REQUIRED COMMONS.
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "OMGAOT.comm"
      INCLUDE "RQSTFLD.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "OUTFIL.comm"
      INCLUDE "LOOPS.comm"
C
      REAL TTND(IM,JM),TRAIN(IM,JM),TCUCN(IM,JM)
C     
C     DECLARE EQUIVALENCES.
      EQUIVALENCE (EGRID1(1,1),P1D(1,1),EL0(1,1))
      EQUIVALENCE (EGRID2(1,1),T1D(1,1),HGT(1,1))
      EQUIVALENCE (PMID(1,1,1),EL(1,1,1))
      EQUIVALENCE (ZMID(1,1,1),RICHNO(1,1,1))
C     
C*****************************************************************************
C     START SUBROUTINE ETAFLD.
C     
      print *, " CWM start of ETAFLD"
      do l=1,lm
      do j=jm,1,-1
!      write(*,"(10E12.7)") (CWM(I,J,L),I=1,IM)
      end do
      end do
      print *, " CWM end of ETAFLD"
C
C  SET UP UTIM FOR THIS TIME STEP
C
      CTIM1=0.
      CTIM2=24.*3600.
      CTIM =NTSD*DT
      IF(CTIM.LT.CTIM1)THEN
        UTIM=0.
      ELSE
        IF(CTIM.LE.CTIM2)THEN
          UTIM=(CTIM-CTIM1)/(CTIM2-CTIM1)
        ELSE
          UTIM=1.
        ENDIF
      ENDIF
C     SET TOTAL NUMBER OF OUTPUT GRID POINTS.
C
C     FROM THE ETA MODEL WE COMPUTE SEA LEVEL PRESSURE USING
C     MESINGER'S ALGORITHM.  THIS CREATES AN UNDERGROUND
C     TEMPERATURE FIELD.  SUBROUTINE ETA2P MAKES USE OF THIS
C     UNDERGROUND TEMPERATURE FIELD IN COMPUTING GEOPOTENTIAL
C     BELOW THE GROUND.  IF THE USER WANTS SEA LEVEL PRESSURE
C     VIA THE SHUELL SCHEME, MAKE IT SO.  THIS SCHEME CREATES
C     ITS OWN UNDERGROUND TEMPERATURES WHICH ETA2P WILL USE.
C     THE ROUTINE ALSO COMPUTES ITS OWN 1000MB GEOPOTENTIAL.
C     
C     OUTPUT SEA LEVEL PRESSURE IF REQUESTED.
C     FIRST, MESINGER'S SEA LEVEL PRESSURE.
      IF (IGET(023).GT.0) THEN
         CALL E2OUT(023,000,PSLP,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(023),LVLS(1,IGET(023)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     SECOND, STANDARD NGM SEA LEVEL PRESSURE.
      IF (IGET(105).GT.0) THEN
         CALL NGMSLP2
         CALL E2OUT(105,000,SLP,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(105),LVLS(1,IGET(105)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     SET BELOW GROUND Q, U, V, AND OMEGA
C     
      CALL BLOSFC2
C     
C     COMPUTE HEIGHT AT INTERFACES.
C     SET SURFACE VALUES.
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        ZINT(I,J,LP1)=FIS(I,J)*GI
        FI(I,J,1)=FIS(I,J)
      ENDDO
      ENDDO
C
C     COMPUTE VALUES FROM THE SURFACE UP.
C
      DO 80 L=LM,1,-1
!$omp  parallel do
        DO J=JSTA,JEND
        DO I=1,IM
          FI(I,J,2)=HTM(I,J,L)*T(I,J,L)*(Q(I,J,L)*D608+H1)*R*
     1             (ALPINT(I,J,L+1)-ALPINT(I,J,L))+FI(I,J,1)
          ZINT(I,J,L)=FI(I,J,2)*GI
          FI(I,J,1)=FI(I,J,2)
        ENDDO
        ENDDO
   80 CONTINUE
C
C     COMPUTE VALUES FROM THE SURFACE BELOW.
      KMM=KMNTM(LM)
!$omp  parallel do
!$omp& private(i,iadd,j,k,lftov1,lmap1,ndrow)
      DO 100 KM=1,KMM
      K=KMNT(KM,LM)
      NDROW=K/IMT
      LFTOV1=MOD(K,IMT)
      IF(LFTOV1-IM.GT.0)THEN
        I=K-NDROW*IMT-IM
        IADD=2
      ELSEIF(LFTOV1.GT.0)THEN
        I=K-NDROW*IMT
        IADD=1
      ELSEIF(LFTOV1.EQ.0)THEN
        I=IM-1
        IADD=0
      ENDIF
      J=2*NDROW+IADD
C     IF ( J .GE. JSTA .AND. J .LE. JEND ) THEN
C
         LMAP1=LMH(I,J)+1
         DO L=LMAP1,LM
            ZINT(I,J,L+1)=DFL(L+1)*GI
         END DO 
C     END IF
  100 CONTINUE
C     
C     COMPUTE MIDLAYER HEIGHTS AND PRESSURES.
!$omp  parallel do
      DO 110 L = 1,LM
      DO J=JSTA,JEND
      DO I=1,IM
        PMID(I,J,L)=D50*(PINT(I,J,L+1)+PINT(I,J,L))
        ZMID(I,J,L)=D50*(ZINT(I,J,L+1)+ZINT(I,J,L))
      ENDDO
      ENDDO
  110 CONTINUE
C     
C     OUTPUT/CALCULATE PRESSURE, OMEGA, POTENTIAL TEMPERATURE,
C     DEWPOINT TEMPERATURE, RELATIVE HUMIDITY, AND 
C     ABSOLUTE VORTICITY ON ETA SURFACES.
C     
      NREC0=1
C
      IF ( (IGET(001).GT.0).OR.(IGET(077).GT.0).OR.
     X     (IGET(002).GT.0).OR.(IGET(003).GT.0).OR.
     X     (IGET(004).GT.0).OR.(IGET(005).GT.0).OR.
     X     (IGET(006).GT.0).OR.(IGET(083).GT.0).OR.
     X     (IGET(007).GT.0).OR.(IGET(008).GT.0).OR.
     X     (IGET(009).GT.0).OR.(IGET(010).GT.0).OR.
     X     (IGET(084).GT.0).OR.(IGET(011).GT.0).OR.
     X     (IGET(041).GT.0).OR.(IGET(124).GT.0).OR.
     X     (IGET(125).GT.0).OR.(IGET(145).GT.0).OR.
     X     (IGET(078).GT.0).OR.(IGET(079).GT.0).OR.
     X     (IGET(140).GT.0).OR.(IGET(040).GT.0).OR.
     X     (IGET(180).GT.0) ) THEN
C

C   IF ANY OF THE CLOUD ARRAYS ARE REQUESTED, COMPUTE ICE/WATER
C   ALSO NEED THIS IF VISIBILITY OR RH IS REQUESTED

      IF(IGET(124).GT.0.OR.IGET(125).GT.0.OR.IGET(145).GT.0
     X            .OR.IGET(006).GT.0.OR.IGET(180).GT.0)THEN
        CLIMIT =1.0E-20

        DO J=JSTA,JEND
        DO I=1,IM
          IW(I,J,1)=0.
        ENDDO
        ENDDO
C
      DO 125 L=2,LM 
      DO J=JSTA,JEND
      DO I=1,IM
        LML=LM-LMH(I,J)
        HH=HTM(I,J,L)*HBM2(I,J)
        TKL=T(I,J,L)
        QKL=Q(I,J,L)
        CWMKL=CWM(I,J,L)
        TMT0=(TKL-273.15)*HH
        TMT15=AMIN1(TMT0,-15.)*HH
        PP=PDSL(I,J)*AETA(L)+PT
        QW=HH*PQ0/PP*EXP(HH*A2*(TKL-A3)/(TKL-A4))
        QI(I,J)=QW*(1.+0.01*AMIN1(TMT0,0.))
C
        U00KL=U00(I,J)+UL(L+LML)*(0.95-U00(I,J))*UTIM
        IF(TMT0.LT.-15.0)THEN
          FIQ=QKL-U00KL*QI(I,J)
          IF(FIQ.GT.D00.OR.CWMKL.GT.CLIMIT) THEN
            IW(I,J,L)=1.
          ELSE
            IW(I,J,L)=0.
          ENDIF
        ENDIF
        IF(TMT0.GE.0.0)IW(I,J,L)=0.
        IF(TMT0.LT.0.0.AND.TMT0.GE.-15.0)THEN
          IW(I,J,L)=0.
          IF(IW(I,J,L-1).EQ.1.0.AND.CWMKL.GT.CLIMIT)IW(I,J,L)=1.
        ENDIF
      ENDDO
      ENDDO
  125 CONTINUE
      ENDIF
C
      DO 190 L=1,LM
C     
C           PRESSURE ON ETA SURFACES.
            IF (IGET(001).GT.0) THEN
             IF (LVLS(L,IGET(001)).GT.0) THEN
               ITYPE = LVLS(L,IGET(001))
               CALL NETAL(PMID,ITYPE,L,LMH,EGRID1)
               CALL E2OUT(001,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(001),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           HEIGHTS ON ETA SURFACES.
            IF (IGET(077).GT.0) THEN
             IF (LVLS(L,IGET(077)).GT.0) THEN
               ITYPE = LVLS(L,IGET(077))
               CALL NETAL(ZMID,ITYPE,L,LMH,EGRID1)
               CALL E2OUT(077,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(077),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           TEMPERATURE ON ETA SURFACES.
            IF (IGET(002).GT.0) THEN
             IF (LVLS(L,IGET(002)).GT.0) THEN
               ITYPE = LVLS(L,IGET(002))
               CALL NETAL(T,ITYPE,L,LMH,EGRID1)
               CALL E2OUT(002,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(002),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           POTENTIAL TEMPERATURE ON ETA SURFACES.
            IF (IGET(003).GT.0) THEN
             IF (LVLS(L,IGET(003)).GT.0) THEN
               ITYPE = LVLS(L,IGET(003))
               CALL NETAL(PMID,ITYPE,L,LMH,P1D)
               CALL NETAL(T,ITYPE,L,LMH,T1D)
               CALL CALPOT2(P1D,T1D,EGRID3,IM,JM)
               CALL E2OUT(003,000,EGRID3,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(003),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           RELATIVE HUMIDITY ON ETA SURFACES.
            IF (IGET(006).GT.0) THEN
             IF (LVLS(L,IGET(006)).GT.0) THEN
               ITYPE = LVLS(L,IGET(006))
               CALL NETAL(PMID,ITYPE,L,LMH,P1D)
               CALL NETAL(T,ITYPE,L,LMH,T1D)
               CALL NETAL(Q,ITYPE,L,LMH,Q1D)
               CALL NETAL(IW,ITYPE,L,LMH,EGRID3)
               CALL CALRH2(P1D,T1D,Q1D,EGRID3,EGRID4,IM,JM)
               CALL E2OUT(006,000,EGRID4,EGRID2,
     X              GRID1,GRID2,IMOUT,JMOUT)
               CALL SCLFLD(GRID1,H100,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1,H100,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(006),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           DEWPOINT ON ETA SURFACES.
            IF (IGET(004).GT.0) THEN
             IF (LVLS(L,IGET(004)).GT.0) THEN
               ITYPE = LVLS(L,IGET(004))
               CALL NETAL(PMID,ITYPE,L,LMH,P1D)
               CALL NETAL(Q,ITYPE,L,LMH,Q1D)
               CALL NETAL(T,ITYPE,L,LMH,T1D)
               CALL CALDWP2(P1D,Q1D,EGRID3,T1D)
               CALL E2OUT(004,000,EGRID3,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(004),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           SPECIFIC HUMIDITY ON ETA SURFACES.
            IF (IGET(005).GT.0) THEN
             IF (LVLS(L,IGET(005)).GT.0) THEN
               ITYPE = LVLS(L,IGET(005))
               CALL NETAL(Q,ITYPE,L,LMH,EGRID1)
               CALL E2OUT(005,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               CALL BOUND(GRID1,H1M12,H99999,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(005),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           MOISTURE CONVERGENCE ON ETA SURFACES.
            IF (IGET(083).GT.0) THEN
             IF (LVLS(L,IGET(083)).GT.0) THEN
               ITYPE = LVLS(L,IGET(083))
               CALL NETAL(Q,ITYPE,L,LMH,Q1D)
               CALL NETAL(U,ITYPE,L,LMV,EGRID1)
               CALL NETAL(V,ITYPE,L,LMV,EGRID2)
               CALL CALMCVG(Q1D,EGRID1,EGRID2,-1,EGRID3)
               CALL E2OUT(083,000,EGRID3,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(083),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           U AND/OR V WIND ON ETA SURFACES.

            IF (IGET(007).GT.0.OR.IGET(008).GT.0) THEN
             IF (LVLS(L,IGET(007)).GT.0.OR.LVLS(L,IGET(008)).GT.0) THEN
               ITYPE = LVLS(L,IGET(007))
               CALL NETAL(U,ITYPE,L,LMV,EGRID1)
               ITYPE = LVLS(L,IGET(008))
               CALL NETAL(V,ITYPE,L,LMV,EGRID2)
               CALL E2OUT(007,008,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               IF (IGET(007).GT.0)
     X              CALL OUTPUT(IOUTYP,IGET(007),LYR,GRID1,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               IF (IGET(008).GT.0)
     X              CALL OUTPUT(IOUTYP,IGET(008),LYR,GRID2,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           OMEGA ON ETA SURFACES.
            IF (IGET(009).GT.0) THEN
             IF (LVLS(L,IGET(009)).GT.0) THEN
               ITYPE = LVLS(L,IGET(009))
               CALL NETAL(OMGA,ITYPE,L,LMH,EGRID1)
               CALL E2OUT(009,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(009),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           ABSOLUTE VORTICITY ON ETA SURFACES.
            IF (IGET(010).GT.0) THEN
             IF (LVLS(L,IGET(010)).GT.0) THEN
               ITYPE = LVLS(L,IGET(010))
               CALL NETAL(U,ITYPE,L,LMV,EGRID1)
               CALL NETAL(V,ITYPE,L,LMV,EGRID2)
               CALL CALVOR(EGRID1,EGRID2,EGRID3)
               CALL E2OUT(010,000,EGRID3,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(010),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           GEOSTROPHIC STREAMFUNCTION ON ETA SURFACES.
            IF (IGET(084).GT.0) THEN
             IF (LVLS(L,IGET(084)).GT.0) THEN
               ITYPE = LVLS(L,IGET(084))
               CALL NETAL(ZMID,ITYPE,L,LMH,EGRID1)
               CALL CALSTRM(EGRID1,EGRID2)
               CALL E2OUT(084,000,EGRID2,EGRID3,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(084),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           TURBULENT KINETIC ENERGY ON ETA SURFACES.
            IF (IGET(011).GT.0) THEN
             IF (LVLS(L,IGET(011)).GT.0) THEN
               ITYPE = LVLS(L,IGET(011))
               CALL NETAL(Q2,ITYPE,L,LMH,EGRID1)
               CALL E2OUT(011,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(011),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C    
C           CLOUD WATER CONTENT
            IF (IGET(124).GT.0) THEN
             IF (LVLS(L,IGET(124)).GT.0) THEN
              DO J=JSTA,JEND
              DO I=1,IM
                IF(CWM(I,J,L).LT.0..AND.CWM(I,J,L).GT.-1.E-10)
     1            CWM(I,J,L)=0.
              ENDDO
              ENDDO
C
              ITYPE = LVLS(L,IGET(124))
              CALL NETAL(CWM,ITYPE,L,LMH,EGRID1)
              CALL NETAL(IW,ITYPE,L,LMH,EGRID2)
C
              DO J=JSTA,JEND
              DO I=1,IM
                EGRID1(I,J)=EGRID1(I,J)*(1.-EGRID2(I,J))
              ENDDO
              ENDDO
C
              CALL E2OUT(124,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
              ID(1:25) = 0
              LYR = L
              IF (ITYPE.GT.1) ID(9) = 109
              CALL OUTPUT(IOUTYP,IGET(124),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           CLOUD ICE CONTENT.
            IF (IGET(125).GT.0) THEN
             IF (LVLS(L,IGET(125)).GT.0) THEN
               ITYPE = LVLS(L,IGET(125))
               CALL NETAL(CWM,ITYPE,L,LMH,EGRID1)
               CALL NETAL(IW,ITYPE,L,LMH,EGRID2)
C
               DO J=JSTA,JEND
               DO I=1,IM
                 EGRID1(I,J)=EGRID1(I,J)*EGRID2(I,J)
               ENDDO
               ENDDO
C
               CALL E2OUT(125,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL OUTPUT(IOUTYP,IGET(125),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           CLOUD FRACTION
C     
            IF (IGET(145).GT.0) THEN
             IF (LVLS(L,IGET(145)).GT.0) THEN
               ITYPE = LVLS(L,IGET(145))
               US=H1                                                           
               CCLIMIT=1.0E-3                                                  
               CLIMIT =1.0E-20                                              
C
               IF(L.EQ.1)THEN
!$omp  parallel do
                 DO J=JSTA,JEND
                 DO I=1,IM
                   EGRID1(I,J)=0.
                 ENDDO
                 ENDDO
                 GO TO 180
               ENDIF
C
!$omp  parallel do
!$omp& private(cwmkl,fiw,hh,lml,pp,qc,qkl,qw,rqkl,tkl,tmt0,
!$omp&         tmt15,u00kl)
               DO J=JSTA,JEND
               DO I=1,IM
                 LML=LM-LMH(I,J)
                 HH=HTM(I,J,L)*HBM2(I,J)
                 TKL=T(I,J,L)    
                 QKL=Q(I,J,L)            
                 CWMKL=CWM(I,J,L)       
                 TMT0=(TKL-273.15)*HH
                 TMT15=AMIN1(TMT0,-15.)*HH     
                 PP=PDSL(I,J)*AETA(L)+PT        
                 QW =HH*PQ0/PP*EXP(HH*A2*(TKL-A3)/(TKL-A4))
                 QI(I,J)=QW *(1.+0.01*AMIN1(TMT0,0.))      
                 QINT(I,J)=QW *(1.-0.0004*TMT15*(TMT15+15.))
                 IF(TMT0.LE.-40.)QINT(I,J)=QI(I,J)           
C
                 U00KL=U00(I,J)+UL(L+LML)*(0.95-U00(I,J))*UTIM
C     -----------THE SATUATION SPECIFIC HUMIDITY---------
                 FIW=IW(I,J,L)  
                 QC =(H1-FIW)*QINT(I,J)+FIW*QI(I,J)   
C     -----------THE RELATIVE HUMIDITY-------------------
                 IF(QC.LE.D00) THEN 
                   RQKL=D00
                 ELSE     
                   RQKL=QKL/QC 
                 ENDIF         
C     -----------CLOUD COVER RATIO (EGRID1)--------------
                 IF(RQKL.GE.0.9999) THEN  
                   EGRID1(I,J)=AMIN1(1.0,RQKL) 
                 ELSE                                                      
                   ARG=-1000.0*CWMKL/(1.-RQKL)
                   ARG=AMAX1(ARG,-12.)
                   EGRID1(I,J)=RQKL*(1.-EXP(ARG))
                 ENDIF      
C
               ENDDO
               ENDDO
  180          CONTINUE
C
               CALL E2OUT(145,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               ID(1:25) = 0
               LYR = L
               IF (ITYPE.GT.1) ID(9) = 109
               CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
               CALL OUTPUT(IOUTYP,IGET(145),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           TEMPERATURE TENDENCY DUE TO RADIATIVE FLUX CONVERGENCE
            IF (IGET(140).GT.0) THEN
             IF (LVLS(L,IGET(140)).GT.0) THEN
               NREC1=LM+11+9*(L-1)
               CALL RDRST2D(TTND,IM,JM,LRSTRT,NREC0,NREC1,1,.FALSE.)
               NREC0=NREC1+1
                  ITYPE = LVLS(L,IGET(140))
                  CALL E2OUT(140,000,TTND,EGRID2,
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  ID(1:25) = 0
                  LYR = L
                  IF (ITYPE.GT.1) ID(9) = 109
                  CALL OUTPUT(IOUTYP,IGET(140),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           TEMPERATURE TENDENCY DUE TO SHORT WAVE RADIATION.
            IF (IGET(040).GT.0) THEN
             IF (LVLS(L,IGET(040)).GT.0) THEN
c              NREC1=LM+11+9*(L-1)
c              CALL RDRST2D(TTND,IM,JM,LRSTRT,NREC0,NREC1,1,.FALSE.)
c              NREC0=NREC1+1
                  ITYPE = LVLS(L,IGET(040))
c                 CALL NETAL(TTND,ITYPE,L,LMH,EGRID1)
C      FILLED WILL -H999 FOR NOW
!$omp  parallel do
                  DO J=JSTA,JEND
                  DO I=1,IM
                    EGRID1(I,J) = -H999
                  ENDDO
                  ENDDO
C
                  CALL E2OUT(040,000,EGRID1,EGRID2,
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  ID(1:25) = 0
                  LYR = L
                  IF (ITYPE.GT.1) ID(9) = 109
                  CALL OUTPUT(IOUTYP,IGET(040),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C           TEMPERATURE TENDENCY DUE TO LONG WAVE RADIATION.
            IF (IGET(041).GT.0) THEN
             IF (LVLS(L,IGET(041)).GT.0) THEN
c              NREC1=LM+11+9*(L-1)
c              CALL RDRST2D(TTND,IM,JM,LRSTRT,NREC0,NREC1,1,.FALSE.)
c              NREC0=NREC1+1
                  ITYPE = LVLS(L,IGET(041))
c                 CALL NETAL(TTND,ITYPE,L,LMH,EGRID1)
C      FILLED WILL -H999 FOR NOW
!$omp  parallel do
                  DO J=JSTA,JEND
                  DO I=1,IM
                    EGRID1(I,J)=-H999
                  ENDDO
                  ENDDO
C
                  CALL E2OUT(041,000,EGRID1,EGRID2,
     1                 GRID1,GRID2,IMOUT,JMOUT)
                  ID(1:25) = 0
                  LYR = L
                  IF (ITYPE.GT.1) ID(9) = 109
                  CALL OUTPUT(IOUTYP,IGET(041),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C
C           LATENT HEATING FROM GRID SCALE RAIN/EVAP. (TIME AVE)
            IF (IGET(078).GT.0) THEN
             IF (LVLS(L,IGET(078)).GT.0) THEN
               NREC1=LM+13+9*(L-1)
               CALL RDRST2D(TRAIN,IM,JM,LRSTRT,NREC0,NREC1,1,.FALSE.)
               NREC0=NREC1+1
                ITYPE = LVLS(L,IGET(078))
                IF(AVRAIN.GT.0.)THEN 
                  RRNUM=1./AVRAIN
                ELSE
                  RRNUM=0.
                ENDIF
!$omp  parallel do
                DO J=JSTA,JEND
                DO I=1,IM
                  EGRID1(I,J)=TRAIN(I,J)*RRNUM
                ENDDO
                ENDDO
C
                CALL E2OUT(078,000,EGRID1,EGRID2,
     1               GRID1,GRID2,IMOUT,JMOUT)
                ID(1:25) = 0
                IFHR       = NTSD/TSPH+0.5
                ITHEAT     = INT(THEAT)
                IFINCR     = MOD(IFHR,ITHEAT)
                ID(19) = IFHR
                ID(20) = 3
                IF (IFINCR.EQ.0) THEN
                   ID(18) = IFHR-ITHEAT
                ELSE
                   ID(18) = IFHR-IFINCR
                ENDIF
                IF (ID(18).LT.0) ID(18) = 0
                LYR = L
                IF (ITYPE.GT.1) ID(9) = 109
                CALL OUTPUT(IOUTYP,IGET(078),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C
C           LATENT HEATING FROM CONVECTION. (TIME AVE)
            IF (IGET(079).GT.0) THEN
             IF (LVLS(L,IGET(079)).GT.0) THEN
               NREC1=LM+14+9*(L-1)
               CALL RDRST2D(TCUCN,IM,JM,LRSTRT,NREC0,NREC1,1,.FALSE.)
               NREC0=NREC1+1
                ITYPE = LVLS(L,IGET(079))
                IF(AVCNVC.GT.0.)THEN
                  RRNUM=1./AVCNVC
                ELSE
                  RRNUM=0.
                ENDIF
!$omp  parallel do
                DO J=JSTA,JEND
                DO I=1,IM
                  EGRID1(I,J) = TCUCN(I,J)*RRNUM
                ENDDO
                ENDDO
C
                CALL E2OUT(079,000,EGRID1,EGRID2,
     1               GRID1,GRID2,IMOUT,JMOUT)
                ID(1:25) = 0
                IFHR       = NTSD/TSPH+0.5
                ITHEAT     = INT(THEAT)
                IFINCR     = MOD(IFHR,ITHEAT)
                ID(19) = IFHR
                ID(20) = 3
                IF (IFINCR.EQ.0) THEN
                   ID(18) = IFHR-ITHEAT
                ELSE
                   ID(18) = IFHR-IFINCR
                ENDIF
                IF (ID(18).LT.0) ID(18) = 0
                LYR = L
                IF (ITYPE.GT.1) ID(9) = 109
                CALL OUTPUT(IOUTYP,IGET(079),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C        PROCESS NEXT ETA LEVEL.
C
 190     CONTINUE
C
C     END OF ETA SURFACE OUTPUT BLOCK.
C
      ENDIF
C   VISIBILITY
      IF (IGET(180).GT.0) THEN
       RDTPHS= 1./(NPHS*DT)

C            NEED TO CALCULATE RAIN WATER AND SNOW MIXING RATIOS
       DO J=JSTA,JEND
       DO I=1,IM
         IF (PREC(I,J).EQ.0) THEN
           QSNO(I,J)=0.
           QRAIN(I,J)=0.
         ELSE
           LLMH=LMH(I,J)
           SNORATE=SR(I,J)*PREC(I,J)*RDTPHS
           RAINRATE=(1-SR(I,J))*PREC(I,J)*RDTPHS
           TERM1=(T(I,J,LM)/PSLP(I,J))**0.4167
           TERM2=(T(I,J,LLMH)/(PDSL(I,J)*AETA(LMH(I,J))+PT))**0.5833
           TERM3=RAINRATE**0.8333
           QRAIN(I,J)=RAINCON*TERM1*TERM2*TERM3
           TERM4=(T(I,J,LM)/PSLP(I,J))**0.47
           TERM5=(T(I,J,LLMH)/(PDSL(I,J)*AETA(LMH(I,J))+PT))**0.53
           TERM6=SNORATE**0.94
           QSNO(I,J)=SNOCON*TERM4*TERM5*TERM6
         ENDIF
         LLMH=LMH(I,J)
         TT(I,J)=T(I,J,LLMH)
         QV(I,J)=Q(I,J,LLMH)
         QCD(I,J)=(1-IW(I,J,LLMH))*CWM(I,J,LLMH)
         QICE(I,J)=IW(I,J,LLMH)*CWM(I,J,LLMH)
         PPP(I,J)=PDSL(I,J)*AETA(LMH(I,J))+PT
       ENDDO
       ENDDO
c      CALL CALVIS(QV,QCD,QR,QICE,QS,TT,PPP,PRSNOW,METH,IICE,VIS)
       CALL CALVIS(QV,QCD,QRAIN,QICE,QSNO,TT,PPP,VIS)
       CALL E2OUT(180,000,VIS,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
       ID(1:25) = 0
       CALL OUTPUT(IOUTYP,IGET(180),LVLS(1,IGET(180)),
     X           GRID1,IMOUT,JMOUT)
       ENDIF
C
C     
C     ASYMPTOTIC AND FREE ATMOSPHERE MASTER LENGTH SCALE (EL), PLUS
C     GRADIENT RICHARDSON NUMBER.  THESE FIELDS ARE PLACED OUTSIDE
C     THE ABOVE ETA LAYER LOOP SO THAT WE CAN EQUIVALENCE THE 3-D
C     EL ARRAY WITH 3-D ARRAY PMID.  THIS IS DONE TO MAKE THE POST
C     A BIT SMALLER.
C
      IF ( (IGET(111).GT.0) .OR. (IGET(146).GT.0) .OR. 
     X     (IGET(147).GT.0) .OR. (IGET(221).GT.0) ) THEN
C     
C        COMPUTE ASYMPTOTIC MASTER LENGTH SCALE.
         CALL CLMAX(DETA,PDSL,HTM,Q2,ZINT,SM,ZINT(1,1,LP1),
     X        LMH,IM,JM,LM,LP1,
     X        EL0,EGRID2,EGRID3,EGRID4,EGRID5)
C     
C        IF REQUESTED, POST ASYMPTOTIC MASTER LENGTH SCALE.
         IF (IGET(147).GT.0) THEN
            CALL E2OUT(147,000,EL0,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25) = 0
            CALL OUTPUT(IOUTYP,IGET(147),LM,GRID1,IMOUT,JMOUT)
         ENDIF
!       H CHUANG AND M EK LIFTED HPBL COMPUTATION FROM ETA MODEL
         IF (IGET(221).GT.0) THEN
           DO J=JSTA,JEND
            DO I=1,IM
             LMHK=LMH(I,J)
             CALL HPBLCAL(I,J,LM,LMHK,LPBL,HPBL(I,J)
     +       ,Q2(I,J,1:LM),ZINT(I,J,1:LM+1))
             if(i.eq.10.and.j.eq.10)print*,'Debug:sample HPBLCAL inp'
     +,      I,J,LM,LMHK,LPBL,HPBL(I,J),(Q2(I,J,L),l=1,lm)
     +       ,(ZINT(I,J,l),l=1,lm)
             if(i.eq.550.and.j.eq.144)print*
     +,      'Debug:sample HPBLCAL in ETAFLD2'
     +,      I,J,LM,LMHK,LPBL,HPBL(I,J),SM(I,J),(Q2(I,J,L),l=1,lm)
     +       ,(ZINT(I,J,l),l=1,lm+1)
C-----------------------------------------------------------------------
            END DO
           END DO
           CALL E2OUT(221,000,HPBL,EGRID2,
     X           GRID1,GRID2,IMOUT,JMOUT)
           ID(1:25) = 0
           CALL OUTPUT(IOUTYP,IGET(221),LVLS(1,IGET(221)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF

C     
C        IF REQUESTED, POST FREE ATMOSPHERE MASTER LENGTH SCALE
C        AND/OR THE GRADIENT RICHARDSON NUMBER.    
C
         IF ( (IGET(111).GT.0) .OR. (IGET(146).GT.0) ) THEN
C     
C           COMPUTE FREE ATMOSPHERE MASTER LENGTH SCALE.
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              HGT(I,J)=ZINT(I,J,LP1)
            ENDDO
            ENDDO
!$omp  parallel do
            DO L=1,LM
               DO J=JSTA,JEND
               DO I=1,IM
                 EL(I,J,L)=D00
               ENDDO
               ENDDO
            ENDDO
            CALL MIXLEN(ZINT,T,PDSL,AETA,PT,Q2,HGT,HTM,EL0,
     X           LM,LM1,LP1,IM,JM,
     X           EL)
C     
C           COMPUTE GRADIENT RICHARDSON NUMBER IF REQUESTED.
C     
            IF ( (IGET(111).GT.0) ) CALL CALRCH(EL,RICHNO)
C
C           LOOP OVER ETA LAYERS.
            DO 200 L = 1,LM
C     
C              POST MIXING LENGTH.
C
            IF (IGET(146).GT.0) THEN
             IF (LVLS(L,IGET(146)).GT.0) THEN
                  ITYPE = LVLS(L,IGET(146))
                  CALL NETAL(EL,ITYPE,L,LMH,EGRID1)
                  CALL E2OUT(146,000,EGRID1,EGRID2,
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  ID(1:25) = 0
                  LYR = L
                  IF (ITYPE.GT.1) ID(9) = 109
                  CALL OUTPUT(IOUTYP,IGET(146),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
C     
C              POST GRADIENT RICHARDSON NUMBER.
C
            IF (IGET(111).GT.0) THEN
             IF (LVLS(L,IGET(111)).GT.0) THEN
                  ITYPE = LVLS(L,IGET(111))
                  CALL NETAL(RICHNO,ITYPE,L,LMH,EGRID1)
                  CALL E2OUT(111,000,EGRID1,EGRID2,
     X                 GRID1,GRID2,IMOUT,JMOUT)
                  ID(1:25) = 0
                  LYR = L
                  IF (ITYPE.GT.1) ID(9) = 109
                  CALL OUTPUT(IOUTYP,IGET(111),LYR,GRID1,IMOUT,JMOUT)
             ENDIF
            ENDIF
 200        CONTINUE
C
         ENDIF
      ENDIF
C     
C     
C     END OF ROUTINE.
C     
      print *, " CWM start of ETAFLD"
      do l=1,lm
      do j=jm,1,-1
!      write(*,"(10E12.7)") (CWM(I,J,L),I=1,IM)
      end do
      end do
      print *, " CWM end of ETAFLD"
      RETURN
      END
