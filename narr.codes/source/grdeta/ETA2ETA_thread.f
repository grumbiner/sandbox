      SUBROUTINE ETA2ETA(HIN,PSFC,ZSFC,WET,SST,SI,EREFSE,IDATE
     1,                  SIGMA,REF,ETOP)
C
C       SUBPROGRAM DOCUMENTAION BLOCK
C
C  SUBPROGRAM:  ETA2ETA               SIGMA-TO-ETA INTERPOLATION PROGRAM
C    AUTHOR: FEDOR MESINGER           ORG: W/NP22         DATE: ??-??-??
C
C
C  ABSTRACT:  SLIGHTLY ADJUSTS ETA MODEL GUESS HISTORY VARIABLES NAD
C             RECOMPUTES HEIGHT, TEMPERATURES, AND WINDS ON ETA LEVELS
C             TO BE CONSISTENT WITH SURFACE PRESSURE
C
C
C  PROGRAM HISTORY LOG:
C    ??-??-??  F MESINGER WROTE ORIGINAL PTETA CODE
C    ??-??-??  T BLACK MADE NUMEROUS REVISONS
C    90-03-14  E ROGERS REWROTE CODE TO MAKE INDEXING CONSISTENT WITH
C              ETA MODEL FORECAST CODES
C    ??-??-??  D DEAVEN AND E ROGERS REVISED OLD PTETA CODE 
C    98-05-08  T BLACK - COMPLETELY RECODED FOR OPTIMIZATION
C    98-06-05  MIKE BALDWIN - CONVERT TO 2-D CODE
C
C---------------------------------------------------------------------
      integer,parameter::real_32=selected_real_kind(6,30)
      INCLUDE "parmeta.h"
      PARAMETER (IMXJM=IM*JM)
C---------------------------------------------------------------------
C
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::TETA,UETA,VETA,QETA
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::H
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::PMVP
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::PDVP
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::PINT,PMID
C
                             D I M E N S I O N
c    1  H(IM,JM,LDMZ)
c    1  H(IM,JM,LDMZ), T(IM,JM,LDM), U(IM,JM,LDM), V(IM,JM,LDM)
c    2, Q(IM,JM,LDM)
c    2, Q(IM,JM,LDM), QETA(IM,JM,LM)
c    2, Q(IM,JM,LDM), HETA(IM,JM,LM), QETA(IM,JM,LM)
     3  PD(IM,JM)
c    3, PD(IM,JM),UETA(IM,JM,LM),VETA(IM,JM,LM), TETA(IM,JM,LM)
c    4, PMVP(IM,JM,LDM),PDVP(IM,JM)
c    5, ALP(IM,JM,LDMZ),ALPR(IM,JM,LDMZ),ALPSQ(IM,JM,LDMZ)
     6, ALPTU(IM,JM),PSFC(IM,JM), WET(IM,JM), SST(IM,JM), SI(IM,JM)
     7, ALPETA(IM,JM)
c    7, PINT(IM,JM,LDMZ), PMID(IM,JM,LDM),ALPETA(IM,JM)
     7, PINTER(IM,JM),PDORG(IM,JM)
     7, PETA(IM,JM),IDXX(IMXJM),IDX(IMXJM),KSTRTX(IM,JM)
     7, JDXX(IMXJM),JDX(IMXJM)
     8, ZSFC(IM,JM),SM(IM,JM),LDT1(IM,JM,LM)
c    8, ZSFC(IM,JM),SM(IM,JM),LDT1(IM,JM,LM),EXNL(IM,JM)
     9, PDB(KB,2), TB(KB,LM,2), UB(KB,LM,2)
c    9, DLT(IM,JM), PDB(KB,2), TB(KB,LM,2), UB(KB,LM,2)
     O, VB(KB,LM,2), QB(KB,LM,2), FIX(IM,JM)
     1, HGT(IM,JM), PHIS(IM,JM),TX(IM,JM)
     2, ETA(LMP1), ETAL(LM), DETA(LM), ZETA(LMP1), REF(IM,JM), IDAT(3)
     4, PETAS(LM),IDATE(4),EREFSE(IM,JM)
     5, RSPL(IM,JM),L1(IM,JM),LL(IM,JM)
     6, IHE(JM),IHW(JM),IVE(JM),IVW(JM)
C
        COMMON /GBLATM/T(IM,JM,LDM),U(IM,JM,LDM),V(IM,JM,LDM)
     1,     Q(IM,JM,LDM)
C
                             P A R A M E T E R
     & (K15=SELECTED_REAL_KIND(15))
C
                             R E A L
     & (KIND=K15) ALP(IM,JM,LDMZ),ALPL3,ALPL2,
     &  DLTPD,B,C,ARGSQT,ALPGT,DLT(IM,JM),ARGLOG,ALPET,ALPL1K,
     &  ARGLG2,ALPM,EXNL(IM,JM),EXNT,ALPETK
C---------------------------------------------------------------------
       DIMENSION ALPR(IM,JM,LDMZ)
C
                             L O G I C A L
     1  RUN,SIGMA
C
C---------------------------------------------------------------------
                             D A T A
     1 LIN /11/, LOROG/14/, LDATE/13/, LOUT/51/
C
C---------------------------------------------------------------------
c                          E Q U I V A L E N C E
c    & (HETA(1,1),H(1,1)),(UETA(1,1),U(1,1))
c    &, (VETA(1,1),V(1,1)),(H(1,1),HIN(1,1))
c    &, (TETA(1,1),T(1,1)),(QETA(1,1),Q(1,1))
C
C--------------UNIVERSAL CONSTANTS--------------------------------------
C
      PI=3.141592654
      PIHF=0.5*PI
      DTR=PI/180.
C
C--------------HORIZONTAL GRID CONSTANTS--------------------------------
C
C --SET UP SWITCHES FOR PRINTING OF OUTPUT, REMOVING MOUNTAINS, ETC.----
C
C        HOUR00 FIELDS TO BE CREATED FOR A SIGMA MODE/NOT A SIGMA MODE
C        EXPERIMENT
C
C
C     CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C
      DO J=1,JM
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
        IVE(J)=MOD(J,2)
        IVW(J)=IVE(J)-1
      ENDDO
C
C------------- READ SIGMA DATA AND DATE RECORD---------------------------
C
c     READ(LDATE)IDATE
      IDAT(1)=IDATE(2)
      IDAT(2)=IDATE(3)
      IDAT(3)=IDATE(4)
      IHRST  =IDATE(1)
C
      WRITE(6,20)(IDAT(I),I=1,3),IHRST
   20 FORMAT(2X,4I6)
C
c     READ(LIN)HIN,T,U,V,Q,PSFC,ZSFC,WET,SST,SI,EREFSE
C
C
      ALLOCATE(TETA(IM,JM,LM))
      ALLOCATE(QETA(IM,JM,LM))
      ALLOCATE(UETA(IM,JM,LM))
      ALLOCATE(VETA(IM,JM,LM))
C
!$OMP PARALLEL DO
      DO L=1,LM
        DO J=1,JM
         DO I=1,IM
          UETA(I,J,L)=U(I,J,L)
          VETA(I,J,L)=V(I,J,L)
          QETA(I,J,L)=Q(I,J,L)
          TETA(I,J,L)=T(I,J,L)
         ENDDO
        ENDDO
      ENDDO
C
!$OMP PARALLEL DO
      DO J=1,JM
       DO I=1,IM
        PSFC(I,J)=PSFC(I,J)*100.0
       ENDDO
      ENDDO
C---------------------------------------------------------------------
C
C------- DEFINITION OF CONSTANTS NEEDED FOR VERTICAL INTERPOLATION ---
C
C---------------------------------------------------------------------
      G=9.80
      R=287.04
      CP=1004.6
      P00=1.E5
      GAMMA=0.0065
      PRF0=101325.0
      T0=288.0
      GORG=G/(R*GAMMA)
      RGOG=1./GORG
      ROG=R/G
      CPOG=CP/G
      CAPA=R/CP
      PT=ETOP*100.
      PETAT=PT
      DETA2=0.
      ALPT=ALOG(PT)
C
C------- DEPTH OF ETA LAYERS ----------------------------------------
C
      READ(16)DETA
      REWIND 16
C
C------- COMPUTE ETA AT THE INTERFACES OF THE MODEL LAYERS ------------------
C
      ZMAX=-100000.0
      ZMIN= 100000.0
C
!$OMP PARALLEL DO
      DO J = 1, JM
       DO I = 1, IM
        FIX(I,J)=0.
        PD(I,J)=PSFC(I,J)-PT
       ENDDO
      ENDDO
C
      ETA(1)=0.
C
      DO L=1,LMM1
        ETA(L+1)=ETA(L)+DETA(L)
      ENDDO
C
      ETA(LMP1)=1.
      DETA(LM)=1.-ETA(LM)
C
      DO L=1,LM
        ETAL(L)=0.5*(ETA(L)+ETA(L+1))
      ENDDO
C
      DO L=1,LMP1
        ZETA(L)=T0*(1.-((PT+ETA(L)*(PRF0-PT))/PRF0)**RGOG)
     1            /GAMMA
      ENDDO
C
C------------- VERTICAL INTERPOLATION TO CONSTANT ETA LAYERS ---------
C
C------- DEFINITION OF PRESSURE DATA AND ETA LAYERS CONSTANTS --------
C
C---------------------------------------------------------------------
C***  READ IN THE PRE-COMPUTED ADJUSTED STEPS, SEA MASK AND REF
C***  FILE IS STORED IN FULL CRAY WORDS
C
C
CMEB  ASSUMING THIS IS A 2-D FILE NOW.
C
      REWIND LOROG
      READ(LOROG)HGT,SM
      READ(LOROG)REF
      REWIND LOROG
C
      IF(SIGMA)THEN
!$OMP PARALLEL DO
        DO J=1,JM
        DO I=1,IM
          REF(I,J)=1.
        ENDDO
        ENDDO
      ENDIF
C
C---------------------------------------------------------------------
      ALLOCATE(H(IM,JM,LDMZ))
      ALLOCATE(PMID(IM,JM,LDM))
      ALLOCATE(PINT(IM,JM,LDMZ))
C***
C***  FIND HEIGHTS ON SIGMA LAYER INTERFACES
C***
!$OMP PARALLEL DO
      DO 110 L=1,LDM
C
      DO J=1,JM
      DO I=1,IM
        PMID(I,J,L)=(ETAL(L)/EREFSE(I,J))*(PSFC(I,J)-PT)+PT
C
        H(I,J,L)=9.E37
        PINT(I,J,L)=(ETA(L)/EREFSE(I,J))*(PSFC(I,J)-PT)+PT
        IF(ETA(L)/EREFSE(I,J).GE.0.9999)THEN
          H(I,J,L)=ZSFC(I,J)
        ENDIF
        IF(ETA(L)/EREFSE(I,J).GE.1.0001)THEN
          PINT(I,J,L)=-10.
          H(I,J,L)=9.E37
        ENDIF
      ENDDO
      ENDDO
C
  110 CONTINUE
C
!$OMP PARALLEL DO
      DO J=1,JM
      DO I=1,IM
        H(I,J,LDMZ)=9.E37
        PINT(I,J,LDMZ)=(ETA(LDMZ)/EREFSE(I,J))*(PSFC(I,J)-PT)+PT
        PINTER(I,J)=(ETA(LDMZ)/EREFSE(I,J))*(PSFC(I,J)-PT)+PT
        IF(ETA(LDMZ)/EREFSE(I,J).GE.0.9999)THEN
          H(I,J,LDMZ)=ZSFC(I,J)
        ENDIF
        IF(ETA(LDMZ)/EREFSE(I,J).GE.1.0001)THEN
          PINT(I,J,LDMZ)=-10.
          H(I,J,LDMZ)=9.E37
        ENDIF
      ENDDO
      ENDDO
C---------------------------------------------------------------------
!$OMP PARALLEL DO PRIVATE(EXNT,EXNL)
      DO 125 L=LDM,1,-1
      DO J=1,JM
      DO I=1,IM
        IF(H(I,J,L+1).LE.1.E37)THEN
          IF(ZSFC(I,J).EQ.H(I,J,L+1))THEN
            EXNL(I,J)=(PSFC(I,J)/P00)**CAPA
          ENDIF
          EXNT=(PINT(I,J,L)/P00)**CAPA
          H(I,J,L)=H(I,J,L+1)+T(I,J,L)*R*
     &             (LOG(PINT(I,J,L+1)/PINT(I,J,L)))/G
C
c---------------------------------------------------------------------
c         IF(I.EQ.5.AND.J.EQ.108 .OR. I.EQ.6.AND.J.EQ.111 
c    1       .OR. I.EQ.116.AND.J.EQ.39. OR. I.EQ.184.AND.J.EQ.350 
c    2       .OR. I.EQ.141.AND.J.EQ.266) THEN
c           WRITE(6,122)I,J,L,ETA(L),REF(I,J),PSFC(I,J),H(I,J,L),
c    1                    HGT(I,J), PINT(I,J,LDMZ)
c 122         FORMAT(2X,'I,J,L=',3I5,' ETA=',E9.4,' REF=',E9.4,
c    1         ' PSFC=',E9.4,' H=',E9.4,
c    2         ' HGT=',E9.4,' PINT(LDMZ)=',E9.4)
c         ENDIF
c---------------------------------------------------------------------
C
          EXNL(I,J)=EXNT
        ENDIF
      ENDDO
      ENDDO
  125 CONTINUE
C---------------------------------------------------------------------
c     DO L=1,LM
c       WRITE(6,130)L,ETA(L),ETAL(L),ZETA(L)
c 130   FORMAT(2X,I3,1X,5(E12.5,1X))
c     ENDDO
C
C---------------------------------------------------------------------
C
C----------- VERTICAL INTERPOLATION: QUADRATIC INTERPOLATION OF
C----------- HEIGHTS (EQUIVALENT TO TEMPERATURE BEING LINEAR IN LN P)
C----------- AND LINEAR INTERPOLATION OF WINDS
C
C---------------------------------------------------------------------
C        COMPUTATION OF THE 'SEA LEVEL' PRESSURE DIFFERENCE
C
!$OMP PARALLEL DO PRIVATE(RSPL)
      DO LD=1,LDM
        DO J=1,JM
        DO I=1,IM
          IF(PINT(I,J,LD+1).GT.0..AND.PINT(I,J,LD).GT.0.)THEN
            ALPR(I,J,LD)=ALOG(PINT(I,J,LD+1)/PINT(I,J,LD))
          ELSE
            ALPR(I,J,LD)=9.E37
          ENDIF
C
          IF(PINT(I,J,LD).GT.0.)THEN
            ALP(I,J,LD)=ALOG(PINT(I,J,LD))
c           ALPSQ(I,J,LD)=ALP(I,J,LD)**2
          ELSE
            ALP(I,J,LD)=9.E37
c           ALPSQ(I,J,LD)=9.E37
          ENDIF
C
          IF(ABS(PINT(I,J,LD)-PSFC(I,J)).LT.0.5)THEN
            RSPL(I,J)=PSFC(I,J)/PINT(I,J,LD-1)
          ENDIF
C
        ENDDO
        ENDDO
      ENDDO
C
      DO J=1,JM
      DO I=1,IM
        IF(PINT(I,J,LDMZ).GT.0.)THEN
          ALP(I,J,LDMZ)=ALOG(PINT(I,J,LDMZ))
c         ALPSQ(I,J,LDMZ)=ALP(I,J,LDMZ)**2
        ELSE
          ALP(I,J,LDMZ)=9.E37
c         ALPSQ(I,J,LDMZ)=9.E37
        ENDIF
      ENDDO
      ENDDO
C---------------------------------------------------------------------
      DO J=1,JM
      DO I=1,IM
        L1(I,J)=LDMZ-2
      ENDDO
      ENDDO
C
      DO 170 L=LDMZ-2,10,-1
      DO J=1,JM
      DO I=1,IM
        IF(PINT(I,J,L+2).LE.0.)THEN
          L1(I,J)=L-1
        ENDIF
      ENDDO
      ENDDO
  170 CONTINUE
C---------------------------------------------------------------------
!$OMP PARALLEL DO PRIVATE(DLTPD,H1A,H2,H3,H3M2,H3M1,
!$OMP&       ALPL3,ALPL1K,ALPL2,B,C,ARGSQT,ALPGT,HLDM)
      DO J=1,JM
      DO I=1,IM
C---------------------------------------------------------------------
      L1K=L1(I,J)
      DLTPD=ALPR(I,J,L1K+1)*ALOG(PINT(I,J,L1K+2)/PINT(I,J,L1K))
     1      *(-ALPR(I,J,L1K))
C
      L2=L1K+1
      L3=L1K+2
      H1A=H(I,J,L1K)
      H2=H(I,J,L2)
      H3=H(I,J,L3)
      H3M2=H3-H2
      H3M1=H3-H1A
C
      ALPL3=ALP(I,J,L3)*ALP(I,J,L3)
      ALPL1K=ALP(I,J,L1K)*ALP(I,J,L1K)
      ALPL2=ALP(I,J,L2)*ALP(I,J,L2)

      B=(H3M2*(ALPL3-ALPL1K)
     1    -H3M1*(ALPL3-ALPL2))/DLTPD
      C=(H3M1*(ALP(I,J,L3)-ALP(I,J,L2))
     &    -H3M2*(ALP(I,J,L3)-ALP(I,J,L1K)))/DLTPD

c     B=(H3M2*(ALPSQ(I,J,L3)-ALPSQ(I,J,L1K))
c    1    -H3M1*(ALPSQ(I,J,L3)-ALPSQ(I,J,L2)))/DLTPD
c     C=(H3M1*(ALP(I,J,L3)-ALP(I,J,L2))
c    &    -H3M2*(ALP(I,J,L3)-ALP(I,J,L1K)))/DLTPD
C
      IF(ABS(C).GE.1.E-10)THEN
        ARGSQT=B**2-(4.*C*(-C*ALPL3-B*ALP(I,J,L3)+H3-
     1           HGT(I,J)))
c       ARGSQT=B**2-(4.*C*(-C*ALPSQ(I,J,L3)-B*ALP(I,J,L3)+H3-
c    1           HGT(I,J)))
C
c       ALPGT=(-B-SQRT(B**2-4.*C*(-C*ALPSQ(I,J,L3)-B*ALP(I,J,L3)+H3-
c    1           HGT(I,J))))/(2.*C)
        ALPGT=(-B-SQRT(ARGSQT))/(2.*C)
        PD(I,J)=(EXP(ALPGT)-PT)/REF(I,J)
        PDORG(I,J) = PD(I,J)
C
        IF(ARGSQT .LT. 0.0) THEN
          write(6,*)'argsqt<0 ',i,j,argsqt
          write(6,*)i,j,B,C,ALPGT,ALP(I,J,L3),HGT(I,J),
     1      H3,ref(i,j),pd(i,j)
          PD(I,J) = PINTER(I,J) - PT
        ENDIF
C
c------------------------------------------------------------------
      ELSE
        HLDM=H(I,J,LDM)
        WRITE(6,*)"SFC TROUBLE"
        PD(I,J)=(PSFC(I,J)*RSPL(I,J)**((H(I,J,L2)-HGT(I,J))/
     1         (H(I,J,L3)-H(I,J,L2)))-PT)/REF(I,J)
        PDORG(I,J) = PD(I,J)
        PD(I,J) = PINTER(I,J) - PT
      ENDIF
C
C  BEGIN CHECKS FOR BAD PD; IF PD DETERMINED TO BE BAD REPLACE
C  WITH PINT(LDMZ)-PT = PSFC-PT 
C
C  FIRST, DO GROSS CHECK
C
      IF(PD(I,J).LT.50000. .OR. PD(I,J).GT.110000.) THEN
         write(6,*)'off-the-wall pd ',i,j,pd(i,j),pinter(i,j)
         PD(I,J) = PINTER(I,J) - PT
      ENDIF
C
C  SECOND, LOOK FOR ABNORMORALLY LOW PD OVER OCEAN
C
      IF(SM(I,J).GT.0.9 .AND. PD(I,J).LT.92500.) THEN
         write(6,*)'off-the-wall pd over water ',i,j,pd(i,j),
     1     pinter(i,j)
         PD(I,J) = PINTER(I,J) - PT
      ENDIF
C
C  FINALLY, IF ABS((PSFC-PT) - PD) > 30 MB ASSUME WE HAVE
C  BAD PD
C
      PDDIFF = ABS((PINTER(I,J)-PT) - PD(I,J))
      IF(PDDIFF.GE.3000.) THEN
         write(6,*)'off-the-wall pd #2 ',i,j,pd(i,j),
     1     pinter(i,j)
         PD(I,J) = PINTER(I,J) - PT
      ENDIF

c     IF(MOD(I,10).EQ.0 .AND. MOD(J,10).EQ.0) THEN
c      PRINT *,'ORIG PD ',I,J,PD(I,J),REF(I,J),PINTER(I,J),
c    1   HGT(I,J),PSFC(I,J),PDORG(I,J)
c     ENDIF

c     IF(I.EQ.5.AND.J.EQ.108 .OR. I.EQ.6.AND.J.EQ.111
c    1   .OR. I.EQ.116.AND.J.EQ.39. OR. I.EQ.184.AND.J.EQ.350
c    2   .OR. I.EQ.141.AND.J.EQ.266) THEN
c      PRINT *,'ORIG PD ',I,J,PD(I,J),REF(I,J),PINTER(I,J),
c    1   HGT(I,J),PSFC(I,J),PDORG(I,J)
c     ENDIF

      ENDDO
      ENDDO
C----------------------------------------------------------------------
C        PD HAS BEEN CHANGED TO MAKE IT EQUAL TO THE DIFFERENCE
C        BETWEEN P*, PRESSURE EXTRAPOLATED TO THE ETA=1 SURFACE, AND PT
C
C------- COMPUTATION OF HEIGHTS AT UPPER BOUNDARIES OF ETA LAYERS -----
C------- AND TEMPERATURE AND SPEC HUMIDITY AT MIDDLE OF ETA LAYERS -----
C
C***  LDT1 HOLDS THE VERTICAL INDEX OF THE PARENT GRID LAYER INTERFACE
C***  THAT IS DIRECTLY ABOVE THE OUTPUT ETA INTERFACE IN QUESTION.
C
C---------------------------------------------------------------------
!$OMP PARALLEL DO
      DO J=1,JM
      DO I=1,IM
        PETA(I,J)=PT+0.0001
        LDT1(I,J,1)=1
      ENDDO
      ENDDO
C
C-------------------------------------------------------------------
      MNKNT=1
      DO 230 L=2,LM
      DETA1=DETA(L-1)
C
CMEB ????
      NUMK=IM*JM
      KNTX=MNKNT
      KSTRT=KNTX+1
C
      DO J=1,JM
      DO I=1,IM
        NN=(J-1)*IM+I
        PETA(I,J)=PETA(I,J)+DETA1*PD(I,J)
        IDXX(NN)=I
        JDXX(NN)=J
        KSTRTX(I,J)=1
      ENDDO
      ENDDO
C
  220 NK=NUMK
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
        JDX(NN)=JDXX(NN)
      ENDDO
      KNTIN=KNTX
      NUMK=0
      DO N=1,NK
        I=IDX(N)
        J=JDX(N)
        PETAK=PETA(I,J)
        PINTA=PINT(I,J,KNTIN)
        PINTB=PINT(I,J,KNTIN+1)
        IF((PETAK.GE.PINTA.AND.PETAK.LE.PINTB
     1     .OR.PETAK.LT.PINT(I,J,2).OR.KNTIN.EQ.LM-1)
     2     .AND.PINTB.GT.0.)THEN
          LDT1(I,J,L)=MIN(KNTIN,LDM-1)
          IF(PETAK+DETA(L)*PD(I,J).LE.PINTB)THEN
            KSTRTX(I,J)=MIN(KSTRT,KNTIN)
          ELSE
            KSTRTX(I,J)=MIN(KSTRT,KNTIN+1)
          ENDIF
        ELSEIF(PINTB.GT.0.)THEN
          NUMK=NUMK+1
          IDXX(NUMK)=I
          JDXX(NUMK)=J
          KNTX=KNTIN+1
        ELSEIF(PINTB.LT.0.)THEN
          LDT1(I,J,L)=LDT1(I,J,L-1)
          IF(PINTA.GT.0.)LDT1(I,J,L)=KNTIN-2
          KSTRTX(I,J)=KNTIN+1
        ENDIF
      ENDDO
C
      IF(NUMK.GT.0)GO TO 220
      IF(NUMK.EQ.0)THEN
        MNKNT=MNKNT+1
        DO J=1,JM
        DO I=1,IM
          MNKNT=MIN(MNKNT,KSTRTX(I,J))
        ENDDO
        ENDDO
      ENDIF
  230 CONTINUE
c------------------------------------------------------------------
      DO L=10,LM
        DO J=1,JM
        DO I=1,IM
          INDX=LDT1(I,J,L)
          IF(PINT(I,J,INDX+2).LE.0.)THEN
            LDT1(I,J,L)=LDT1(I,J,L)-1
          ENDIF
        ENDDO
        ENDDO
      ENDDO
C-------------------------------------------------------------------
C---------------------------------------------------------------------
!$OMP PARALLEL DO PRIVATE(H1A,H2,H3,H3M2,H3M1,ALPL3,ALPL1K,
!$OMP&    B,C,ARGLOG,ALPET,ARGLG2,ALPM,HETA) 
      DO 300 L=1,LM
C---------------------------------------------------------------------
      DO J=1,JM
      DO I=1,IM
        L1K=LDT1(I,J,L)
        DLT(I,J)=ALPR(I,J,L1K+1)*LOG(PINT(I,J,L1K+2)/PINT(I,J,L1K))
     1        *(-ALPR(I,J,L1K))
C
        L1K=LDT1(I,J,L)
        L2=L1K+1
        L3=L1K+2
        H1A=H(I,J,L1K)
        H2=H(I,J,L2)
        H3=H(I,J,L3)
        H3M2=H3-H2
        H3M1=H3-H1A
C
        ALPL3=ALP(I,J,L3)*ALP(I,J,L3)
        ALPL1K=ALP(I,J,L1K)*ALP(I,J,L1K)
        ALPL2=ALP(I,J,L2)*ALP(I,J,L2)
C
        B=(H3M2*(ALPL3-ALPL1K)
     1    -H3M1*(ALPL3-ALPL2))/DLT(I,J)
        C=(H3M1*(ALP(I,J,L3)-ALP(I,J,L2))
     1    -H3M2*(ALP(I,J,L3)-ALP(I,J,L1K)))/DLT(I,J)
C
c       B=(H3M2*(ALPSQ(I,J,L3)-ALPSQ(I,J,L1K))
c    1    -H3M1*(ALPSQ(I,J,L3)-ALPSQ(I,J,L2)))/DLT(I,J)
c       C=(H3M1*(ALP(I,J,L3)-ALP(I,J,L2))
c    1    -H3M2*(ALP(I,J,L3)-ALP(I,J,L1K)))/DLT(I,J)
C
        ARGLOG=PT+PD(I,J)*ETA(L)
        ALPET=LOG(ARGLOG)
        ARGLG2=PT+PD(I,J)*(ETA(L)+0.5*DETA(L))
        ALPM=LOG(ARGLG2)
        HETA=H2+B*(ALPET-ALP(I,J,L2))+C*(ALPET**2-ALPL2)
c       HETA(I,J,L)=H2+B*(ALPET-ALP(I,J,L2))+C*(ALPET**2-ALPSQ(I,J,L2))
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
  300 CONTINUE
      DEALLOCATE(H)
      DEALLOCATE(PINT)
C-----------------------------------------------------------------------
C
C        COMPUTATION OF WINDS WITHIN ETA LAYERS
C
C-----------------------------------------------------------------------
      ALLOCATE(PMVP(IM,JM,LDM))
      ALLOCATE(PDVP(IM,JM))
C
!$OMP PARALLEL DO
      DO L=1,LDM
C
        DO J=2,JM-1
        DO I=2,IM-1
          PMVP(I,J,L)=0.25*(PMID(I+IVE(J),J,L)+PMID(I+IVW(J),J,L)
     1                 +PMID(I,J+1,L)+PMID(I,J-1,L))
        ENDDO
        ENDDO
C***
C***  COMPUTE NORTH AND SOUTH EDGES AND RECOMPUTE EAST AND WEST SIDES
C***
        DO I=1,IM-1
          PMVP(I,1,L)=0.5*(PMID(I+IVE(1),1,L)+PMID(I+IVW(1),1,L))
          PMVP(I,JM,L)=0.5*(PMID(I+IVE(JM),JM,L)+PMID(I+IVW(JM),JM,L))
        ENDDO
        DO J=2,JM-1,2
          PMVP(1,J,L)=0.5*(PMID(1,J+1,L)+PMID(1,J-1,L))
          PMVP(IM,J,L)=0.5*(PMID(IM,J+1,L)+PMID(IM,J-1,L))
        ENDDO
        DO J=1,JM,2
          PMVP(IM,J,L)=PMVP(IM-1,J,L)
        ENDDO
        DO J=3,JM-2,2
          PMVP(1,J,L)=0.25*(PMID(1+IVE(J),J,L)+PMID(1+IVW(J),J,L)
     1                 +PMID(1,J+1,L)+PMID(1,J-1,L))
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
!$OMP PARALLEL DO
      DO L=1,LDM-1
        DO J=1,JM
        DO I=1,IM
          ALPR(I,J,L)=ALOG(PMVP(I,J,L+1)/PMVP(I,J,L))
          ALP (I,J,L)=ALOG(PMVP(I,J,L))
        ENDDO
        ENDDO
      ENDDO
C
!$OMP PARALLEL DO
      DO J=2,JM-1
       DO I=2,IM-1
          PDVP(I,J)=0.25*(PD(I+IVE(J),J)+PD(I+IVW(J),J)
     1                 +PD(I,J+1)+PD(I,J-1))
       ENDDO
      ENDDO
C***
C***  COMPUTE NORTH AND SOUTH EDGES AND RECOMPUTE EAST AND WEST SIDES
C***
      DO I=1,IM-1
        PDVP(I,1) =0.5*(PD(I+IVE(1),1)+PD(I+IVW(1),1))
        PDVP(I,JM)=0.5*(PD(I+IVE(JM),JM)+PD(I+IVW(JM),JM))
      ENDDO
      DO J=2,JM-1,2
        PDVP(1,J) =0.5*(PD(1,J+1)+PD(1,J-1))
        PDVP(IM,J)=0.5*(PD(IM,J+1)+PD(IM,J-1))
      ENDDO
      DO J=1,JM,2
        PDVP(IM,J)=PDVP(IM-1,J)
      ENDDO
      DO J=3,JM-2,2
        PDVP(1,J)=0.25*(PD(1+IVE(J),J)+PD(1+IVW(J),J)
     1                 +PD(1,J+1)+PD(1,J-1))
      ENDDO
C-----------------------------------------------------------------------
!$OMP PARALLEL DO
      DO J=1,JM
      DO I=1,IM
        ALP(I,J,LDM)=ALOG(PMVP(I,J,LDM))
      ENDDO
      ENDDO
      DEALLOCATE(PMVP)
      DEALLOCATE(PMID)
C
C***
C***  COMPUTE WINDS (SINCE FIX IS HARDWIRED TO 0. FOR ALL K,
C***                 SKIP CHECKING THE VALUE OF FIX AND PROCEED)
C***
      MNKNT=1
      DO 400 L=1,LM
C
      DO J=1,JM
      DO I=1,IM
        NN=(J-1)*IM+I
        ALPETA(I,J)=ALOG(PT+PDVP(I,J)*ETAL(L))
        IDXX(NN)=I
        JDXX(NN)=J
      ENDDO
      ENDDO
C
      NUMK=IM*JM
      KNTX=MNKNT
C
  350 NK=NUMK
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
        JDX(NN)=JDXX(NN)
      ENDDO
      KNTIN=KNTX
      KP1=MIN(LM,KNTIN+1)
      NUMK=0
C
cdir$ ivdep
      DO N=1,NK
        I=IDX(N)
        J=JDX(N)
        ALPETK=ALPETA(I,J)
        IF(ALPETK.GE.ALP(I,J,KNTIN).AND.ALPETK.LE.ALP(I,J,KP1)
     1    .AND.ABS(U(I,J,KP1)).GT.0..OR.ALPETK.LT.ALP(I,J,1)
     2    .OR.KNTIN.EQ.LM-1)THEN
          ULD=U(I,J,KP1)
          VLD=V(I,J,KP1)
          CF=(ALP(I,J,KP1)-ALPETK)/ALPR(I,J,KNTIN)
          UKB=ULD+(U(I,J,KNTIN)-ULD)*CF
          VKB=VLD+(V(I,J,KNTIN)-VLD)*CF
          ALPTU(I,J)=ALPETK
          LL(I,J)=L
          UETA(I,J,L)=UKB
          VETA(I,J,L)=VKB
        ELSEIF(ALPETK.GT.ALP(I,J,KP1))THEN
          NUMK=NUMK+1
          IDXX(NUMK)=I
          JDXX(NUMK)=J
          KNTX=KNTIN+1
        ENDIF
      ENDDO
      IF(NUMK.GT.0)THEN
        IF(NUMK.EQ.IMJM)MNKNT=MNKNT+1
        GO TO 350
      ENDIF
  400 CONTINUE
      DEALLOCATE(PDVP)
c------------------------------------------------------------------
c     IF(I.EQ.1.AND.J.EQ.1)THEN
c       WRITE(6,335)L,LDM1,ULD,VLD,UETA(I,J,L),VETA(I,J,L),
c    1              ALPR(I,J,LDM1),CF,U(I,J,LDM1),V(I,J,LDM1)
c 335   FORMAT(1X,'L,LDM= ',2I3,' ULD,VLD= ',2(E11.4,1X),
c    1         ' UETA,VETA= ',2(E11.4,1X),' ALPR(K,LDM1)= ',E11.4,
c    2         ' CF= 'E11.4,' ULDM1,VLDM1= ',2(E11.4,1X))
c     ENDIF
c------------------------------------------------------------------
C
C----- CAP SPEC HUMIDITY BASED ON SATURATION VALUES --------------------
C
C-----------------------------------------------------------------------
      DO 425 L=1,LM
      DO J=1,JM
      DO I=1,IM
        IF(TETA(I,J,L).LT.150.)TETA(I,J,L)=150.
        CLOGES=-CM1/TETA(I,J,L)-CM2*ALOG10(TETA(I,J,L))+CM3
        ESE=10.**(CLOGES+2.)
        QS=EPS*ESE/(ETAL(L)*PD(I,J)+PT-ESE*(1.-EPS))
        QSMX=0.95*QS
        QETA(I,J,L)=AMIN1(QETA(I,J,L),QSMX)
      ENDDO
      ENDDO
  425 CONTINUE
      print *,'ok after 425'
C------------------------------------------------------------------
C
C     CONVERT FROM VIRTUAL TO TRUE TEMPERATURE.
C
C------------------------------------------------------------------
!$OMP PARALLEL DO PRIVATE(cloges,denom2,ese,i,idx,idxx,iter,
!$OMP&    j,jdx,jdxx,l,nk,n,nn,numk,qsmx,qsx,tx)
CMEB ????
      DO 475 L=1,LM
C
      DO J=1,JM
      DO I=1,IM
        NN=(J-1)*IM+I
        TETA(I,J,L)=AMIN1(TETA(I,J,L),350.)
        TETA(I,J,L)=AMAX1(TETA(I,J,L),150.)
        TX(I,J)=TETA(I,J,L)
        IDXX(NN)=I
        JDXX(NN)=J
      ENDDO
      ENDDO
      NUMK=IM*JM
C
      ITER=0
  460 ITER=ITER+1
      NK=NUMK
      NUMK=0
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
        JDX(NN)=JDXX(NN)
      ENDDO
C
cdir$ ivdep
      DO N=1,NK
        I=IDX(N)
        J=JDX(N)
        TETA(I,J,L)=TX(I,J)/(1.+0.608*QETA(I,J,L))
        CLOGES=-CM1/TETA(I,J,L)-CM2*ALOG10(TETA(I,J,L))+CM3
        ESE=10.**(CLOGES+2.)
        DENOM2=ETAL(L)*PD(I,J)+PT-ESE*(1.-EPS)
        QSX=EPS*ESE/DENOM2
        QSMX=0.98*QSX
        IF(QETA(I,J,L).GT.QSMX)THEN
          QETA(I,J,L)=QSMX
          NUMK=NUMK+1
          IDXX(NUMK)=I
          JDXX(NUMK)=J
        ENDIF
      ENDDO
      IF(NUMK.GT.0.AND.ITER.LE.2)GO TO 460
C
  475 CONTINUE
C
c------------------------------------------------------------------
c     DO L=1,LDM
c       HIN(1,L)=0.
c       HIN(2,L)=0.
c       DO J=1,JM
c       DO I=1,IM
c         HIN(1,L)=HIN(1,L)+Q(I,J,L)
c         HIN(2,L)=HIN(2,L)+QETA(I,J,L)
c       ENDDO
c       ENDDO
c     ENDDO
C
c     DO L=1,LDM
c       HIN(1,L)=HIN(1,L)/(IM*JM)
c       HIN(2,L)=HIN(2,L)/(IM*JM)
c       WRITE(6,*)"qsave bar,qeta bar,level",HIN(1,L),HIN(2,L),L
c     ENDDO
C---------------------------------------------------------------
C***
C***  NOW REDEFINE PD TO HAVE IT EQUAL TO PSFC-PT.
C***
C---------------------------------------------------------------
      DO J=1,JM
      DO I=1,IM
        IF(FIX(I,J).NE.1.)THEN
          PD(I,J)=REF(I,J)*PD(I,J)
        ELSEIF(FIX(I,J).EQ.1.)THEN
          PD(I,J)=PSFC(I,J)-PT
        ENDIF
        PHIS(I,J) = G * HGT(I,J)
c       IF(MOD(I,10).EQ.0 .AND. MOD(J,10).EQ.0) THEN
c        PRINT *,'FNL PD ',I,J,PD(I,J),REF(I,J),PINT(I,J,46),
c    1   HGT(I,J),PSFC(I,J),PDORG(I,J)
c       ENDIF
      ENDDO
      ENDDO
C
c------------------------------------------------------------------
C     WRITE(6,*)'TEMPERATURE'
C     DO L=1,LM
C       WRITE(6,*)'L=',L
C       DO I=1,IM
C           WRITE(6,540)(TETA(I,J,L),J=1,21)
c 540       FORMAT(' ',21F6.0)
C       ENDDO
C     ENDDO
C
c     DO J=1,JM
c     DO I=1,IM
c       IF(PD(I,J).GT.105000..OR.PD(I,J).LT.60000.)THEN
c         WRITE(6,540)I,J,PD(I,J)
c 540     FORMAT(' PTETA I,J=',2I4,' PD=',E12.5)
c       ENDIF
c     DO L=1,LM
c       IF(TETA(I,J,L).GT.350..OR.TETA(I,J,L).LT.150.)THEN
c         WRITE(6,550)I,J,L,TETA(I,J,L)
c 550     FORMAT(' PTETA I,J=',2I4,' L=',I2,' T=',E12.5)
c       ENDIF
c       IF(QETA(I,J,L).GT.1.)THEN
c         WRITE(6,555)I,J,L,QETA(I,J,L)
c 555     FORMAT(' PTETA I,J=',2I4,' L=',I2,' Q=',E12.5)
c       ENDIF
c       IF(UETA(I,J,L).GT.150.E0.OR.UETA(I,J,L).LT.-150.E0)THEN
c         WRITE(6,560)I,J,L,UETA(I,J,L)
c 560     FORMAT(' PTETA I,J=',2I4,' L=',I2,' U=',E12.5)
c       ENDIF
c       IF(VETA(I,J,L).GT.150.E0.OR.VETA(I,J,L).LT.-150.E0)THEN
c         WRITE(6,565)I,J,L,VETA(I,J,L)
c 565     FORMAT(' PTETA I,J=',2I4,' L=',I2,' V=',E12.5)
c       ENDIF
c     ENDDO
c     ENDDO
c     ENDDO
c     WRITE(6,570)
c 570 FORMAT(' BEFORE TOPOG = 0')
c     DO L=1,LM
c     DO J=1,JM
c     DO I=1,IM
c       IF(PD(I,J).LT.8000.)THEN
c         WRITE(6,575)I,J,PD(I,J)
c 575     FORMAT(2X,'I,J = ',2I5,' PD(I,J) = ',E12.5)
c       ENDIF
c     ENDDO
c     ENDDO
c     ENDDO
C
C------- SETTING U,V AND T EQUAL TO ZERO AT POINTS BELOW THE GROUND ---
C------- SURFACE ------------------------------------------------------
C
!$OMP PARALLEL DO
      DO 600 L=1,LM
      DO 600 J=1,JM
      DO 600 I=1,IM
      IF(ETA(L+1)-REF(I,J).GT.1.E-6)THEN
c------------------------------------------------------------------
        TETA(I,J,L)=0.
        QETA(I,J,L)=0.
        UETA(I+IHW(J),J,L)=0.
        VETA(I+IHW(J),J,L)=0.
        UETA(I+IHE(J),J,L)=0.
        VETA(I+IHE(J),J,L)=0.
        IF(J.LT.JM) THEN
          UETA(I,J+1,L)=0.
          VETA(I,J+1,L)=0.
        ENDIF
        IF(J.GT.1) THEN
          UETA(I,J-1,L)=0.
          VETA(I,J-1,L)=0.
        ENDIF
      ENDIF
  600 CONTINUE
c------------------------------------------------------------------
c     WRITE(6,625)
c 625   FORMAT(' AFTER TOPOG = 0')
c     DO L=1,LM
c     DO J=1,JM
c     DO I=1,IM
c       IF(I.GT.95.AND.I.LT.100.AND.L.GE.10)THEN 
c         WRITE(6,630) I,J,L,TETA(I,J,L),QETA(I,J,L),
c    &                       UETA(I,J,L),VETA(I,J,L)
c 630     FORMAT(2X,3(I5,1X),4(E12.5,1X))
c       ENDIF
c     ENDDO
c     ENDDO
c     ENDDO
C
c     DO L=1,LM
c       WRITE(6,631)L,ETA(L+1),REF(74,48)
c       WRITE(6,632)L,TETA(1,1,L),QETA(1,1,L)
c       WRITE(6,633)L,TETA(74,48,L),QETA(74,48,L)
c       WRITE(6,644)L,TETA(85,81,L),QETA(85,81,L)
c 631   FORMAT(' PTETA  L=',I2,' ETA(+1)=',E12.5,' REF=',E12.5)
c 632   FORMAT(' L=',I2,' TETA(1,1)=',E12.5,' QETA(1,1)=',E12.5)
c 633   FORMAT(' L=',I2,' TETA(77,48)=',E12.5,' QETA(77,48)=',E12.5)
c 644   FORMAT(' L=',I2,' TETA(85,81)=',E12.5,' QETA(85,81)=',E12.5)
c       WRITE(6,645) L
c 645   FORMAT(2X,' OUT OF PTETA  L = ',I2)
c       DO J=1,JM
c       DO I=1,IM
c         IF(I.EQ.1.OR.I.EQ.100)THEN
c           IF(L.EQ.8)THEN
c             WRITE(6,646)I,J,L,TETA(I,J,L),QETA(I,J,L),UETA(I,J,L),
c    1                      VETA(I,J,L),HETA(I,J,L),HGT(I,J)
c 646         FORMAT(2X,3(I5,1X),6(E12.5,1X))
c           ENDIF
c         ENDIF
c       ENDDO
c       ENDDO
c     ENDDO
C
c     print *,'lm = ',lm
      DO j=1,jM
       DO i=1,iM
         IF(MOD(I,50).EQ.0 .AND. MOD(J,50).EQ.0) THEN
            WRITE(6,647) I,J,REF(I,J),HGT(I,J),PD(I,J),PDORG(I,J)
  647       FORMAT(2X,'I,J(OUTPUT)= ',2I4,4(1X,E12.5))
            DO L = 1, LM
             WRITE(6,648)L,TETA(I,J,L),QETA(I,J,L),UETA(I,J,L),
     1         VETA(I,J,L)
  648        FORMAT(1X,I2,1X,4(E12.5,1X))
            ENDDO
        ENDIF
       ENDDO
      ENDDO
C
C------------- THE SEPARATION OF THE BOUNDARY VALUES -------------------
C
C       SOUTH BOUNDARY - H POINTS
C
      N=1
      DO I=1,IM
        PDB(N,1)=PD(I,1)
        PDB(N,2)=0.
        N=N+1
      ENDDO
C
      DO L=1,LM
        N=1
        DO I=1,IM
          TB(N,L,1)=TETA(I,1,L)
          TB(N,L,2)=0.
          QB(N,L,1)=QETA(I,1,L)
          QB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
C       NORTH BOUNDARY - H POINTS
C
      N=IM+1
      DO I=1,IM
        PDB(N,1)=PD(I,JM)
        PDB(N,2)=0.
        N=N+1
      ENDDO
C
      DO L=1,LM
        N=IM+1
        DO I=1,IM
          TB(N,L,1)=TETA(I,JM,L)
          TB(N,L,2)=0.
          QB(N,L,1)=QETA(I,JM,L)
          QB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
C       WEST BOUNDARY - H POINTS
C
      N=2*IM+1
      DO J=3,JM-2,2
        PDB(N,1)=PD(1,J)
        PDB(N,2)=0.
        N=N+1
      ENDDO
C
      DO L=1,LM
        N=2*IM+1
        DO J=3,JM-2,2
          TB(N,L,1)=TETA(1,J,L)
          TB(N,L,2)=0.
          QB(N,L,1)=QETA(1,J,L)
          QB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
C       EAST BOUNDARY - H POINTS
C
      N=2*IM+(JM+1)/2-1
      DO J=3,JM-2,2
        PDB(N,1)=PD(IM,J)
        PDB(N,2)=0.
        N=N+1
      ENDDO
C
      DO L=1,LM
        N=2*IM+(JM+1)/2-1
        DO J=3,JM-2,2
          TB(N,L,1)=TETA(IM,J,L)
          TB(N,L,2)=0.
          QB(N,L,1)=QETA(IM,J,L)
          QB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
C       SOUTH BOUNDARY - V POINTS
C
      DO L=1,LM
        N=1
        DO I=1,IM-1
          UB(N,L,1)=UETA(I,1,L)
          UB(N,L,2)=0.
          VB(N,L,1)=VETA(I,1,L)
          VB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
C       NORTH BOUNDARY - V POINTS
C
      DO L=1,LM
        N=IM
        DO I=1,IM-1
          UB(N,L,1)=UETA(I,JM,L)
          UB(N,L,2)=0.
          VB(N,L,1)=VETA(I,JM,L)
          VB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
C       WEST BOUNDARY - V POINTS
C
      DO L=1,LM
        N=2*IM-1
        DO J=2,JM-1,2
          UB(N,L,1)=UETA(1,J,L)
          UB(N,L,2)=0.
          VB(N,L,1)=VETA(1,J,L)
          VB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
C       EAST BOUNDARY - V POINTS
C
      DO L=1,LM
        N=2*IM-2+JM/2+1
        DO J=2,JM-1,2
          UB(N,L,1)=UETA(IM,J,L)
          UB(N,L,2)=0.
          VB(N,L,1)=VETA(IM,J,L)
          VB(N,L,2)=0.
          N=N+1
        ENDDO
      ENDDO
C
      REWIND LOUT
      RUN=.TRUE.
      NTSD=0
      WRITE(LOUT)RUN,IDAT,IHRST,NTSD,UETA,VETA,WET,SST,SI
      WRITE(LOUT)TETA,QETA,PD,PHIS,SM,REF,ETA,PT,DETA,ETAL,ZETA
     1,          PDB,TB,QB,UB,VB
C
      DEALLOCATE(TETA)
      DEALLOCATE(UETA)
      DEALLOCATE(VETA)
      DEALLOCATE(QETA)
      RETURN
      END
