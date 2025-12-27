      SUBROUTINE SLP(NHB,PD,RES,FIS,T,Q,NTSD,NEST,PSLP)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  SLP         SLP REDUCTION
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-04-22
C
C ABSTRACT:  THIS ROUTINE COMPUTES THE SEA LEVEL PRESSURE
C            REDUCTION USING EITHER THE MESINGER RELAXATION
C            METHOD OR THE STANDARD NCEP REDUCTION
C
C PROGRAM HISTORY LOG:
C   99-04-22  T BLACK - ORIGINATOR
C   00-01-10  JIM TUCCILLO - MPI VERSION
C   00-11-02  T BLACK - SLP FOR NEST BOUNDARIES
C
C USAGE:  CALL SLP FROM PROGRAM POST0
C
C   INPUT ARGUMENT LIST:
C     NHB  - UNIT NUMBER FOR READING THE NHB FILE
C     PD   - SFC PRESSURE MINUS PTOP
C     RES  - RECIPROCAL OF ETA AT THE GROUND
C     FIS  - SURFACE GEOPOTENTIAL
C     T    - TEMPERATURE 
C     Q    - SPECIFIC HUMIDITY
C     NTSD - THE TIMESTEP
C     NEST - IF A NESTED RUN THEN NEST IS .TRUE.
C
C   OUTPUT ARGUMENT LIST:
C     PSLP - THE FINAL REDUCED SEA LEVEL PRESSURE ARRAY
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C             NONE
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "PARA.comm"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
                            P A R A M E T E R
     & (LP1=LM+1,IMJM=IM*JM-JM/2,JM2=JM-2,KBI=2*IM+JM-3,KBI2=KBI-4)
                            P A R A M E T E R
     & (NRLX1=500,NRLX2=100,KSLPD=1
cwas &, OVERRC=1.75,AD05=OVERRC*0.05,CFT0=OVERRC-1.
     &, OVERRC=1.50,AD05=OVERRC*0.05,CFT0=OVERRC-1.
     &, ROG=287.04/9.8)
C-----------------------------------------------------------------------
C 
       REAL,ALLOCATABLE :: HTM(:,:,:)
C
                            R E A L
     & TTV   (IM,MY_JSD:MY_JED),
     & PDSL1 (IM,MY_JSD:MY_JED),
     & PBI   (IM,MY_JSD:MY_JED),
     & SLPX  (IM,MY_JSD:MY_JED)
                            R E A L
     & DETA(LM),RDETA(LM),AETA(LM),F4Q2(LM),ETA(LP1),DFL(LP1)
                            R E A L
     & TSLPB(KBI,LM),TSLPB2(KBI2,LM)
     &,PSLPB(KBI),PSLPB2(KBI2)
C-----------------------------------------------------------------------
                            I N T E G E R
     & KMNTM,LMH(IM,JM)
                            I N T E G E R
     & IMNT(IMJM),JMNT(IMJM)
                            I N T E G E R
     & IHE(JM),IHW(JM),IVE(JM),IVW(JM)
C-----------------------------------------------------------------------
                            L O G I C A L
     & SIGMA,STDRD,NO_REDUCE,NEST,EXBC
C-----------------------------------------------------------------------
C
       REAL PD  (IM,MY_JSD:MY_JED)
       REAL RES (IM,MY_JSD:MY_JED)
       REAL FIS (IM,MY_JSD:MY_JED)
       REAL PSLP(IM,MY_JSD:MY_JED)
       REAL T   (IM,MY_JSD:MY_JED,LM)
       REAL Q   (IM,MY_JSD:MY_JED,LM)
C
       REAL DUM(IM,JM),BCDUM(2*KBI*(6*LM+1))
C-----------------------------------------------------------------------
C
       LOGICAL LFRST
       DATA LFRST/.TRUE./
C-----------------------------------------------------------------------
C
       SAVE
C
C-----------------------------------------------------------------------
C
      IF(LFRST)THEN
        LFRST=.FALSE.
C
C***
C***  READ IN THE ARRAYS AND CONSTANTS THAT ARE NEEDED
C***
        REWIND NHB
C
        READ(NHB)NFCST,NBC,LIST,DT,IDTAD,SIGMA
        READ(NHB)LMH
        READ(NHB)LMV
        READ(NHB)
        READ(NHB)
        READ(NHB)
        READ(NHB)
        READ(NHB)
C
        STDRD=.FALSE.
        NO_REDUCE=.FALSE.
        IF(SIGMA)STDRD=.TRUE.
C
C-----------------------------------------------------------------------
C
C***  FIND THE MOST ELEVATED GLOBAL LAYER WHERE THERE IS LAND
C
C-----------------------------------------------------------------------
        DO L=1,LM
cwas      READ(NHB)((HTM(I,J,L),I=1,IM),J=1,JM)
          READ(NHB)((DUM(I,J),I=1,IM),J=1,JM)
C
          DO J=JSTA_I,JEND_I
          DO I=1,IM
            IF(DUM(I,J).LT.0.5)GO TO 666
          ENDDO
          ENDDO
C
C***  IF WE GET TO HERE, WE HAVE ALL ATMOSPHERE
C
          GOTO 667
  666     CONTINUE
C
C***  IF WE GET HERE, WE HAVE FOUND A NON_ATM POINT
C
          LHMNT=L
          GOTO 668
  667     CONTINUE
        ENDDO
C
        LHMNT=LM+1
ccccccc GO TO 669
  668   CONTINUE
C
        CALL MPI_ALLREDUCE
     1   (LHMNT,LXXX,1,MPI_INTEGER,MPI_MIN,MPI_COMM_COMP,IERR)
C-----------------------------------------------------------------------
C
        LREC=MIN(LHMNT,LM)
        DO I=1,LREC-LXXX+1
          BACKSPACE NHB
        ENDDO
C
        LHMNT=LXXX
        ALLOCATE(HTM(IM,MY_JSD:MY_JED,LHMNT:LM))
C
        DO L=LHMNT,LM
          READ(NHB)((DUM(I,J),I=1,IM),J=1,JM)
          DO J=MY_JSD,MY_JED
          DO I=1,IM
            HTM(I,J,L)=DUM(I,J)
          ENDDO
          ENDDO
        ENDDO
C
  669   CONTINUE
C
        DO L=1,LM
          READ(NHB)
        ENDDO
C
        READ(NHB)DY,CPGFV,EN,ENT,R,PT,TDDAMP  
     1,          F4D,F4Q,EF4T,DETA,RDETA,AETA,F4Q2,ETA,DFL
C
C-----------------------------------------------------------------------
      ENDIF      ! END OF LFRST IF BLOCK
C-----------------------------------------------------------------------
C***
C***  CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C***
      DO J=1,JM
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
        IVE(J)=MOD(J,2)
        IVW(J)=IVE(J)-1
      ENDDO
C-----------------------------------------------------------------------
C***
C***  INITIALIZE ARRAYS.  LOAD SLP ARRAY WITH SURFACE PRESSURE.
C***
      DO J=JSTA_I,JEND_I
      DO I=1,IM
        PSLP(I,J)=0.
        TTV(I,J)=0.
      ENDDO
      ENDDO
C
      DO 110 J=JSTA_I,JEND_I
      DO 110 I=1,IM
      PDSL1(I,J)=RES(I,J)*PD(I,J)
      PSLP(I,J)=PD(I,J)+PT
      PBI (I,J)=PSLP(I,J)
  110 CONTINUE
C
C***  CALCULATE SEA LEVEL PRESSURE FOR PROFILES (AND POSSIBLY
C***  FOR POSTING BY POST PROCESSOR).
C
C***  "STDRD" REFERS TO THE "STANDARD" SLP REDUCTION SCHEME.
C***  THIS IS THE ONLY SCHEME AT PRESENT AVAILABLE FOR A SIGMA=.TRUE.
C***  ETA MODEL RUN.
C
  220 IF(LHMNT.EQ.LP1)THEN
        NO_REDUCE=.TRUE.
      ENDIF
C***
C
      IF(NO_REDUCE)GO TO 430
      IF(STDRD)GO TO 400
C
      LL=LM
C
      DO J=JSTA_IM2,JEND_IM2
      DO I=2,IM-1
        IF(HTM(I,J,LL).LE.0.5) THEN
          TGSS=FIS(I,J)/(R*ALOG((PDSL1(I,J)+PT)/(PD(I,J)+PT)))
          LMAP1=LMH(I,J)+1
C
          DO 260 L=LMAP1,LM
          T(I,J,L)=TGSS
  260     CONTINUE
C
        ENDIF
      ENDDO 
      ENDDO
C----------------------------------------------------------------
C***
C***  IF THIS IS A NESTED RUN, READ IN THE PARENT TEMPERATURE
C***  VALUES (INTERPOLATED THE NEST VERTICAL DISTRIBUTION)
C***  FOR THE SLP RELAXATION PROCEDURE
C***
      IF(NEST)THEN
        IF(ME.EQ.0)THEN
          LRECBC=4*(1+(1+6*LM)*KBI*2+(KBI+KBI2)*(LM+1))
          OPEN(UNIT=NBC,ACCESS='DIRECT',RECL=LRECBC)
C
          NREC=NINT((NTSD-1)*DT/3600.)+2
          READ(NBC,REC=NREC)BCHR,BCDUM
     1,                     TSLPB,TSLPB2,PSLPB,PSLPB2
          CLOSE(NBC)
        ENDIF
C
        CALL MPI_BCAST(TSLPB,KBI*LM,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(TSLPB2,KBI2*LM,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(PSLPB,KBI,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(PSLPB2,KBI2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C
      ENDIF
C----------------------------------------------------------------
C
C***  CREATE A TEMPORARY TV ARRAY, AND FOLLOW BY SEQUENTIAL
C***  OVERRELAXATION, DOING NRLX PASSES.
C
C----------------------------------------------------------------
      NRLX=NRLX1
C
C----------------------------------------------------------------
C----------------------------------------------------------------
      DO 300 L=LHMNT,LM
C----------------------------------------------------------------
C----------------------------------------------------------------
C
      KMN=0
      KMNTM=0
C
      DO 240 J=JSTA_IM2,JEND_IM2
      DO 240 I=2,IM-1
      IF(HTM(I,J,L).GT.0.5)GO TO 240
      KMN=KMN+1
      IMNT(KMN)=I
      JMNT(KMN)=J
  240 CONTINUE
C
      KMNTM=KMN
C
      DO 270 J=JSTA_I,JEND_I
      DO 270 I=1,IM
      TTV(I,J)=T(I,J,L)
  270 CONTINUE
C
C----------------------------------------------------------------
C***  FOR GRID BOXES NEXT TO MOUNTAINS REPLACE TTV BY AN "EQUIVALENT"
C***  TV, ONE WHICH CORRESPONDS TO THE CHANGE IN P BETWEEN REFERENCE
C***  INTERFACE GEOPOTENTIALS, INSTEAD OF BETWEEN LAYER INTERFACES
C----------------------------------------------------------------
C
      DO J=JSTA_IM2,JEND_IM2
      DO I=2,IM-1
        IF(HTM(I,J,L).GT.0.5.AND.
     1     HTM(I+IHW(J),J-1,L)*HTM(I+IHE(J),J-1,L)
     2    *HTM(I+IHW(J),J+1,L)*HTM(I+IHE(J),J+1,L)
     3    *HTM(I-1     ,J  ,L)*HTM(I+1     ,J  ,L)
     4    *HTM(I       ,J-2,L)*HTM(I       ,J+2,L).LT.0.5)THEN
          LMST=LMH(I,J)
C***
C***  FIND P AT THE REFERENCE INTERFACE GEOPOTENTIAL AT THE BOTTOM
C***
          PBIN=PT+PD(I,J)
          PHBI=DFL(LMST+1)
C
          DO LI=LMST,1,-1
            PTIN=PBIN-DETA(LI)*PD(I,J)*RES(I,J)
            TRTV=2.*R*T(I,J,LI)*(1.+0.608*Q(I,J,LI))
            PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
            IF(PHTI.GE.DFL(L+1))GO TO 273
            PBIN=PTIN
            PHBI=PHTI
          ENDDO
C
  273     DPOSP=(PHTI-DFL(L+1))/TRTV
          PRBIN=(1.+DPOSP)/(1.-DPOSP)*PTIN
C***
C***  FIND P AT THE REFERENCE INTERFACE GEOPOTENTIAL AT THE TOP
C***
          PBIN=PT+PD(I,J)
          PHBI=DFL(LMST+1)
C
          DO LI=LMST,1,-1
            PTIN=PBIN-DETA(LI)*PD(I,J)*RES(I,J)
            TRTV=2.*R*T(I,J,LI)*(1.+0.608*Q(I,J,LI))
            PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
            IF(PHTI.GE.DFL(L))GO TO 275
            PBIN=PTIN
            PHBI=PHTI
          ENDDO
C
  275     DPOSP=(PHTI-DFL(L))/TRTV
          PRTIN=(1.+DPOSP)/(1.-DPOSP)*PTIN
C
          TTV(I,J)=(DFL(L)-DFL(L+1))/(2.*R)*(PRBIN+PRTIN)/(PRBIN-PRTIN)
        ENDIF
      ENDDO
      ENDDO
C----------------------------------------------------------------
C***
C***  FOR POINTS IN THE OUTER TWO BOUNDARY ROWS THAT ARE NEXT TO
C***  MOUNTAINS, SET TTVs EQUAL TO THE TEMPERATURES DERIVED
C***  EARLIER FROM THE PARENT GRID'S SLP
C***  AND SET PSLP EQUAL TO THE PARENT'S SLP
C----------------------------------------------------------------
      IF(NEST)THEN
C
C***  FIRST DO THE OUTER BOUNDARY ROW
C
        N=1
C
        DO I=1,IM    
C
          IF(JSTA_I.EQ.1)THEN          ! Southern Edge
            IF(HTM(I,3,L).LT.0.5)THEN
              TTV(I,1)=TSLPB(N,L)
            ENDIF
C 
            IF(L.EQ.LM)PSLP(I,1)=PSLPB(N)
          ENDIF
          N=N+1
        ENDDO
C
        DO I=1,IM   
C
          IF(JEND_I.EQ.JM)THEN         ! Northern Edge
            IF(HTM(I,JM-2,L).LT.0.5)THEN
              TTV(I,JM)=TSLPB(N,L)
            ENDIF
C
            IF(L.EQ.LM)PSLP(I,JM)=PSLPB(N)
          ENDIF
          N=N+1
        ENDDO
C
        DO J=3,JM-2,2   ! Western Edge
C
          IF(J.GE.JSTA_I.AND.J.LE.JEND_I)THEN
            IF(HTM(3,J,L).LT.0.5)THEN
              TTV(1,J)=TSLPB(N,L)
            ENDIF
C
            IF(L.EQ.LM)PSLP(1,J)=PSLPB(N)
          ENDIF
          N=N+1
        ENDDO
C
        DO J=3,JM-2,2   ! Eastern Edge
C
          IF(J.GE.JSTA_I.AND.J.LE.JEND_I)THEN
            IF(HTM(IM-2,J,L).LT.0.5)THEN
              TTV(IM,J)=TSLPB(N,L)
            ENDIF
C
            IF(L.EQ.LM)PSLP(IM,J)=PSLPB(N)
          ENDIF
          N=N+1
        ENDDO
C
C***  NOW DO THE INNER 2ND (INNER) BOUNDARY ROW
C
        N=1
        DO I=1,IM-1   ! 1st Row From Southern Edge  
C
          IF(JSTA_I.EQ.1)THEN   
            IF(HTM(I,3,L).LT.0.5.OR.HTM(I+1,3,L).LT.0.5)THEN
              TTV(I,2)=TSLPB2(N,L)
            ENDIF
C
            IF(L.EQ.LM)PSLP(I,2)=PSLPB2(N)
          ENDIF
          N=N+1
        ENDDO
C
        DO I=1,IM-1   ! 1st Row From Northern Edge  
C
          IF(JEND_I.EQ.JM)THEN   
            IF(HTM(I,JM-2,L).LT.0.5.OR.HTM(I+1,JM-2,L).LT.0.5)THEN
              TTV(I,JM-1)=TSLPB2(N,L)
            ENDIF
C
            IF(L.EQ.LM)PSLP(I,JM-1)=PSLPB2(N)
          ENDIF
          N=N+1
        ENDDO
C
        DO J=4,JM-3,2   ! 1st Row From Western Edge
C
          IF(J.GE.JSTA_I.AND.J.LE.JEND_I)THEN
            IF(HTM(2,J-1,L).LT.0.5.OR.HTM(2,J+1,L).LT.0.5)THEN
              TTV(1,J)=TSLPB2(N,L)
            ENDIF
C
            IF(L.EQ.LM)PSLP(1,J)=PSLPB2(N)
          ENDIF
          N=N+1
        ENDDO
C
        DO J=4,JM-3,2   ! 1st Row From Eastern Edge
C
          IF(J.GE.JSTA_I.AND.J.LE.JEND_I)THEN
            IF(HTM(IM-1,J-1,L).LT.0.5.OR.HTM(IM-1,J+1,L).LT.0.5)THEN
              TTV(IM-1,J)=TSLPB2(N,L)
            ENDIF
C
            IF(L.EQ.LM)PSLP(IM-1,J)=PSLPB2(N)
          ENDIF
          N=N+1
        ENDDO
C
      ENDIF   ! End of NEST Block
C----------------------------------------------------------------
      KMM=KMNTM
C----------------------------------------------------------------
C***
C***  HERE IS THE RELAXATION LOOP
C***
C----------------------------------------------------------------
      DO 285 N=1,NRLX
C
      CALL UPDATE(TTV)   ! Exchange haloes
C
      DO 280 KM=1,KMM
      I=IMNT(KM)
      J=JMNT(KM)
      TTV(I,J)=AD05*(4.*(TTV(I+IHW(J),J-1)+TTV(I+IHE(J),J-1)
     1                  +TTV(I+IHW(J),J+1)+TTV(I+IHE(J),J+1))
     2                  +TTV(I-1,J)       +TTV(I+1,J)
     3                  +TTV(I,J-2)       +TTV(I,J+2))
     4                  -CFT0*TTV(I,J)

  280 CONTINUE
C
  285 CONTINUE
C----------------------------------------------------------------
C
      DO 290 KM=1,KMM
      I=IMNT(KM)
      J=JMNT(KM)
      T(I,J,L)=TTV(I,J)
  290 CONTINUE
C
  300 CONTINUE
C----------------------------------------------------------------
C***
C***  CALCULATE THE SEA LEVEL PRESSURE AS PER THE NEW SCHEME.
C***
C     VALUES FOR IMNT AND JMNT ARE FOR LAYER LM - THIS IS WHAT WE WANT
      KMM=KMNTM
C
      DO 320 KM=1,KMM
      I=IMNT(KM)
      J=JMNT(KM)
      LMAP1=LMH(I,J)+1
      PBIN=PT+PD(I,J)
C
      DO L=LMAP1,LM
        PTIN=PBIN
        DPOSP=(DFL(L)-DFL(L+1))/(2.*R*T(I,J,L))
        PBIN=(1.+DPOSP)/(1.-DPOSP)*PTIN
      ENDDO
C
      PSLP(I,J)=PBIN
  320 CONTINUE
C--------------------------------------------------------------------
C     SKIP THE STANDARD SCHEME.
C--------------------------------------------------------------------
      GO TO 430
C--------------------------------------------------------------------
C***
C***  IF YOU WANT THE "STANDARD" ETA/SIGMA REDUCTION
C***  THIS IS WHERE IT IS DONE.
C***
  400 CONTINUE
C
      DO 410 J=JSTA_I,JEND_I
      DO 410 I=1,IM
      IF(FIS(I,J).GE.1.)THEN
        LMA=LMH(I,J)
        ALPP1=ALOG(PDSL1(I,J)*ETA(LMA+1)+PT)
        SLOP=0.0065*ROG*T(I,J,LMA)
        IF(SLOP.LT.0.50)THEN
          SLPP=ALPP1+FIS(I,J)/(R*T(I,J,LMA))
        ELSE
          TTT=-(ALOG(PDSL1(I,J)*ETA(LMA)+PT)+ALPP1)
     1         *SLOP*0.50+T(I,J,LMA)
          SLPP=(-TTT+SQRT(TTT*TTT+2.*SLOP*
     1          (FIS(I,J)/R+
     2          (TTT+0.50*SLOP*ALPP1)*ALPP1)))/SLOP
        ENDIF
        PSLP(I,J)=EXP(SLPP)
      ENDIF
  410 CONTINUE
C
C****************************************************************
C     AT THIS POINT WE HAVE A SEA LEVEL PRESSURE FIELD BY
C     EITHER METHOD.  5-POINT AVERAGE THE FIELD ON THE E-GRID.
C****************************************************************
C
  430 CONTINUE
C
      DO 440 J=JSTA_I,JEND_I
      DO 440 I=1,IM
      SLPX(I,J)=PSLP(I,J)
  440 CONTINUE
C
      DO 480 KS=1,KSLPD
C
      CALL UPDATE(PSLP)    ! Exchange haloes
C
      DO 460 J=JSTA_IM2,JEND_IM2
      IHH2=IM-1-MOD(J+1,2)
      DO 460 I=2,IHH2
C
C***  EXTRA AVERAGING UNDER MOUNTAINS TAKEN OUT, FM, MARCH 96
C
      SLPX(I,J)=0.125*(PSLP(I+IHW(J),J-1)+PSLP(I+IHE(J),J-1)
     1                +PSLP(I+IHW(J),J+1)+PSLP(I+IHE(J),J+1)
     2                +4.*PSLP(I,J))
  460 CONTINUE
C
      DO J=JSTA_I,JEND_I
      DO I=1,IM
        PSLP(I,J)=SLPX(I,J)
      ENDDO
      ENDDO
C
  480 CONTINUE
C
      END
