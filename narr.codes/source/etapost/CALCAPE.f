      SUBROUTINE CALCAPE(ITYPE,P1D,T1D,Q1D,L1D,CAPE,CINS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALCAPE     COMPUTES CAPE AND CINS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-02-10      
C     
C ABSTRACT:  
C     
C     THIS ROUTINE COMPUTES CAPE AND CINS GIVEN TEMPERATURE,
C     PRESSURE, AND SPECIFIC HUMIDTY.  IN "STORM AND CLOUD 
C     DYNAMICS" (1989, ACADEMIC PRESS) COTTON AND ANTHES DEFINE
C     CAPE (EQUATION 9.16, P501) AS
C
C                  EL
C         CAPE =  SUM G * LN(THETAP/THETAA) DZ 
C                 LCL
C     
C     WHERE,
C      EL    = EQUILIBRIUM LEVEL,
C     LCL    = LIFTING CONDENSTATION LEVEL,
C       G    = GRAVITATIONAL ACCELERATION,
C     THETAP = LIFTED PARCEL POTENTIAL TEMPERATURE,
C     THETAA = AMBIENT POTENTIAL TEMPERATURE.
C     
C     NOTE THAT THE INTEGRAND LN(THETAP/THETAA) APPROXIMATELY
C     EQUALS (THETAP-THETAA)/THETAA.  THIS RATIO IS OFTEN USED
C     IN THE DEFINITION OF CAPE/CINS.
C     
C     TWO TYPES OF CAPE/CINS CAN BE COMPUTED BY THIS ROUTINE.  THE
C     SUMMATION PROCESS IS THE SAME FOR BOTH CASES.  WHAT DIFFERS
C     IS THE DEFINITION OF THE PARCEL TO LIFT.  FOR ITYPE=1 THE
C     PARCEL WITH THE WARMEST THETA-E IN A DPBND PASCAL LAYER ABOVE
C     THE MODEL SURFACE IS LIFTED.  THE ARRAYS P1D, T1D, Q1D, AND
C     L1D ARE NOT USED.  FOR ITYPE=2 THE ARRAYS P1D, T1D, Q1D, AND
C     L1D DEFINE THE PARCEL TO LIFT IN EACH COLUMN.  BOTH TYPES OF
C     CAPE/CINS MAY BE COMPUTED IN A SINGLE EXECUTION OF THE POST
C     PROCESSOR.
C     
C     THIS ALGORITHM PROCEEDS AS FOLLOWS.
C     FOR EACH COLUMN, 
C        (1)  INITIALIZE RUNNING CAPE AND CINS SUM TO 0.0
C        (2)  COMPUTE TEMPERATURE AND PRESSURE AT THE LCL USING
C             LOOK UP TABLE (PTBL).  USE EITHER PARCEL THAT GIVES
C             MAX THETAE IN LOWEST DPBND ABOVE GROUND (ITYPE=1)
C             OR GIVEN PARCEL FROM T1D,Q1D,...(ITYPE=2).
C        (3)  COMPUTE THE TEMP OF A PARCEL LIFTED FROM THE LCL.
C             WE KNOW THAT THE PARCEL'S
C             EQUIVALENT POTENTIAL TEMPERATURE (THESP) REMAINS
C             CONSTANT THROUGH THIS PROCESS.  WE CAN
C             COMPUTE TPAR USING THIS KNOWLEDGE USING LOOK
C             UP TABLE (SUBROUTINE TTBLEX).
C        (4)  FIND THE EQUILIBRIUM LEVEL.  THIS IS DEFINED AS THE
C             HIGHEST POSITIVELY BUOYANT LAYER.
C             (IF THERE IS NO POSITIVELY BUOYANT LAYER, CAPE/CINS
C              WILL BE ZERO)
C        (5)  COMPUTE CAPE/CINS.  
C             (A) COMPUTE THETAP.  WE KNOW TPAR AND P.
C             (B) COMPUTE THETAA.  WE KNOW T AND P.  
C        (6)  ADD G*(THETAP-THETAA)*DZ TO THE RUNNING CAPE OR CINS SUM.
C             (A) IF THETAP > THETAA, ADD TO THE CAPE SUM.
C             (B) IF THETAP < THETAA, ADD TO THE CINS SUM.
C        (7)  ARE WE AT EQUILIBRIUM LEVEL? 
C             (A) IF YES, STOP THE SUMMATION.
C             (B) IF NO, CONTIUNUE THE SUMMATION.
C        (8)  ENFORCE LIMITS ON CAPE AND CINS (I.E. NO NEGATIVE CAPE)
C     
C PROGRAM HISTORY LOG:
C   93-02-10  RUSS TREADON
C   93-06-19  RUSS TREADON - GENERALIZED ROUTINE TO ALLOW FOR 
C                            TYPE 2 CAPE/CINS CALCULATIONS.     
C   94-09-23  MIKE BALDWIN - MODIFIED TO USE LOOK UP TABLES
C                            INSTEAD OF COMPLICATED EQUATIONS.
C   94-10-13  MIKE BALDWIN - MODIFIED TO CONTINUE CAPE/CINS CALC
C                            UP TO AT HIGHEST BUOYANT LAYER.
C   98-06-12  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-18  T BLACK      - COMPUTE APE INTERNALLY
C   00-01-04  JIM TUCCILLO - MPI VERSION              
C     
C USAGE:    CALL CALCAPE(ITYPE,P1D,T1D,Q1D,L1D,CAPE,CINS)
C   INPUT ARGUMENT LIST:
C     ITYPE    - INTEGER FLAG SPECIFYING HOW PARCEL TO LIFT IS
C                IDENTIFIED.  SEE COMMENTS ABOVE.
C     P1D      - ARRAY OF PRESSURE OF PARCELS TO LIFT.
C     T1D      - ARRAY OF TEMPERATURE OF PARCELS TO LIFT.
C     Q1D      - ARRAY OF SPECIFIC HUMIDITY OF PARCELS TO LIFT.
C     L1D      - ARRAY OF MODEL LEVEL OF PARCELS TO LIFT.
C
C   OUTPUT ARGUMENT LIST: 
C     CAPE     - CONVECTIVE AVAILABLE POTENTIAL ENERGY (J/KG)
C     CINS     - CONVECTIVE INHIBITION (J/KG)
C     
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       P2FILT   - SMOOTH DATA ON STAGGERED E-GRID.
C       BOUND    - BOUND (CLIP) DATA BETWEEN UPPER AND LOWER LIMTS.
C       TTBLEX   - LOOKUP TABLE ROUTINE TO GET T FROM THETAE AND P
C
C     LIBRARY:
C       COMMON   - VRBLS
C                  PHYS
C                  EXTRA
C                  IOUNIT
C                  LOOPS
C                  MASKS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C
C     
C     
C     INCLUDE/SET PARAMETERS.  CONSTANTS ARE FROM BOLTON (MWR, 1980).
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "params"
      PARAMETER (DPBND=70.E2,
     & ELIVW=2.72E6,ELOCP=ELIVW/CP)
      PARAMETER (ISMTHP=2,ISMTHT=2,ISMTHQ=2)
C     
C     DECLARE VARIABLES.
C     
      INTEGER L1D(IM,JM),IEQL(IM,JM),IPTB(IM,JM),ITHTB(IM,JM)
      INTEGER KLRES(IM,JM),KHRES(IM,JM),LCL(IM,JM)
C      INTEGER KNUML(LM),KNUMH(LM)
C     
      REAL P1D(IM,JM),T1D(IM,JM),Q1D(IM,JM)
      REAL CAPE(IM,JM),CINS(IM,JM)
      REAL THESP(IM,JM),TPAR(IM,JM,LM),PSP(IM,JM)
      REAL QQ(IM,JM),PP(IM,JM)
      INTEGER IDX(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "VRBLS.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "CTLBLK.comm"
C
C**************************************************************
C     START CALCAPE HERE.
C     
C
C     COMPUTE CAPE/CINS
C
C        WHICH IS: THE SUM FROM THE LCL TO THE EQ LEVEL OF
C             G * (LN(THETAP) - LN(THETAA)) * DZ
C
C             (POSITIVE AREA FOR CAPE, NEGATIVE FOR CINS)
C
C        WHERE:
C             THETAP IS THE PARCEL THETA
C             THETAA IS THE AMBIENT THETA
C             DZ IS THE THICKNESS OF THE LAYER
C
C         USING LCL AS LEVEL DIRECTLY BELOW SATURATION POINT
C         AND EQ LEVEL IS THE HIGHEST POSITIVELY BUOYANT LEVEL.
C  
C         IEQL = EQ LEVEL
C
C     INITIALIZE CAPE AND CINS ARRAYS
C 
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        CAPE(I,J) = D00
        CINS(I,J) = D00
        LCL(I,J)  = D00
        THESP(I,J)= D00
        IEQL(I,J) = LM+1
      ENDDO
      ENDDO
C
!$omp  parallel do
      DO L=1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          TPAR(I,J,L)= D00
        ENDDO
        ENDDO
      ENDDO
C     
C     FOR TYPE 2 CAPE/CINS SMOOTH DATA PRIOR TO CALCULATION.
C     NOTE THAT FOR TYPE 1 CAPE/CINS ARRAYS P1D, T1D, Q1D 
C     ARE DUMMY ARRAYS.
C     
      IF (ITYPE.EQ.2) THEN
         CALL P2FILT(ISMTHP,HBM2,P1D)
         CALL P2FILT(ISMTHT,HBM2,T1D)
         CALL P2FILT(ISMTHQ,HBM2,Q1D)
         CALL BOUNDL(Q1D,H1M12,H99999,IM,JM)
      ENDIF
C-------FOR ITYPE=1-----------------------------------------------------
C---------FIND MAXIMUM THETA E LAYER IN LOWEST DPBND ABOVE GROUND-------
C-------FOR ITYPE=2-----------------------------------------------------
C---------FIND THETA E LAYER OF GIVEN T1D, Q1D, P1D---------------------
C--------------TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES-------------------
      DO 20 KB=1,LM
       IF (ITYPE.EQ.2.AND.KB.GT.1) GOTO 20
!$omp  parallel do
!$omp& private(apebtk,apespk,bqk,bqs00k,bqs10k,iq,it,ittbk,lmhk,
!$omp&         p00k,p01k,p10k,p11k,pkl,psfck,qbtk,sqk,sqs00k,
!$omp&         sqs10k,tbtk,tpspk,tqk,tthbtk,tthesk,tthk)
       DO 10 J=JSTA_M,JEND_M
       DO 10 I=2,IM-1
        LMHK   =LMH(I,J)
        PSFCK  =AETA(LMHK)*PDSL(I,J)+PT
          PKL = AETA(KB)*PDSL(I,J)+PT
          IF (ITYPE.EQ.1.AND.(PKL.LT.PSFCK-DPBND.OR.PKL.GT.PSFCK))
     &             GOTO 10
          IF (ITYPE.EQ.1) THEN
            TBTK   =T(I,J,KB)
            QBTK   =Q(I,J,KB)
            APEBTK =(H10E5/(PDSL(I,J)*AETA(KB)+PT))**CAPA
          ELSE
            PKL    =P1D(I,J)
            TBTK   =T1D(I,J)
            QBTK   =Q1D(I,J)
            APEBTK =(H10E5/PKL)**CAPA
          ENDIF
C--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX--------------
          TTHBTK =TBTK*APEBTK
          TTHK   =(TTHBTK-THL)*RDTH
          QQ(I,J)=TTHK-AINT(TTHK)
          ITTBK  =INT(TTHK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
          IF(ITTBK.LT.1)    THEN
            ITTBK  =1
            QQ(I,J)=D00
          ENDIF
          IF(ITTBK.GE.JTB)  THEN
            ITTBK  =JTB-1
            QQ(I,J)=D00
          ENDIF
C--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
          BQS00K=QS0(ITTBK)
          SQS00K=SQS(ITTBK)
          BQS10K=QS0(ITTBK+1)
          SQS10K=SQS(ITTBK+1)
C--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
          BQK    =(BQS10K-BQS00K)*QQ(I,J)+BQS00K
          SQK    =(SQS10K-SQS00K)*QQ(I,J)+SQS00K
          TQK    =(QBTK-BQK)/SQK*RDQ
          PP(I,J)=TQK-AINT(TQK)
          IQ     =INT(TQK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
          IF(IQ.LT.1)    THEN
            IQ     =1
            PP(I,J)=D00
          ENDIF
          IF(IQ.GE.ITB)  THEN
            IQ     =ITB-1
            PP(I,J)=D00
          ENDIF
C--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
          IT=ITTBK
          P00K=PTBL(IQ  ,IT  )
          P10K=PTBL(IQ+1,IT  )
          P01K=PTBL(IQ  ,IT+1)
          P11K=PTBL(IQ+1,IT+1)
C--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
          TPSPK=P00K+(P10K-P00K)*PP(I,J)+(P01K-P00K)*QQ(I,J)
     2      +(P00K-P10K-P01K+P11K)*PP(I,J)*QQ(I,J)
          APESPK=(H10E5/TPSPK)**CAPA
          TTHESK=TTHBTK*EXP(ELOCP*QBTK*APESPK/TTHBTK)
C--------------CHECK FOR MAXIMUM THETA E--------------------------------
          IF(TTHESK.GT.THESP(I,J)) THEN
            PSP  (I,J)=TPSPK
            THESP(I,J)=TTHESK
          ENDIF
 10   CONTINUE
 20     CONTINUE
C-----CHOOSE LAYER DIRECTLY BELOW PSP AS LCL AND------------------------
C-----ENSURE THAT THE LCL IS ABOVE GROUND.------------------------------
C-------(IN SOME RARE CASES FOR ITYPE=2, IT IS NOT)---------------------
      DO L=1,LM
!$omp  parallel do
!$omp& private(pkl)
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          PKL = AETA(L)*PDSL(I,J)+PT
          IF (PKL.LT.PSP(I,J)) LCL(I,J)=L+1
        ENDDO
        ENDDO
      ENDDO
!$omp  parallel do
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          IF (LCL(I,J).GT.LMH(I,J)) LCL(I,J)=LMH(I,J)
        ENDDO
        ENDDO
C-----------------------------------------------------------------------
C---------FIND TEMP OF PARCEL LIFTED ALONG MOIST ADIABAT (TPAR)---------
C-----------------------------------------------------------------------
!!$omp  parallel do
      DO 30 L=LM,1,-1
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
      KNUML=0
      KNUMH=0
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        KLRES(I,J)=0
        KHRES(I,J)=0
        PKL=AETA(L)*PDSL(I,J)+PT
        IF(L.LE.LCL(I,J)) THEN
          IF(PKL.LT.PLQ)THEN
            KNUML=KNUML+1
            KLRES(I,J)=1
          ELSE
            KNUMH=KNUMH+1
            KHRES(I,J)=1
          ENDIF
        ENDIF
      ENDDO
      ENDDO
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PLQ
C**
      IF(KNUML.GT.0)THEN
        CALL TTBLEX(TPAR(1,1,L),TTBL,ITB,JTB,KLRES,PDSL,AETA(L)
     1,             HTM(1,1,L),PT,PL,QQ,PP,RDP,THE0,STHE,RDTHE,THESP
     2,             IPTB,ITHTB)
      ENDIF
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PLQ
C**
      IF(KNUMH.GT.0)THEN
        CALL TTBLEX(TPAR(1,1,L),TTBLQ,ITBQ,JTBQ,KHRES,PDSL,AETA(L)
     1,             HTM(1,1,L),PT,PLQ,QQ,PP,RDPQ,THE0Q,STHEQ,RDTHEQ
     2,             THESP,IPTB,ITHTB)
      ENDIF

C------------SEARCH FOR EQ LEVEL----------------------------------------
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        IF(KHRES(I,J).GT.0)THEN
          IF(TPAR(I,J,L).GT.T(I,J,L)) IEQL(I,J)=L
        ENDIF
      ENDDO
      ENDDO
C
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        IF(KLRES(I,J).GT.0)THEN
          IF(TPAR(I,J,L).GT.T(I,J,L)) IEQL(I,J)=L
        ENDIF
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
 30   CONTINUE
C------------COMPUTE CAPE AND CINS--------------------------------------
c      DO K=KHL01,KHH01
c        LCLK=LCL(K)
c        IEQK=IEQL(K)
cCDIR$ SHORTLOOP
c        DO L=IEQK,LCLK
c          PRESK=AETA(L)*PDSL(K)+PT
c          DZKL=T(K,L)*(Q(K,L)*D608+H1)*ROG*PDSL(K)*DETA(L)/PRESK
c          THETAP=TPAR(K,L)*(H10E5/PRESK)**CAPA
c          THETAA=T(K,L)*(H10E5/PRESK)**CAPA
c          IF (THETAP.LT.THETAA)
c     &      CINS(K)=CINS(K)+G*(ALOG(THETAP)-ALOG(THETAA))*DZKL
c          IF (THETAP.GT.THETAA)
c     &      CAPE(K)=CAPE(K)+G*(ALOG(THETAP)-ALOG(THETAA))*DZKL
c        ENDDO
c      ENDDO
      LBEG=100
      LEND=0
C
!$omp  parallel do
!$omp& private(lbeg,lend)
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        LBEG=MIN(IEQL(I,J),LBEG)
        LEND=MAX(LCL(I,J),LEND)
      ENDDO
      ENDDO
C
      DO L=LBEG,LEND
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          IDX(I,J)=0
          IF(L.GE.IEQL(I,J).AND.L.LE.LCL(I,J))THEN
            IDX(I,J)=1
          ENDIF
        ENDDO
C
        ENDDO
C
!$omp  parallel do
!$omp& private(dzkl,presk,thetaa,thetap)
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          IF(IDX(I,J).GT.0)THEN
            PRESK=AETA(L)*PDSL(I,J)+PT
            DZKL=T(I,J,L)*(Q(I,J,L)*D608+H1)
     *           *ROG*PDSL(I,J)*DETA(L)/PRESK
            THETAP=TPAR(I,J,L)*(H10E5/PRESK)**CAPA
            THETAA=T(I,J,L)*(H10E5/PRESK)**CAPA
            IF(THETAP.LT.THETAA)THEN
              CINS(I,J)=CINS(I,J)
cjjt *                 +G*(ALOG(THETAP/THETAA))*DZKL
     *                 +G*(ALOG(THETAP)-ALOG(THETAA))*DZKL
            ELSEIF(THETAP.GT.THETAA)THEN
              CAPE(I,J)=CAPE(I,J)
cjjt *                 +G*(ALOG(THETAP/THETAA))*DZKL
     *                 +G*(ALOG(THETAP)-ALOG(THETAA))*DZKL
            ENDIF
          ENDIF
        ENDDO
        ENDDO
      ENDDO
         
C    
C     ENFORCE LOWER LIMIT OF 0.0 ON CAPE AND UPPER
C     LIMIT OF 0.0 ON CINS.
C
!$omp  parallel do
      DO 40 J=JSTA,JEND
      DO 40 I=1,IM
c         IF (CAPE(K).LT.D00) 
c     X        WRITE(STDOUT,*)
c     X        'CALCAPE:  NEGATIVE CAPE AT K=',K,CAPE(K)
c         IF (CINS(K).GT.D00)
c     X        WRITE(STDOUT,*)
c     X        'CALCAPE:  POSTIVE CINS AT K = ',K,CINS(K)
         CAPE(I,J) = AMAX1(D00,CAPE(I,J))
         CINS(I,J) = AMIN1(CINS(I,J),D00)
 40   CONTINUE
C     
C     END OF ROUTINE.
C     
      RETURN
      END
