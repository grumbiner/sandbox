      SUBROUTINE PTETAE(PT,KBIMX,LMIMX,NBCEX
     1,                 IMI,JMI,LMI0
     2,                 IUDETO,IUDETI
     3,                 PDBO,TBO,QBO,UBO,VBO,Q2BO,CWMBO
     4,                 HTMBO,USTARBO
     5,                 PDBI,TBI,QBI,UBI,VBI,Q2BI,CWMBI)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  PTETAE      ETA-TO-ETA INTERPOLATION PROGRAM
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-05-25
C
C  ABSTRACT:  VERTICALLY INTERPOLATES ETA DATA AT ONE VERTICAL
C             RESOLUTION TO ETA LAYERS AT ANOTHER RESOLUTION.
C             PROGRAM ASSUMES HORIZONTAL INTERPOLATION HAS BEEN
C             PERFORMED EARLIER AND THAT THE TOP PRESSURE OF THE
C             INITIAL AND FINAL VERTICAL GRIDS IS THE SAME
C
C PROGRAM HISTORY LOG:
C    95-07-14  T BLACK - WROTE CODE FOR ETA RESTRT-->ETA RESTRT ABILITY
C    96-10-29  T BLACK - MODIFIED FOR GENERAL NESTING CAPABILITY
C    99-05-25  T BLACK - MODIFIED FOR BACKGROUND PRE-POST JOB
C
C USAGE:  CALL PTETAE FROM SUBROUTINE BCEX
C
C   INPUT ARGUMENT LIST:
C         PT - THE TOP PRESSURE OF THE DOMAIN
C      KBIMX - THE MAXIMUM LENGTH OF ANY NEST BOUNDARY'S HORIZONTAL
C              EXTENT
C      LMIMX - THE MAXIMUM NUMBER OF MODEL LEVELS FOR ANY NEST
C      NBCEX - THE NUMBER OF NESTS
C        IMI - THE ARRAY OF IM's FOR ALL NESTS
C        JMI - THE ARRAY OF JM's FOR ALL NESTS
C       LMI0 - THE ARRAY OF LM's FOR ALL NESTS
C     IUDETO - THE UNIT NUMBER OF THE OPERATIONAL DETA's
C     IUDETI - THE BASE UNIT NUMBER OF THE NEST DETA's
C       PDBO - THE PD ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C        TBO - THE TEMPERATURE ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C        QBO - THE SPEC HUMIDITY ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C        UBO - THE U COMPONENT ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C        VBO - THE V COMPONENT ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C       Q2BO - THE TKE ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C      CWMBO - THE CLOUD WATER ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C      HTMBO - THE HEIGHT MASK ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C    USTARBO - THE USTAR ARRAY FOR ALL NEST BOUNDARIES AFTER
C              HORIZONTAL INTERPOLATION FROM THE OPERATIONAL GRID
C
C   OUTPUT ARGUMENT LIST:
C       PDBI - THE FINAL PD ARRAY FOR ALL NEST BOUNDARIES
C        TBI - THE FINAL TEMP ARRAY FOR ALL NEST BOUNDARIES
C        QBI - THE FINAL SPEC HUMIDITY ARRAY FOR ALL NEST BOUNDARIES
C        UBI - THE FINAL U COMPONENT ARRAY FOR ALL NEST BOUNDARIES 
C        VBI - THE FINAL V COMPONENT ARRAY FOR ALL NEST BOUNDARIES
C       Q2BI - THE FINAL TKE ARRAY FOR ALL NEST BOUNDARIES 
C      CWMBI - THE FINAL CLOUD WATER ARRAY FOR ALL NEST BOUNDARIES
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  NONE
C
C----------------------------------------------------------------------
      INCLUDE "parmeta"
C----------------------------------------------------------------------
                             P A R A M E T E R
     1 (LMOP=LM+1,LMOM=LM-1)
C
                             P A R A M E T E R
     1 (CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622
     2, RD=287.04,G=9.80,RG=1./G,EPSQ=2.E-12,EPSQ2=0.2
     3, B1=11.87799326)
C--------------------------------------------------------------------
                             P A R A M E T E R
     & (K15=SELECTED_REAL_KIND(15))
C
                             R E A L
     & (KIND=K15) B,C,ALPET,ALPETK
     &,           ALP(KBIMX,LM+1),ALPETA(KBIMX),ALPSQ(KBIMX,LM+1)
C--------------------------------------------------------------------
                               R E A L
     1 PDBI(KBIMX,NBCEX)
     2,TBI(KBIMX,LMIMX,NBCEX),QBI(KBIMX,LMIMX,NBCEX)
     3,UBI(KBIMX,LMIMX,NBCEX),VBI(KBIMX,LMIMX,NBCEX)
     4,Q2BI(KBIMX,LMIMX,NBCEX),CWMBI(KBIMX,LMIMX,NBCEX)
C
                               R E A L
     1 PDBO(KBIMX,NBCEX)
     2,TBO(KBIMX,LM,NBCEX),QBO(KBIMX,LM,NBCEX)
     3,UBO(KBIMX,LM,NBCEX),VBO(KBIMX,LM,NBCEX)
     4,Q2BO(KBIMX,LM,NBCEX),CWMBO(KBIMX,LM,NBCEX)
     5,HTMBO(KBIMX,LM,NBCEX),USTARBO(KBIMX,NBCEX)
C----------------------------------------------------------------------
                               R E A L
     1  ETAO(LM+1),ETAI(LMIMX+1)
     2, DETAO(LM),AETAO(LM),DETAI(LMIMX),AETAI(LMIMX),PINTO(KBIMX,LM+1)
     3, ALPR(KBIMX,LM)
     4, PDSLO(KBIMX),ZETAO(KBIMX,LM+1),ZETAI(KBIMX,LMIMX)
     5, DLT(KBIMX,LMIMX),PMIDO(KBIMX,LM)
     6, PINTI(LMIMX),ZSFCI(KBIMX),REFI(KBIMX),DFLO(LM+1)
     7, RESO(KBIMX),PSFCO(KBIMX),TX(KBIMX)
     8, PDIJ(KBIMX),PETA(KBIMX)
     9, PDVP(KBIMX),PMVP(KBIMX,2),PMIDI(KBIMX)
     O, UTOP(KBIMX),VTOP(KBIMX),Q2TOP(KBIMX),ALPTU(KBIMX),PHUB(KBIMX)
     1, ALPSFC(KBIMX),Q2SFC(KBIMX)
C-----------------------------------------------------------------------
                               I N T E G E R
     1  LDT1(KBIMX,LMIMX),LMHO(KBIMX)
     2, IDX(KBIMX),IDXX(KBIMX),KNTN(KBIMX),K1(KBIMX),K2(KBIMX),LL(KBIMX)
     3, KSTRTX(KBIMX)
C
                               I N T E G E R
     1 IMI(9),JMI(9),LMI0(9)
C-----------------------------------------------------------------------
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C-----------------------------------------------------------------------
C***
C***  DEFINITION OF CONSTANTS NEEDED FOR VERTICAL INTERPOLATION
C***
      CP=1004.6
      P00=1.E5
      GAMMA=0.0065
      PRF0=101325.
      T0=288.
      GORG=G/(RD*GAMMA)
      RGOG=1./GORG
      ROG=RD/G
      CPOG=CP/G
      CAPA=RD/CP
      ALPT=ALOG(PT)
C-------------------------------------------------------------
      REWIND IUDETO
      READ(IUDETO)DETAO
      IUDETI0=IUDETI
C-------------------------------------------------------------
C-------------------------------------------------------------
C***
C***  THE GRAND LOOP OVER EACH OF THE NESTS
C***
C-------------------------------------------------------------
C-------------------------------------------------------------
      DO 1000 NB=1,NBCEX
C-------------------------------------------------------------
      KBI=2*IMI(NB)+JMI(NB)-3
      LMI=LMI0(NB)
C-------------------------------------------------------------
      IUDETI0=IUDETI0+1
      REWIND IUDETI0
      READ(IUDETI0)(DETAI(L),L=1,LMI)
C
      DO K=1,KBI
        ZSFCI(K)=0.
        REFI(K)=1.
      ENDDO
C***
C***  SINCE WE ARE ON THE NEST'S BOUNDARY, LMH ALWAYS EQUALS LMI
C***
      LMHI=LMI
C
C----------------------------------------------------------------------
C***  COMPUTE ETA AT THE INTERFACES OF THE MODEL LAYERS
C----------------------------------------------------------------------
C***
C***  FIRST ON THE OUTER GRID
C***
      ETAO(1)=0.
C
      DO L=2,LM
        ETAO(L)=ETAO(L-1)+DETAO(L-1)
      ENDDO
C
      ETAO(LM+1)=1.
      DETAO(LM)=1.-ETAO(LM)
C
C***
C***  THEN ON THE INNER GRID
C***
C
      ETAI(1)=0.
C
      DO L=2,LMI
        ETAI(L)=ETAI(L-1)+DETAI(L-1)
      ENDDO
C
      ETAI(LMI+1)=1.
      DETAI(LMI)=1.-ETAI(LMI)
C***
C***  COMPUTE AETA IN THE MIDDLES OF THE LAYERS
C***
      DO L=1,LMI
         AETAI(L)=0.5*(ETAI(L)+ETAI(L+1))
      ENDDO
      DO L=1,LM
         AETAO(L)=0.5*(ETAO(L)+ETAO(L+1))
      ENDDO
C----------------------------------------------------------------
C***
C***  COMPUTE THE HEIGHTS OF THE STANDARD INTERFACES
C***  FOR THE OUTER GRID
C***
      DO L=1,LM+1
        DFLO(L)=G*T0*(1.-((PT+ETAO(L)*(PRF0-PT))/PRF0)**RGOG)/GAMMA
      ENDDO
C***
C***  GENERATE THE LMH AND THE SURFACE ETA ARRAYS
C***
      DO K=1,KBI
        LMHO(K)=0
      ENDDO
C
      DO L=2,LM
        DO K=1,KBI
          IF(HTMBO(K,L,NB).LT.0.1.AND.LMHO(K).EQ.0)THEN
            LMHO(K)=L-1
            RESO(K)=1./ETAO(L)
          ENDIF
        ENDDO
      ENDDO
C
      DO K=1,KBI
        IF(LMHO(K).EQ.0)THEN
          LMHO(K)=LM
          RESO(K)=1.
        ENDIF
      ENDDO
C----------------------------------------------------------------
C***
C***  COMPUTE HEIGHTS OF THE OUTER GRID ETA INTERFACES
C***
      DO K=1,KBI
        PDSLO(K)=RESO(K)*PDBO(K,NB)
        ZETAO(K,LM+1)=0.0
C       ZETAO(K,LM+1)=FISO(K)*RG
      ENDDO
C       
      DO L=LM,1,-1
        DO K=1,KBI
          DPDE=DETAO(L)*PDSLO(K)
          APEL=PT+AETAO(L)*PDSLO(K)
          RTOP=RD*TBO(K,L,NB)*(1.+0.608*QBO(K,L,NB))/APEL
          FI=G*ZETAO(K,L+1)+RTOP*DPDE
          ZETAO(K,L)=((FI-DFLO(L))*HTMBO(K,L,NB)+DFLO(L))*RG
        ENDDO
      ENDDO
C***
C***  FIND THE PRESSURE ON THE OUTER GRID INTERFACES
C***
      DO K=1,KBI
        PINTO(K,1)=PT
      ENDDO
C
      DO L=2,LM+1
        DO K=1,KBI
          PINTO(K,L)=PDSLO(K)*ETAO(L)+PT
        ENDDO
      ENDDO
C***
C***  FIND THE PRESSURE AT THE OUTER GRID MIDLAYER LOCATIONS
C***
      DO L=1,LM
        DO K=1,KBI
          PMIDO(K,L)=PINTO(K,L)+0.5*DETAO(L)*PDSLO(K)
        ENDDO
      ENDDO
C***
C***  COMPUTE THE LOGS OF PRESSURE FOR UPCOMING INTERPOLATIONS
C***
      DO LD=1,LM
        DO K=1,KBI
          ALPR(K,LD)=ALOG(PINTO(K,LD+1)/PINTO(K,LD))
        ENDDO
      ENDDO
C
      DO LD=1,LM+1
        DO K=1,KBI
          ALP(K,LD)=ALOG(PINTO(K,LD))
          ALPSQ(K,LD)=ALP(K,LD)**2
        ENDDO
      ENDDO
C-------------------------------------------------------------
C***
C***  VERTICAL INTERPOLATION: QUADRATIC INTERPOLATION OF
C***  HEIGHTS (EQUIVALENT TO TEMPERATURE BEING LINEAR IN LN P)
C***  AND LINEAR INTERPOLATION OF WINDS
C***
C-------------------------------------------------------------
C***
C***  ASSIGN TO PD ON THE NEST BOUNDARY THE VALUE OF PDSL AT THE
C***  NEAREST H POINT IN THE PARENT ETA RUN
C***
C----------------------------------------------------------------------
      DO K=1,KBI
        PDIJ(K)=PDSLO(K)
C
C**********************************************************************
C***  THE FOLLOWING LINE IS ONLY TRUE IF PT IS THE SAME
C***  FOR BOTH THE INNER AND OUTER DOMAINS
C**********************************************************************
C
        ZETAI(K,1)=ZETAO(K,1)
C
      ENDDO
C----------------------------------------------------------------------
C***
C***  COMPUTATION OF HEIGHTS AT UPPER BOUNDARIES OF ETA LAYERS 
C***  AND TEMPERATURE AND SPEC HUMIDITY AT MIDDLE OF ETA LAYERS
C***
C***  LDT1 HOLDS THE INDEX OF THE OUTER GRID ETA INTERFACE THAT IS
C***  DIRECTLY ABOVE THE INNER GRID ETA INTERFACE IN QUESTION.
C***
      DO K=1,KBI
        PETA(K)=PT
        LDT1(K,1)=1
      ENDDO
C
C----------------------------------------------------------------------
      MNKNT=1
      DO 100 L=2,LMI
      DETA1=DETAI(L-1)
C
      NUMK=KBI
      KNTX=MNKNT
      KSTRT=KNTX+1
C
      DO K=1,KBI
        PETA(K)=PETA(K)+DETA1*PDIJ(K)
        IDXX(K)=K
        KSTRTX(K)=1
      ENDDO
C
   50 NK=NUMK
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
      ENDDO
      KNTIN=KNTX
      NUMK=0
C
cdir$ ivdep
      DO N=1,NK
        K=IDX(N)
        PETAK=PETA(K)
        PINTOA=PINTO(K,KNTIN)
        PINTOB=PINTO(K,KNTIN+1)
        IF(PETAK.GT.PINTOA.AND.PETAK.LE.PINTOB.OR.
     1     PETAK.LT.PINTO(K,2))THEN
          LDT1(K,L)=MIN(KNTIN,LM-1)
          IF(PETAK+DETAI(L)*PDIJ(K).LE.PINTOB)THEN
            KSTRTX(K)=MIN(KSTRT,KNTIN)
          ELSE
            KSTRTX(K)=MIN(KSTRT,KNTIN+1)
          ENDIF
        ELSE
          NUMK=NUMK+1
          IDXX(NUMK)=K
          KNTX=KNTIN+1
        ENDIF
      ENDDO
C
      IF(NUMK.GT.0)GO TO 50
      IF(NUMK.EQ.0)THEN
        MNKNT=MNKNT+1
        DO K=1,KBI
          MNKNT=MIN(MNKNT,KSTRTX(K))
        ENDDO
      ENDIF
  100 CONTINUE
C
      DO 110 L=1,LMI
      DO K=1,KBI
        L1=LDT1(K,L)
        DLT(K,L)=ALPR(K,L1+1)*(-ALPR(K,L1))*ALOG(PINTO(K,L1+2)
     1                                           /PINTO(K,L1))
      ENDDO    
  110 CONTINUE
C----------------------------------------------------------------------
C***
C***  NOW COMPUTE THE HEIGHTS OF THE INNER GRID INTERFACES
C***
C----------------------------------------------------------------------
                               DO 200 L=2,LMI
C----------------------------------------------------------------------
      DO 190 K=1,KBI
C
      L1=LDT1(K,L)
      L2=L1+1
      L3=L1+2
      H1=ZETAO(K,L1)
      H2=ZETAO(K,L2)
      H3=ZETAO(K,L3)
      H3M2=H3-H2
      H3M1=H3-H1
C***
C***  THE COEFFICIENTS FOR QUADRATIC INTERPOLATION OF HEIGHTS.
C***
      B=(H3M2*(ALPSQ(K,L3)-ALPSQ(K,L1))-H3M1*(ALPSQ(K,L3)-ALPSQ(K,L2)))
     1        /DLT(K,L)
      C=(H3M1*(ALP(K,L3)-ALP(K,L2))-H3M2*(ALP(K,L3)-ALP(K,L1)))
     1        /DLT(K,L)
C
      ARGLOG=PT+PDIJ(K)*ETAI(L)
      ALPET=ALOG(ARGLOG)
C***
C***  THE HEIGHTS OF THE INNER GRID ETA INTERFACES.
C***
      ZETAI(K,L)=H2+B*(ALPET-ALP(K,L2))+C*(ALPET**2-ALPSQ(K,L2))
C
  190 CONTINUE
C
  200 CONTINUE
C
      DO K=1,KBI
        PDBI(K,NB)=PDIJ(K)
      ENDDO
C--------------------------------------------------------------
C***
C***  COMPUTATION OF WINDS WITHIN ETA LAYERS
C***
C--------------------------------------------------------------
      IMT=2*IMI(NB)-1
      JMT=JMI(NB)/2+1
C***
C***  ARRANGEMENT OF LOCATIONS IN THE V BOUNDARY FILE
C***
C                       2ND
C           IBBP--------->----------ITB
C           ILB                     KBI
C            |                       |
C       3RD /|\                     /|\ 4TH
C            |                       |
C           IMT                     ILBP
C            1----------->----------IBB
C                       1ST
      IBB=IMT/2
      IBBP=IBB+1
      ITB=IMT-1
      ILB=IMT+JMT-2
      ILBP=ILB+1
      ILBP2=ILB+2
      ILBM=ILB-1
      IMTP=IMT+1
      KBM=KBI-1
C***
C***  FIND AVERAGE PD OVER VELOCITY BOUNDARY POINTS
C***
      DO K=1,IBB
        K1(K)=K
        K2(K)=K+1
      ENDDO
      DO K=IBBP,ITB
        K1(K)=K+1
        K2(K)=K+2
      ENDDO
      DO K=IMTP,ILBM
        K1(K)=K+1
        K2(K)=K+2
      ENDDO
      DO K=ILBP2,KBM
        K1(K)=K
        K2(K)=K+1
      ENDDO
      K1(IMT)=1
      K2(IMT)=IMT+2
      K1(ILB)=ILB+1
      K2(ILB)=IBB+2
      K1(ILBP)=IBB+1
      K2(ILBP)=ILBP2
      K1(KBI)=KBI
      K2(KBI)=ITB+2
C***
C***  AVERAGE THE PRESSURE AT VELOCITY POINTS.
C***
      DO 210 K=1,KBI
      K1K=K1(K)
      K2K=K2(K)
      PDVP(K)=0.5*(PDBI(K1K,NB)+PDBI(K2K,NB))
      PMVP(K,1)=0.5*(PMIDO(K1K,1)+PMIDO(K2K,1))
      ALP(K,1)=ALOG(PMVP(K,1))
  210 CONTINUE
C
      DO 225 L=2,LM
      DO K=1,KBI
        K1K=K1(K)
        K2K=K2(K)
        PMVP(K,2)=0.5*(PMIDO(K1K,L)+PMIDO(K2K,L))
        ALPR(K,L-1)=ALOG(PMVP(K,2)/PMVP(K,1))
        ALP(K,L)=ALOG(PMVP(K,2))
        PMVP(K,1)=PMVP(K,2)
      ENDDO   
  225 CONTINUE
C-------------------------------------------------------------------
C***
C***  INTERPOLATE THE WINDS
C***
C-------------------------------------------------------------------
C***  FOR EACH MODEL LAYER IN THE NEST DOMAIN, SEARCH THROUGH
C***  LEVELS IN THE PARENT DOMAIN.  WHEN TWO PARENT LEVELS
C***  SANDWICH POINTS ON A NEST LEVEL, DO THE VERTICAL
C***  INTERPOLATION TO THE NEST LEVEL THEN REMOVE THOSE
C***  NEST LEVEL POINTS FROM CONSIDERATION AND CONTINUE SEARCHING
C***  PARENT LEVELS FOR THE REMAINING POINTS ON THE GIVEN
C***  NEST LEVEL.
C***
      MNKNT=1
      DO 270 L=1,LMI
C
C***  ALPETA IS THE LN OF PRESSURE ON THE 
C***  INNER GRID MID-LEVEL
C
      DO K=1,KBI
        ALPETA(K)=ALOG(PT+PDVP(K)*AETAI(L))
        IDXX(K)=K
      ENDDO
C
      NUMK=KBI
      KNTX=MNKNT
C
  230 NK=NUMK
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
      ENDDO
      KNTIN=KNTX
      KP1=MIN(LM,KNTIN+1)
      NUMK=0
C
cdir$ ivdep
      DO N=1,NK
        K=IDX(N)
        ALPETK=ALPETA(K)
        IF(ALPETK.GT.ALP(K,KNTIN).AND.ALPETK.LE.ALP(K,KP1)
     1    .AND.ABS(UBO(K,KP1,NB)).GT.0..OR.ALPETK.LT.ALP(K,1))THEN
          ULD=UBO(K,KP1,NB)
          VLD=VBO(K,KP1,NB)
          CF=(ALP(K,KP1)-ALPETK)/ALPR(K,KNTIN)
          UKB=ULD+(UBO(K,KNTIN,NB)-ULD)*CF
          VKB=VLD+(VBO(K,KNTIN,NB)-VLD)*CF
          UTOP(K)=UKB
          VTOP(K)=VKB
          ALPTU(K)=ALPETK
          LL(K)=L
          UBI(K,L,NB)=UKB
          VBI(K,L,NB)=VKB
C***
C***  IF THE NEW ETA GROUND SURFACE IS BELOW THE OLD GROUND, THEN
C***  ASSUME THE WIND COMPONENTS DECREASE TO ZERO AT THE SURFACE.
C***  THE SAME IS TRUE FOR ANY NEW ETA LEVEL BELOW THE LOWEST
C***  OLD ETA SURFACE.
C***
        ELSEIF(ALPETK.GT.ALP(K,LM).OR.UBO(K,KP1,NB).EQ.0.)THEN
          UDIF=UTOP(K)
          VDIF=VTOP(K)
          ALPDIF=ALPTU(K)-ALOG(PT+PDVP(K))
          LL(K)=LL(K)+1
          LLK=LL(K)
          UBI(K,LLK,NB)=UTOP(K)-UDIF*(ALPTU(K)-ALPETK)/ALPDIF
          VBI(K,LLK,NB)=VTOP(K)-VDIF*(ALPTU(K)-ALPETK)/ALPDIF
        ELSEIF(ALPETK.GT.ALP(K,KP1))THEN
          NUMK=NUMK+1
          IDXX(NUMK)=K
          KNTX=KNTIN+1
        ENDIF
      ENDDO
      IF(NUMK.GT.0)THEN
        IF(NUMK.EQ.KBI)MNKNT=MNKNT+1
        GO TO 230
      ENDIF
c     IF(NUMK.EQ.0)MNKNT=KNTX
C
  270 CONTINUE
C
C--------------------------------------------------------------------
C***
C***  INTERPOLATE Q AND CLD WATER LINEARLY IN LN(P)
C***
cfpp$ noconcur l
      DO 300 L=LMI,1,-1
      DO K=1,KBI
        IF(L.EQ.LMI)PHUB(K)=0.
        PHLB=PHUB(K)
        PHUB(K)=G*ZETAI(K,L)
        TBI(K,L,NB)=-(PHLB-PHUB(K))*(PT+AETAI(L)*PDBI(K,NB))
     1             /(RD*DETAI(L)*PDBI(K,NB))
        IF(TBI(K,L,NB).LT.150.)TBI(K,L,NB)=150.
      ENDDO
  300 CONTINUE
C
      DO K=1,KBI
        ALP(K,1)=ALOG(PMIDO(K,1))
        PSFCO(K)=PDBO(K,NB)+PT
      ENDDO
C
      DO LD=2,LM
      DO K=1,KBI
        ALPR(K,LD-1)=ALOG(PMIDO(K,LD)/PMIDO(K,LD-1))
        ALP(K,LD)=ALOG(PMIDO(K,LD))
      ENDDO
      ENDDO
C
      MNKNT=1
      DO 340 L=1,LMHI
C
      DO K=1,KBI
        PMIDI(K)=AETAI(L)*PDBI(K,NB)+PT
        ALPETA(K)=ALOG(PMIDI(K))
        IDXX(K)=K
      ENDDO
C
      NUMK=KBI
      KNTX=MNKNT
C
  310 NK=NUMK
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
      ENDDO
      KNTIN=KNTX
      KP1=KNTIN+1
      NUM0=0
      NUMK=0
C
cdir$ ivdep
      DO N=1,NK
        K=IDX(N)
        LKO=LMHO(K)
        ALPETK=ALPETA(K)
        IF(ALPETK.GT.ALP(K,KNTIN).AND.ALPETK.LE.ALP(K,KP1)
     1    .AND.KP1.LE.LKO.OR.ALPETK.LT.ALP(K,1)
     2    .OR.ALPETK.GT.ALP(K,KP1).AND.PMIDI(K).LE.PSFCO(K)
     3    .AND.KP1.EQ.LKO)THEN
          QLD=QBO(K,KP1,NB)
          CWMLD=CWMBO(K,KP1,NB)
          CF=(ALP(K,KP1)-ALPETA(K))/ALPR(K,KNTIN)
          CWMBI(K,L,NB)=CWMLD+(CWMBO(K,KNTIN,NB)-CWMLD)*CF
          QETAIJ=QLD+(QBO(K,KNTIN,NB)-QLD)*CF
          CLOGES=-CM1/TBI(K,L,NB)-CM2*ALOG10(TBI(K,L,NB))+CM3
          ESE=10.**(CLOGES+2.)
          QSX=EPS*ESE/(PMIDI(K)-ESE*(1.-EPS))
          QSMX=0.98*QSX
          QBI(K,L,NB)=AMIN1(QETAIJ,QSMX,20.E-3)
C***
C***  IF THE NEW ETA GROUND SURFACE IS BELOW THE OLD GROUND THEN
C***  ASSUME THE RH (GIVEN T) IS EQUAL TO THAT ON THE LOWEST
C***  OLD SURFACE AND THAT THE CLOUD WATER IS ZERO.
C***  NOTE:  TBI INSIDE THE NEXT BLOCK IS ACTUALLY THE VIRTUAL
C***         TEMPERATURE YET IT IS BEING USED AS THE TRUE T
C***
        ELSEIF(PMIDI(K).GT.PSFCO(K))THEN
          CLOGES=-CM1/TBO(K,LKO,NB)-CM2*ALOG10(TBO(K,LKO,NB))+CM3
          ESS=10.**(CLOGES+2.)
          QSLDM=EPS*ESS/(PMIDO(K,LKO)-ESS*(1.-EPS))
          RHLDM=QBO(K,LKO,NB)/QSLDM
          RHLDM=AMIN1(RHLDM,0.98)
          CLOGES=-CM1/TBI(K,L,NB)-CM2*ALOG10(TBI(K,L,NB))+CM3
          ESL=10.**(CLOGES+2.)
          QSL=EPS*ESL/(AETAI(L)*PDBI(K,NB)+PT-ESL*(1.-EPS))
          QBI(K,L,NB)=RHLDM*QSL
          QBI(K,L,NB)=AMIN1(QBI(K,L,NB),20.E-3)
          CWMBI(K,L,NB)=0.
        ELSEIF(ALPETK.GT.ALP(K,KP1))THEN
          NUMK=NUMK+1
          IDXX(NUMK)=K
          KNTX=KNTIN+1
        ENDIF
      ENDDO
      IF(NUMK.GT.0)THEN
        IF(NUMK.EQ.KBI)MNKNT=MNKNT+1
        GO TO 310
      ENDIF
c     IF(NUMK.EQ.0)MNKNT=KNTX
  340 CONTINUE
C
      DO L=1,LMI
      DO K=1,KBI
        QBI(K,L,NB)=AMAX1(QBI(K,L,NB),EPSQ)
        CWMBI(K,L,NB)=AMAX1(CWMBI(K,L,NB),0.)
      ENDDO   
      ENDDO   
C
C***
C***  CONVERT FROM VIRTUAL TO TRUE TEMPERATURE.
C***
      DO 375 L=1,LMI
C
      DO K=1,KBI
        TBI(K,L,NB)=AMIN1(TBI(K,L,NB),350.)
        TBI(K,L,NB)=AMAX1(TBI(K,L,NB),150.)
        TX(K)=TBI(K,L,NB)
        IDXX(K)=K
      ENDDO
      NUMK=KBI
C
      ITER=0
  360 ITER=ITER+1
      NK=NUMK
      NUMK=0
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
      ENDDO
C
cdir$ ivdep
      DO N=1,NK
        K=IDX(N)
        TBI(K,L,NB)=TX(K)/(1.+0.608*QBI(K,L,NB))
        CLOGES=-CM1/TBI(K,L,NB)-CM2*ALOG10(TBI(K,L,NB))+CM3
        ESE=10.**(CLOGES+2.)
        DENOM2=AETAI(L)*PDBI(K,NB)+PT-ESE*(1.-EPS)
        QSX=EPS*ESE/DENOM2
        QSMX=0.98*QSX
        IF(QBI(K,L,NB).GT.QSMX)THEN
          QBI(K,L,NB)=QSMX
          NUMK=NUMK+1
          IDXX(NUMK)=K
        ENDIF
      ENDDO
      IF(NUMK.GT.0.AND.ITER.LE.2)GO TO 360
C
  375 CONTINUE
C--------------------------------------------------------------------
C***
C***  INTERPOLATE Q2 LINEARLY IN LN(P) (BUT ON INTERFACES)
C***
      DO K=1,KBI
        Q2SFC(K)=AMAX1(B1**(2./3.)*USTARBO(K,NB)**2,EPSQ2)
        ALPSFC(K)=ALOG(PDBI(K,NB)*REFI(K)+PT)
        ALP(K,LMOP)=ALOG(PINTO(K,LMOP))
      ENDDO
C
      DO 400 LD=2,LM
      DO K=1,KBI
        ALPR(K,LD)=ALOG(PINTO(K,LD+1)/PINTO(K,LD))
        ALP(K,LD)=ALOG(PINTO(K,LD))
      ENDDO
  400 CONTINUE
C
      MNKNT=2
      LKI=LMHI-1
      DO 450 L=1,LKI
C
      DO K=1,KBI
        ALPETA(K)=ALOG(ETAI(L+1)*PDBI(K,NB)+PT)
        IDXX(K)=K
      ENDDO
C
      NUMK=KBI
      KNTX=MNKNT
C
  410 NK=NUMK
      DO NN=1,NK
        IDX(NN)=IDXX(NN)
      ENDDO
C
      KNTIN=KNTX
      KP1=KNTIN+1
      NUMK=0
C
cdir$ ivdep
      DO N=1,NK
        K=IDX(N)
        LKO=LMHO(K)
        ALPETK=ALPETA(K)
        IF(ALPETK.GT.ALP(K,KNTIN).AND.ALPETK.LE.ALP(K,KP1)
     1     .AND.KP1.LE.LKO.OR.ALPETK.LT.ALP(K,2))THEN
          Q2LD=Q2BO(K,KP1,NB)
          CF=(ALP(K,KP1)-ALPETK)/ALPR(K,KNTIN)
          Q2IJ=Q2LD+(Q2BO(K,KNTIN,NB)-Q2LD)*CF
          Q2TOP(K)=AMAX1(Q2IJ,EPSQ2)
          ALPTU(K)=ALPETK
          LL(K)=L
          Q2BI(K,L,NB)=Q2TOP(K)
C***
C***  IF THE NEW Q2 INTERFACE IS BELOW THE TOP OF THE LOWEST 
C***  OUTER GRID LAYER, ASSUME Q2 DECREASES LINEARLY TO Q2 AT
C***  THE NEW GROUND.
C***
        ELSEIF(ALPETK.GT.ALP(K,LKO))THEN
          Q2DIF=Q2TOP(K)-Q2SFC(K)
          ALPDIF=ALPTU(K)-ALPSFC(K)
          LL(K)=LL(K)+1
          LLK=LL(K)
          Q2BI(K,LLK,NB)=Q2TOP(K)-Q2DIF*(ALPTU(K)-ALPETK)/ALPDIF
        ELSEIF(ALPETK.GT.ALP(K,KP1))THEN
          NUMK=NUMK+1
          IDXX(NUMK)=K
          KNTX=KNTIN+1
        ENDIF
      ENDDO
      IF(NUMK.GT.0)THEN
        IF(NUMK.EQ.KBI)MNKNT=MNKNT+1
        GO TO 410
      ENDIF
c     IF(NUMK.EQ.0)MNKNT=KNTX
C
  450 CONTINUE
C-----------------------------------------------------------
C***
C***  NOW REDEFINE PD TO HAVE IT EQUAL TO PSFC-PT.
C***
      DO K=1,KBI
        PDBI(K,NB)=PDBI(K,NB)*REFI(K)
        Q2BI(K,LMHI,NB)=Q2SFC(K)
      ENDDO
C------------------------------------------------------------
 1000 CONTINUE
C------------------------------------------------------------
      RETURN
      END
