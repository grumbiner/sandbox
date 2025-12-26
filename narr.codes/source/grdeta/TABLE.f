C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE TABLE(PTBL,TTBL,PT
     &,                RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)
C     ******************************************************************
C     *                                                                *
C     *    GENERATE VALUES FOR LOOK-UP TABLES USED IN CONVECTION       *
C     *                                                                *
C     ******************************************************************
                             P A R A M E T E R
     & (ITB=076,JTB=134)
                             P A R A M E T E R
     & (THH=365.,PH=105000.
     &, PQ0=379.90516,A1=610.78,A2=17.2693882,A3=273.16,A4=35.86
     &, R=287.04,CP=1004.6,ELIWV=2.683E6,EPS=1.E-9)
                             D I M E N S I O N
     &  PTBL  (ITB,JTB),TTBL  (JTB,ITB),QSOLD (JTB),POLD  (JTB)
     &, QS0   (JTB),SQS   (JTB),QSNEW (JTB)
     &, Y2P   (JTB),APP   (JTB),AQP   (JTB),PNEW  (JTB)
     &, TOLD  (JTB),THEOLD(JTB),THE0  (ITB),STHE  (ITB)
     &, Y2T   (JTB),THENEW(JTB),APT   (JTB),AQT   (JTB),TNEW  (JTB)
C--------------COARSE LOOK-UP TABLE FOR SATURATION POINT----------------
      KTHM=JTB
      KPM=ITB
      KTHM1=KTHM-1
      KPM1=KPM-1
C
      PL=PT
C
      DTH=(THH-THL)/REAL(KTHM-1)
      DP =(PH -PL )/REAL(KPM -1)
C
      RDTH=1./DTH
      RDP=1./DP
      RDQ=KPM-1
C
      TH=THL-DTH
C-----------------------------------------------------------------------
              DO 500 KTH=1,KTHM
          TH=TH+DTH
          P=PL-DP
          DO 510 KP=1,KPM
      P=P+DP
      APE=(100000./P)**(R/CP)
      QSOLD(KP)=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
 510  POLD(KP)=P
C
      QS0K=QSOLD(1)
      SQSK=QSOLD(KPM)-QSOLD(1)
      QSOLD(1  )=0.
      QSOLD(KPM)=1.
C
          DO 520 KP=2,KPM1
      QSOLD(KP)=(QSOLD(KP)-QS0K)/SQSK
C
      IF((QSOLD(KP)-QSOLD(KP-1)).LT.EPS) QSOLD(KP)=QSOLD(KP-1)+EPS
C
 520  CONTINUE
C
      QS0(KTH)=QS0K
      SQS(KTH)=SQSK
C-----------------------------------------------------------------------
      QSNEW(1  )=0.
      QSNEW(KPM)=1.
      DQS=1./REAL(KPM-1)
C
          DO 530 KP=2,KPM1
 530  QSNEW(KP)=QSNEW(KP-1)+DQS
C
      Y2P(1   )=0.
      Y2P(KPM )=0.
C
      CALL SPLINE(JTB,KPM,QSOLD,POLD,Y2P,KPM,QSNEW,PNEW,APP,AQP)
C
          DO 540 KP=1,KPM
 540  PTBL(KP,KTH)=PNEW(KP)
C-----------------------------------------------------------------------
 500  CONTINUE
C--------------COARSE LOOK-UP TABLE FOR T(P) FROM CONSTANT THE----------
      P=PL-DP
              DO 550 KP=1,KPM
          P=P+DP
          TH=THL-DTH
          DO 560 KTH=1,KTHM
      TH=TH+DTH
      APE=(100000./P)**(R/CP)
      QS=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
      TOLD(KTH)=TH/APE
 560  THEOLD(KTH)=TH*EXP(ELIWV*QS/(CP*TOLD(KTH)))
C
      THE0K=THEOLD(1)
      STHEK=THEOLD(KTHM)-THEOLD(1)
      THEOLD(1   )=0.
      THEOLD(KTHM)=1.
C
          DO 570 KTH=2,KTHM1
      THEOLD(KTH)=(THEOLD(KTH)-THE0K)/STHEK
C
      IF((THEOLD(KTH)-THEOLD(KTH-1)).LT.EPS)
     1    THEOLD(KTH)=THEOLD(KTH-1)  +  EPS
C
 570  CONTINUE
C
      THE0(KP)=THE0K
      STHE(KP)=STHEK
C-----------------------------------------------------------------------
      THENEW(1  )=0.
      THENEW(KTHM)=1.
      DTHE=1./REAL(KTHM-1)
      RDTHE=1./DTHE
C
          DO 580 KTH=2,KTHM1
 580  THENEW(KTH)=THENEW(KTH-1)+DTHE
C
      Y2T(1   )=0.
      Y2T(KTHM)=0.
C
      CALL SPLINE(JTB,KTHM,THEOLD,TOLD,Y2T,KTHM,THENEW,TNEW,APT,AQT)
C
          DO 590 KTH=1,KTHM
 590  TTBL(KTH,KP)=TNEW(KTH)
C-----------------------------------------------------------------------
 550  CONTINUE
C
      RETURN
      END
