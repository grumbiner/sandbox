C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE TABLEQ(TTBLQ
     &,                 RDP,RDTHE,PL,THL,STHE,THE0)
C     ******************************************************************
C     *                                                                *
C     *        GENERATE VALUES FOR FINER LOOK-UP TABLES USED           *
C     *                       IN CONVECTION                            *
C     *                                                                *
C     ******************************************************************
                             P A R A M E T E R
     & (ITB=152,JTB=440)
                             P A R A M E T E R
     & (THH=325.,PH=105000.
     &, PQ0=379.90516,A1=610.78,A2=17.2693882,A3=273.16,A4=35.86
     &, R=287.04,CP=1004.6,ELIWV=2.683E6,EPS=1.E-9)
                             D I M E N S I O N
     &  TTBLQ (JTB,ITB)
     &, TOLD  (JTB),THEOLD(JTB),THE0  (ITB),STHE  (ITB)
     &, Y2T   (JTB),THENEW(JTB),APT   (JTB),AQT   (JTB),TNEW  (JTB)
C--------------COARSE LOOK-UP TABLE FOR SATURATION POINT----------------
      KTHM=JTB
      KPM=ITB
      KTHM1=KTHM-1
      KPM1=KPM-1
C
      DTH=(THH-THL)/REAL(KTHM-1)
      DP =(PH -PL )/REAL(KPM -1)
C
      RDP=1./DP
      TH=THL-DTH
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
      THEOLD(KTH)=TH*EXP(ELIWV*QS/(CP*TOLD(KTH)))
 560  CONTINUE
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
      THENEW(KTH)=THENEW(KTH-1)+DTHE
 580  CONTINUE
C
      Y2T(1   )=0.
      Y2T(KTHM)=0.
C
      CALL SPLINE(JTB,KTHM,THEOLD,TOLD,Y2T,KTHM,THENEW,TNEW,APT,AQT)
C
          DO 590 KTH=1,KTHM
      TTBLQ(KTH,KP)=TNEW(KTH)
 590  CONTINUE
C-----------------------------------------------------------------------
 550  CONTINUE
C
      RETURN
      END
