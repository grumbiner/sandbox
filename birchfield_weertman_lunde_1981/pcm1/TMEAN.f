      SUBROUTINE TMEAN(NS,NSZ)
C     COMPUTE GLOBAL/HEMISPHERE ANNUAL MEAN TS AND TX VALUES  10
C     COMPUTE TMIX ANNUAL MEAN  2/4/81
C     TEST TO SEE IF IT IS CORRECT BLOCK TO AVERAGE MOVED TO TWOB 10/3/83 
      COMMON W(46,50) 
      COMMON/MINOR/NMINOR,JFM,DUMA(25)
      COMMON/MIXCOM/CMIX
      COMMON/CONST/M,MM,MP,MPP,N,DUMM(42) 
      COMMON/LARGE/NFLD,
     1JTH1P,JTH2P,JTSLP,JTSOP,JTXLP,JTXOP,
     2JTMP,JHLP,JHOP,JXIOP,JQMLP,JQMOP, 
     3JTH1,JTH2,JTSL,JTSO,JTXL,JTXO,
     4JTM,JHL,JHO,JXIO,JQML,JQMO
      COMMON/CFCOM/NCG,NSC
      REAL TSAM(3),TXAM(3),TMAM(3),AVL(3),AVO(3)
      REAL TMEM(3,2),TMFLUX(3)
      DATA TSAM,TXAM,TMAM/9*0.0/
      MMOD(I,J)=MOD(J+MOD(I,J),J) 
      CALL SPLAND(JTSL,AVL(1),AVL(2),AVL(3),JFM)
      CALL SPSEA(JTSO,AVO(1),AVO(2),AVO(3),JFM) 
      DO 21 L=1,3 
 21   TSAM(L)=TSAM(L)+AVL(L)+AVO(L) 
      CALL SPLAND(JTXL,AVL(1),AVL(2),AVL(3),JFM)
      CALL SPSEA(JTXO,AVO(1),AVO(2),AVO(3),JFM) 
      DO 22 L=1,3 
 22   TXAM(L)=TXAM(L)+AVL(L)+AVO(L) 
      CALL SPNORM(JTM,AVO(1),AVO(2),AVO(3),3,1) 
      DO 23 L=1,3 
 23   TMAM(L)=TMAM(L)+AVO(L)
      IF(MMOD(NS-NSZ,NCG).NE.0) RETURN
      II=MIN0(2,MAX0(1,(NS-NSZ)/NCG)) 
      DO 30 L=1,3 
      TMAM(L)=TMAM(L)/N 
      TSAM(L)=TSAM(L)/N 
      TXAM(L)=TXAM(L)/N 
 30   TMEM(L,II)=TXAM(L)
      IF(II.NE.2) THEN
        WRITE(6,10) NS,TSAM,TXAM,TMAM 
 10   FORMAT(' MEAN ANNUAL ',I5,
     1 ' TS G=',G15.8,' N=',G15.8,' S=',G15.8/18X,
     1 6H T* G= ,G15.8,' N=',G15.8,' S=',G15.8/16X, 
     1 ' TMIX G=',G15.8,' N=',G15.8,' S=',G15.8/4X, 
     1 ' INTO MIXED LAYER G=',G15.8,' N=',G15.8,' S=',G15.8)
      ELSE
        DO 52 L=1,3 
        TMFLUX(L)=(TMEM(L,2)-TMEM(L,1))*CMIX/31556900.
 52     TMEM(L,1)=TMEM(L,2) 
        WRITE(6,10) NS,TSAM,TXAM,TMAM,TMFLUX
      ENDIF 
      ENTRY TMSET 
      DO 40 L=1,3 
      TMAM(L)=0.0 
      TSAM(L)=0.0 
 40   TXAM(L)=0.0 
      RETURN
      END 
