      SUBROUTINE EQTSTX(NS,NSZ,EQSKP) 
C     CONVERTED TO MACHINE INDEPENDANT CODE 8/12/83.  BOB GRUMBINE
C     SKIPTEST WHEN FM=1,FIX VARIABLE NAMES IN EQCOM       10/11/79 
C     DO TESTS ON WINTER HEMISPHERE 10/11/79
C     TEST FOR EQUILBRIUM AT THE
C     SOLSTICES WHEN OPERATING ON THE 
C     COURSE GRID9 SET EQSKP=.TRUE. 
C     AND PREPARE TO EXIT LOOP IF THE 
C     TEST IS SATISFIED FOR TWO SOLSTICES 
      COMMON/CONST/ 
     1 M,MM,MP,MPP,N, 
     2 DELPHI,DELT,PI,AEARTH,CA,
     3 CP,CO,CD,P00,P0, 
     4 SIGMA1,SIGMA2,XKAPPA,SIG,XLC,
     5 XLF,RHOW,RHOS,DIFF,XKS,
     6 XKI,XIMIN,SC,
     7 ITMAX,XLAM,XLS,PWLF, 
     8 PWLS,CDPW,CDCP,CDLC,CDLS,
     9 PWPS,XKSKI,RSEA,SGKP1,SGKP2, 
     1 ZSGKPA,ZSGKPB,CLRE,SQ1,SQ2 
      COMMON/LARGE/NFLD,
     1 JTH1P,JTH2P,JTSLP,JTSOP,JTXLP,JTXOP, 
     2 JTMP,JHLP,JHOP,JXIOP,JQMLP,JQMOP,
     3 JTH1,JTH2,JTSL,JTSO,JTXL,JTXO, 
     4 JTM,JHL,JHO,JXIO,JQML,JQMO 
C     LARGE CONTAINS THE POINTERS TO
C     THE PAIRS OF  'MAJOR' FIELDS IN W . VARIABLE
C     NAMES MADE BY ADDING J TO THE NAMES USED FOR THE DATA.
      EQUIVALENCE(JFLD(1,1),JTH1P)
      DIMENSION JFLD(12,2)
      COMMON/MINOR/NMINOR,
     1 JFM,JRSN,JPS,JSP,JSPP,JCZB,JCZBP,
     2 JQ1P,JQ2P
     3  ,JEXHT
      DIMENSION KFLD(26)
      EQUIVALENCE (KFLD(1),JFM) 
C     MINOR POINTS TO OTHER FIELDS IN W 
C     NMINOR IS THE NUMBER OF POINTERS IN MINOR.
      COMMON/CFCOM/NCG,NSC
      COMMON/EQCOM/NMS,NMW,NMSKP, 
     1 LNS,LNW,MZH,DTTST,DITST, 
     2 QTST,KTNS,KINS,KTNW,KINW,
     3 LEQTST,IU
      LOGICAL LNS,LNW,LEQTST,EQSKP,IPRINT 
      COMMON W(46,50) 
      DIMENSION T(46),XI(46)
      LOGICAL SUM 
      DIMENSION QC(46),TOL(3) 
      DIMENSION KSEA(23),NOTT(23),NOTI(23),ISET(23),NOTQ(23), 
     1  KILLA(23),KILLB(23),NTEST(23),ICE(23) 
      LOGICAL NTE0
      CHARACTER*1 PIC(11,11)
      INTEGER B,BITS(8) 
      DATA IPRINT/.FALSE./
      DATA KSEA,NOTT,NOTI,ISET/23*0,23*0,23*0,23*0/ 
      DATA NOTQ,KILLA,KILLB,NTEST/23*0,23*0,23*0,23*0/
      DATA ICE/23*0/
      MMOD(I,J)=MOD(J+MOD(I,J),J) 
      B(N)=MIN0(1,1*N)
      EQSKP=.FALSE. 
      IF (.NOT.LEQTST) RETURN 
      IF (MMOD(NS,NCG).EQ.NMS) GO TO 70 
      IF (MMOD(NS,NCG).EQ.NMW) GO TO 80 
  90  IF ((MMOD(NS,NCG).EQ.NMSKP).AND.LNS.AND.LNW)
     1 GO TO 100
      RETURN
C     DO TESTS ON WINTER HEMISPHERE 10/11/79
C     NORTHERN HEMISPHERE SUMMER SOLSTICE 
C     TEST IN SOUTH 
  70  MA=1
      MB=MZH-1
      SUM=.TRUE.
      KT=KTNS 
      KI=KINS 
      GO TO 10
C     NORTHERN HEMISPHERE WINTER SOLSTICE 
C     TEST IN NORTH 
  80  MA=MZH
      MB=MPP
      SUM=.FALSE. 
      KT=KTNW 
      KI=KINW 
  10  IF (NS-NSZ.LT.NCG) THEN 
        NTE0=.FALSE.
        GO TO 56
      ENDIF 
CLINUX      CALL READMS(IU,T,MPP,KT)
CLINUX      CALL READMS(IU,XI,MPP,KI) 
      NTE0=.TRUE. 
      IF (SUM)MZ=MA-1 
      IF (.NOT.SUM)MZ=MB+1
      IF (SUM)ICMZ=1
      IF (.NOT.SUM)ICMZ=-1
C     INITIALIZE ARRAYS TO 0
      DO 15 II=1,23 
        KSEA(II)=0
        NOTT(II)=0
        NOTI(II)=0
        ICE(II)=0 
        NOTQ(II)=0
 15   CONTINUE
      IPOS=0
      DO 30 MZK=MA,MB 
      IPOS=IPOS+1 
      MZ=MZ+ICMZ
C     SET ARRAY ELEMENT 1 FOR CONDITIONS NOT SATISFIED. 
      IF (W(MZ,JFM).NE.1) KSEA(IPOS)=1
      IF (ABS(T(MZ)-W(MZ,JTXO)).GE.DTTST) NOTT(IPOS)=1
      IF (ABS(XI(MZ)-W(MZ,JXIO)).GE.DITST) NOTI(IPOS)=1 
      IF (W(MZ,JXIO).GT.0.0) ICE(IPOS)=1
      QC(MZ)=QCO(W(MZ,JTXO),0.0,W(MZ,JXIO)) 
      IF (ABS(QC(MZ)).GE.QTST) NOTQ(IPOS)=1 
 30   CONTINUE
      DO 35 MZK=1,IPOS
      KILLA(MZK)=KSEA(MZK)*NOTT(MZK)
      KILLB(MZK)=KSEA(MZK)*NOTI(MZK)*NOTQ(MZK)*ICE(MZK) 
      NTEST(MZK)=KILLA(MZK)+KILLB(MZK)
      IF(NTEST(MZK).NE.0) NTE0=.FALSE.
 35   CONTINUE
      WRITE(6,31)NS,NTEST,KILLA,KILLB,KSEA,NOTT,NOTI,NOTQ,ICE,SUM 
 31   FORMAT(' EQ ',I6,4(1X,23I1)/4(1X,23I1),1X,L1) 
 56   IF (.NOT.NTE0) THEN 
C       TEST NOT SATISFIED
        IF (SUM)LNS=.FALSE. 
        IF (.NOT.SUM)LNW=.FALSE.
       ELSE 
C       TEST SATISFIED
        IF (SUM)LNS=.TRUE.
        IF (.NOT.SUM)LNW=.TRUE. 
      ENDIF 
CLINUX      CALL WRITMS(IU,W(1,JTXO),MPP,KT,-1,0) 
CLINUX      CALL WRITMS(IU,W(1,JXIO),MPP,KI,-1,0) 
      GO TO 90
C     PREPARE FOR IMMEDIATE SKIP OUT OF LOOP
C     ON RETURN TO TWOB 
  100 EQSKP=.TRUE.
      WRITE(6,101)NS,DTTST,DITST,QTST 
 101  FORMAT(//27H EQULIBRIUM TEST LOOP EXIT   ,I10,3G15.8) 
      NSZ=NS
      IF (.NOT.IPRINT) RETURN 
      ENTRY EQTSTP
      IF (SUM)MZ=MA-1 
      IF (.NOT.SUM)MZ=MB+1
      IPOS=0
      DO 40 MZK=MA,MB 
      MZ=MZ+ICMZ
      IPOS=IPOS+1 
      BITS(1)=B(NTEST(IPOS))
      BITS(2)=B(KILLA(IPOS))
      BITS(3)=B(KILLB(IPOS))
      BITS(4)=B(KSEA(IPOS)) 
      BITS(5)=B(NOTT(IPOS)) 
      BITS(6)=B(NOTI(IPOS)) 
      BITS(7)=B(NOTQ(IPOS)) 
      BITS(8)=B(ICE(IPOS))
      MS=MZ-1 
C     DONT USE STATEMENT FUNCTION IN I/O LIST AT NCAR 
      WRITE(6,112)MS, 
     1 W(MZ,JTXO),T(MZ),W(MZ,JXIO),XI(MZ),QC(MZ), 
     2 BITS 
 112  FORMAT(' EQ ',I6,5G15.8,2(3I2,1X,I2,1X))
 40   CONTINUE
      DO 42 L=1,3 
 42   TOL(L)=0.0
      DO 43 MZ=MA,MB
      IF (W(MZ,JFM).EQ.1.0) GO TO 43
      TOL(1)=AMAX1(TOL(1),ABS(T(MZ)-W(MZ,JTXO)))
      IF (W(MZ,JXIO).EQ.0.0)GO TO 43
      TOL(2)=AMAX1(TOL(2),ABS(XI(MZ)-W(MZ,JXIO))) 
      TOL(3)=AMAX1(TOL(3),ABS(QC(MZ)))
 43   CONTINUE
      WRITE(6,44)TOL
 44   FORMAT(' MAXDT=',G15.8,'MAXDI=',G15.8,'MAXQC=',G15.8) 
      DO 46 II=1,11 
      DO 46 L=1,11
 46   PIC(L,II)=' ' 
      DO 45 L=2,10
      PIC(11,L)='I' 
      PIC(1,L)='I'
      PIC(L,11)='-' 
 45   PIC(L,1)='-'
      PIC(11,11)='+'
      PIC(11,1)='+' 
      PIC(1,11)='+' 
      PIC(1,1)='+'
      IIP=11.5-10.*DITST/TOL(2) 
      IF (IIP.LE.11.AND.IIP.GE.1) THEN
        DO 49 L=1,11
 49     PIC(L,IIP)='='
      ENDIF 
      IQP=1.5+10.*QTST/TOL(3) 
      IF (IQP.LE.11.AND.IQP.GE.1) THEN
        DO 52 L=1,11
 52     PIC(IQP,L)='Q'
      ENDIF 
      DO 47 MZ=MA,MB
      IF (W(MZ,JFM).NE.1.0.AND.W(MZ,JXIO).NE.0.0) THEN
        IIP=11.5-10.*ABS(XI(MZ)-W(MZ,JXIO))/TOL(2)
        IQP=1.5+ 10.*ABS(QC(MZ))/TOL(3) 
        PIC(IQP,IIP)='*'
      ENDIF 
 47   CONTINUE
      WRITE(6,48)PIC
 48   FORMAT(5X,11A1) 
      RETURN
      ENTRY EQSETX(NS,NSZ)
C     SET VARIABLES IN COMMON/EQCOM/
      LEQTST=.TRUE. 
C     POSITION OF INITIAL CONDITION 
      NMSKP=MMOD(NSZ,NCG) 
C     POSITION OF SOLSTICES (APPROX)
      NMS=NSC*IFIX(0.5+0.25*N)
      NMW=NSC*IFIX(0.5+0.75*N)
C     LOGICAL FLAGS 
      LNW=.FALSE. 
      LNS=.FALSE. 
C     EQUATOR 
      MZH=(MP/2.+1.5) 
C     RECORD KEYS ON DISK FILE
C     SAME DISK FILE USED FOR CYCSUB AND GRID 
      KTNS=NFLD+NMINOR+1
      KINS=KTNS+1 
      KTNW=KINS+1 
      KINW=KTNW+1 
      RETURN
      END 
