      SUBROUTINE TSIT(HL,TH1,TH2,TSL,TSO,PS,ZL) 
C     COMPUTES INITIAL PS,TSL,TSO 
C     COMPLETELY REWRITTEN 4/27/81
C     FOR 2 LAYER ENERGY EQUATION WHICH 
C     ASSUMES TH1 AND TH2 ARE OCEAN THETAS
C     ZL=ALOG(PSL/PSO) REPLACES THE EXHT FIELD
C 
C 
      COMMON/CONST/ 
     1M,MM,MP,MPP,N,
     2DELPHI,DELT,PI,AEARTH,CA, 
     3CP,CO,CD,P00,P0,
     4SIGMA1,SIGMA2,XKAPPA,SIG,XLC, 
     5XLF,RHOW,RHOS,DIFF,XKS, 
     6XKI,XIMIN,SC, 
     7ITMAX,XLAM,XLS,PWLF,
     8PWLS,CDPW,CDCP,CDLC,CDLS, 
     9PWPS,XKSKI,RSEA,SGKP1,SGKP2,
     1ZSGKPA,ZSGKPB,CLRE,SQ1,SQ2
C     NEW COMMON BLOCK ZCONST 4/27/81 
      COMMON/ZCONST/Z1,Z2,DZ,DZI,PPSKZ1,PPSKZ2,GEE,SIPERMB,RGEE 
C     COMMON/ZCONST/INITIALIZED IN REVISED CON 4/27/81
C     Z1=ALOG(SIGMA1) 
C     Z2=ALOG(SIGMA2) 
C     DZ=Z1-Z2
C     DZI=1./DZ 
C     PPSKZ1=(SIGMA1*(P0/P00))**XKAPPA*DZI
C     PPSKZ2=(SIGMA2*(P0/P00))**XKAPPA*DZI
C     ACCELERATION OF GRAVITY (METERS/SEC)
C     GEE=9.8 
C     CONVERSION FACTOR BETWEEN SI AND MILLIBAR PRESSURE UNITS
C     SIPERMB=1.E2
C     NOTE R=XKAPPA*CP (R IS GAS LAW CONSTANT)
C     RGEE=XKAPPA*CP/GEE
C     IN /CONST/
C     CO=SIPERMB*(CP/(2.*GEE))
C     STATEMENT FUNCTIONS 
      ROOTP(A,B)=(-B+SQRT(B*B-4.*A))/(2.*A) 
C     NEW FORMULA FOR TSL 4/27/81 
      TSLAND(TH1,TH2,ZL)=PPSKZ1*TH1*(ZL-Z2)+PPSKZ2*TH2*(Z1-ZL)
C     A1(H)=PPSKZ1*RGEE*TH1/H 
C     A2(H)=PPSKZ2*RGEE*TH2/H 
      AA(H)=(PPSKZ1*RGEE*TH1/H-(PPSKZ2*RGEE*TH2/H))/2.0 
      BB(H)=(PPSKZ2*RGEE*TH2/H)*Z1-PPSKZ1*RGEE*TH1*Z2/H 
C     (OLD)FORMULA FOR TSO
C     TSSEA(TH1,TH2)=RSEA*(TH2*ZSGKPA+TH1*ZSGKPB) 
C 
      IF (HL.EQ.0.0) THEN 
        PS=P0 
        ZL=0.0
       ELSE 
C       SOLVE QUADRATIC FOR ZL=ALOG(PSL/PSO)
        ZL=ROOTP(AA(HL),BB(HL)) 
        PS=P0*EXP(ZL) 
      ENDIF 
      TSO=RSEA*(TH2*ZSGKPA+TH1*ZSGKPB)
      TSL=TSLAND(TH1,TH2,ZL)
      RETURN
      END 