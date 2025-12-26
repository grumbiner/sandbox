       SUBROUTINE G2S1(ZS,DS,TS,PS,RS,QS,U,V,T,P,R,Q,
     *             NSIG,JCAP,NLON,NLATH,WGTS,
     *       IRS,IIS,AP,BP,AQR,BQR,GR,SLAT,CLAT,PE0,QE0,RO0,TRIGS,IFAX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    G2S1       GRID TO SPECTRAL TRANSFORM
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 90-09-21
C
C ABSTRACT: CONVERT ALL SPECTRAL MODEL VARIABLES FROM GRID
C   TO COEFFICIENT FORM.
C
C PROGRAM HISTORY LOG:
C   90-09-21  PARRISH
C
C USAGE:    CALL G2S1(ZS,DS,TS,PS,RS,QS,U,V,T,P,R,Q,
C    *             NSIG,JCAP,NLON,NLATH,WGTS,
C    *       IRS,IIS,AP,BP,AQR,BQR,GR,SLAT,CLAT,PE0,QE0,RO0,TRIGS,IFAX)
C   INPUT ARGUMENT LIST:
C     U        - LONGITUDE COMPONENT OF WINDS
C     V        - LATITUDE COMPONENT OF WINDS
C     T        - TEMPS
C     P        - LOG(PSFC)
C     R        - TERRAIN
C     Q        - MOISTURE VARIABLES
C     NSIG     - NUMBER OF SIGMA LEVELS
C     JCAP     - TRIANGULAR TRUNCATION
C     NLON     - NUMBER OF LONGITUDES
C     NLATH    - NUMBER OF GAUSSIAN LATS IN ONE HEMISPHERE
C     WGTS     - WGTS(NLATH): INTEGRATION WEIGHTS
C     IRS      - IRS(0:JCAP,0:JCAP): ADDR OF RE(ZS(N,L)), ETC.
C     IIS      - IIS(0:JCAP,0:JCAP): SAME FOR IM PART
C     AP       - AP(0:JCAP,0:JCAP):  RECURSION
C     BP       - BP(0:JCAP,0:JCAP):    CONSTANTS
C     AQR      - AQR(0:JCAP,0:JCAP):     FOR GENERATING
C     BQR      - BQR(0:JCAP,0:JCAP):       VECTOR SPHERICAL
C     GR       - GR(0:JCAP,0:JCAP):          HARMONICS
C     SLAT     - SLAT(NLATH):  SIN(GAUSSIAN LATITUDES)
C     CLAT     - CLAT(NLATH):  COS(GAUSSIAN LATS)
C     PE0      - PE0(NLATH,0:JCAP): P(L,L)
C     QE0      - QE0(NLATH,0:JCAP): Q(L,L)
C     RO0      - RO0(NLATH,0:JCAP): R(L,L)
C     TRIGS    - TRIGS(NLON*2)
C     IFAX     - IFAX(10)
C
C   OUTPUT ARGUMENT LIST:
C     ZS       - ZS((JCAP+1)*(JCAP+2)+1,NSIG): VORTICITY COEFFICIENTS
C     DS       - DS((JCAP+1)*(JCAP+2)+1,NSIG): DIVERGENCE COEFFICIENTS
C     TS       - TS((JCAP+1)*(JCAP+2)+1,NSIG): VIRTUAL TEMP COEFS (K)
C     PS       - PS((JCAP+1)*(JCAP+2)+1): LOG(PSF) COEFS  (P IN CB)
C     RS       - RS((JCAP+1)*(JCAP+2)+1): TERRAIN COEFS (M)
C     QS       - QS((JCAP+1)*(JCAP+2)+1,NSIG): SPECIFIC HUMITITY COEFS
C
C ATTRIBUTES:
C   LANGUAGE: CFT77
C   MACHINE:  CRAY YMP
C
C$$$
         DIMENSION ZS((JCAP+1)*(JCAP+2)+1,NSIG)
         DIMENSION DS((JCAP+1)*(JCAP+2)+1,NSIG)
         DIMENSION TS((JCAP+1)*(JCAP+2)+1,NSIG)
         DIMENSION PS((JCAP+1)*(JCAP+2)+1)
         DIMENSION RS((JCAP+1)*(JCAP+2)+1)
         DIMENSION QS((JCAP+1)*(JCAP+2)+1,NSIG)
         DIMENSION U(2*NLATH,NLON+1,NSIG),V(2*NLATH,NLON+1,NSIG)
         DIMENSION T(2*NLATH,NLON+1,NSIG),Q(2*NLATH,NLON+1,NSIG)
         DIMENSION P(2*NLATH,NLON+1),R(2*NLATH,NLON+1)
         double precision WGTS(NLATH)
         DIMENSION IRS(0:JCAP,0:JCAP),IIS(0:JCAP,0:JCAP)
         double precision AP(0:JCAP,0:JCAP),BP(0:JCAP,0:JCAP)
         double precision AQR(0:JCAP,0:JCAP),BQR(0:JCAP,0:JCAP)
         double precision GR(0:JCAP,0:JCAP)
         double precision  SLAT(NLATH),CLAT(NLATH)
         double precision PE0(NLATH,0:JCAP),QE0(NLATH,0:JCAP)
         double precision RO0(NLATH,0:JCAP)
         DIMENSION TRIGS(NLON*2),IFAX(10)
C--------
C-------- INTERNAL SCRATCH DYNAMIC SPACE FOLLOWS:
C--------
         double precision PE(NLATH,0:JCAP),QE(NLATH,0:JCAP)
         double precision RO(NLATH,0:JCAP)
         double precision PO(NLATH,0:JCAP),QO(NLATH,0:JCAP)
         double precision RE(NLATH,0:JCAP)
         DIMENSION WORK(2*NLATH*NLON)
C--------
         RERTH=CONMC('RERTH$')
C--------
C-------- NEXT DO FOURIER ANALYSIS IN LONGITUDE
C--------
         LOT=NLATH*2
         DO 1400 K=1,NSIG
           CALL M1FFTM(U(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,-1)
           CALL M1FFTM(V(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,-1)
           CALL M1FFTM(T(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,-1)
           CALL M1FFTM(Q(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,-1)
 1400    CONTINUE
         CALL M1FFTM(P,WORK,TRIGS,IFAX,LOT,1,NLON,LOT,-1)
         CALL M1FFTM(R,WORK,TRIGS,IFAX,LOT,1,NLON,LOT,-1)
         JCAP2=JCAP+2
         PO=0.d0
         QO=0.d0
         RE=0.d0
         PE=PE0
         QE=QE0
         RO=RO0
C--------
C-------- NOW SUM IN LATITUDE (AFTER ZEROING OUTPUT ARRAYS)
C--------
         ZS=0.
         DS=0.
         TS=0.
         PS=0.
         RS=0.
         QS=0.
C--------
         DO 1300 J=2,NLATH
           JR=2*NLATH+1-J
C----------
C---------- FIRST SEPERATE INTO EVEN AND ODD PARTS
C----------
CDIR$ IVDEP
             DO 1000 LL=1,NSIG*(NLON+1)
               TRE=T(J,LL,1)
               TRO=T(JR,LL,1)
               T(J,LL,1)=(TRE+TRO)*WGTS(J)
               T(JR,LL,1)=(TRE-TRO)*WGTS(J)
               QRE=Q(J,LL,1)
               QRO=Q(JR,LL,1)
               Q(J,LL,1)=(QRE+QRO)*WGTS(J)
               Q(JR,LL,1)=(QRE-QRO)*WGTS(J)
               URE=U(J,LL,1)
               URO=U(JR,LL,1)
               U(J,LL,1)=(URE+URO)*WGTS(J)
               U(JR,LL,1)=(URE-URO)*WGTS(J)
               VRE=V(J,LL,1)
               VRO=V(JR,LL,1)
               V(J,LL,1)=(VRE+VRO)*WGTS(J)
               V(JR,LL,1)=(VRE-VRO)*WGTS(J)
 1000        CONTINUE
CDIR$ IVDEP
           DO 1200 LL=1,JCAP*2+2
             PRE=P(J,LL)
             PRO=P(JR,LL)
             P(J,LL)=(PRE+PRO)*WGTS(J)
             P(JR,LL)=(PRE-PRO)*WGTS(J)
             RRE=R(J,LL)
             RRO=R(JR,LL)
             R(J,LL)=(RRE+RRO)*WGTS(J)
             R(JR,LL)=(RRE-RRO)*WGTS(J)
1200       CONTINUE
           DO 900 M=0,JCAP,2
C------------
C------------ FIRST SUM EVEN/ODD TERMS (N=L,L+2,...)
C------------
             DO 200 K=1,NSIG
               IR=IRS(M,0)-2
               LR=-1
CDIR$ IVDEP
               DO 100 L=0,JCAP-M
                 LR=LR+2
                 LI=LR+1
                 IR=IR+2
                 II=IR+1
                 TS(IR,K)=TS(IR,K)+PE(J,L)*T(J,LR,K)
                 TS(II,K)=TS(II,K)+PE(J,L)*T(J,LI,K)
                 QS(IR,K)=QS(IR,K)+PE(J,L)*Q(J,LR,K)
                 QS(II,K)=QS(II,K)+PE(J,L)*Q(J,LI,K)
                 ZS(IR,K)=ZS(IR,K)-V(J,LI,K)*QE(J,L)+U(JR,LR,K)*RO(J,L)
                 ZS(II,K)=ZS(II,K)+V(J,LR,K)*QE(J,L)+U(JR,LI,K)*RO(J,L)
                 DS(IR,K)=DS(IR,K)-U(J,LI,K)*QE(J,L)-V(JR,LR,K)*RO(J,L)
                 DS(II,K)=DS(II,K)+U(J,LR,K)*QE(J,L)-V(JR,LI,K)*RO(J,L)
 100           CONTINUE
 200         CONTINUE
             IR=IRS(M,0)-2
             LR=-1
CDIR$ IVDEP
             DO 300 L=0,JCAP-M
               LR=LR+2
               LI=LR+1
               IR=IR+2
               II=IR+1
               PS(IR)=PS(IR)+PE(J,L)*P(J,LR)
               PS(II)=PS(II)+PE(J,L)*P(J,LI)
               RS(IR)=RS(IR)+PE(J,L)*R(J,LR)
               RS(II)=RS(II)+PE(J,L)*R(J,LI)
300          CONTINUE
C------------
C------------ NOW DO ODD/EVEN  (N=L+1,L+3,...)
C------------
             IF(M+1.LE.JCAP) THEN
               I=M-JCAP2
CDIR$ IVDEP
               DO 400 L=0,JCAP-M-1
                 I=I+JCAP2
                 PO(J,L)=AP(I,0)*SLAT(J)*PE(J,L)+BP(I,0)*PO(J,L)
                 QO(J,L)=AQR(I,0)*SLAT(J)*QE(J,L)
     *                     +BQR(I,0)*QO(J,L)
                 RE(J,L)=AQR(I,0)*SLAT(J)*RO(J,L)
     *                 +BQR(I,0)*RE(J,L)+GR(I,0)*PE(J,L)*CLAT(J)
400            CONTINUE
               DO 600 K=1,NSIG
                 IR=IRS(M+1,0)-2
                 LR=-1
CDIR$ IVDEP
                 DO 500 L=0,JCAP-M-1
                   IR=IR+2
                   II=IR+1
                   LR=LR+2
                   LI=LR+1
                   TS(IR,K)=TS(IR,K)+PO(J,L)*T(JR,LR,K)
                   TS(II,K)=TS(II,K)+PO(J,L)*T(JR,LI,K)
                   QS(IR,K)=QS(IR,K)+PO(J,L)*Q(JR,LR,K)
                   QS(II,K)=QS(II,K)+PO(J,L)*Q(JR,LI,K)
                  ZS(IR,K)=ZS(IR,K)+U(J,LR,K)*RE(J,L)-V(JR,LI,K)*QO(J,L)
                  ZS(II,K)=ZS(II,K)+U(J,LI,K)*RE(J,L)+V(JR,LR,K)*QO(J,L)
                  DS(IR,K)=DS(IR,K)-V(J,LR,K)*RE(J,L)-U(JR,LI,K)*QO(J,L)
                  DS(II,K)=DS(II,K)-V(J,LI,K)*RE(J,L)+U(JR,LR,K)*QO(J,L)
 500             CONTINUE
 600           CONTINUE
               IR=IRS(M+1,0)-2
               LR=-1
CDIR$ IVDEP
               DO 700 L=0,JCAP-M-1
                 IR=IR+2
                 II=IR+1
                 LR=LR+2
                 LI=LR+1
                 PS(IR)=PS(IR)+PO(J,L)*P(JR,LR)
                 PS(II)=PS(II)+PO(J,L)*P(JR,LI)
                 RS(IR)=RS(IR)+PO(J,L)*R(JR,LR)
                 RS(II)=RS(II)+PO(J,L)*R(JR,LI)
700            CONTINUE
C--------------
C-------------- GET NEXT PE,QE,RO
C--------------
               I=M-JCAP2+1
CDIR$ IVDEP
               DO 800 L=0,MAX(0,JCAP-M-2)
                 I=I+JCAP2
                 PE(J,L)=AP(I,0)*SLAT(J)*PO(J,L)+BP(I,0)*PE(J,L)
                 QE(J,L)=AQR(I,0)*SLAT(J)*QO(J,L)
     *                     +BQR(I,0)*QE(J,L)
                 RO(J,L)=AQR(I,0)*SLAT(J)*RE(J,L)
     *                 +BQR(I,0)*RO(J,L)+GR(I,0)*PO(J,L)*CLAT(J)
800            CONTINUE
             END IF
900        CONTINUE
1300     CONTINUE
C--------
C------- MULTIPLY DS,ZS BY DIMENSIONAL LAPLACIAN
C--------
         DO 1292 L=0,JCAP
           DO 1290 N=L,JCAP
             DEL2=N*(N+1.)/(RERTH*RERTH)
CDIR$ IVDEP
             DO 1280 K=1,NSIG
               ZS(IRS(N,L),K)=DEL2*ZS(IRS(N,L),K)
               ZS(IIS(N,L),K)=DEL2*ZS(IIS(N,L),K)
               DS(IRS(N,L),K)=DEL2*DS(IRS(N,L),K)
               DS(IIS(N,L),K)=DEL2*DS(IIS(N,L),K)
 1280        CONTINUE
 1290      CONTINUE
 1292    CONTINUE
C--------
C-------- MAKE SURE THAT ZONAL IMAGINARY COEFS ARE ZERO, AND
C-------- (0,0) COMPONENT OF Z,D ALSO
C--------
         DO 20 K=1,NSIG
           DO 10 N=0,JCAP
             ZS(IIS(N,0),K)=0.
             DS(IIS(N,0),K)=0.
             TS(IIS(N,0),K)=0.
             QS(IIS(N,0),K)=0.
 10        CONTINUE
           ZS(IRS(0,0),K)=0.
           DS(IRS(0,0),K)=0.
 20      CONTINUE
         DO 30 N=0,JCAP
           PS(IIS(N,0))=0.
           RS(IIS(N,0))=0.
30       CONTINUE
       RETURN
       END
