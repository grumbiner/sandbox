      SUBROUTINE S2G1(ZS,DS,TS,PS,RS,QS,U,V,T,P,R,Q,
     *                  NSIG,JCAP,NLON,NLATH,
     *       IRS,IIS,AP,BP,AQR,BQR,GR,SLAT,CLAT,PE0,QE0,RO0,TRIGS,IFAX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    S2G1       SPECTRAL TO GRID TRANSFORM
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 90-09-21
C
C ABSTRACT: SUM COEFFICIENTS OF SPHERICAL HARMONIC SERIES
C   FOR ALL SPECTRAL MODEL VARIABLES.
C
C PROGRAM HISTORY LOG:
C   90-09-21  PARRISH
C
C USAGE:    CALL S2G1(ZS,DS,TS,PS,RS,QS,U,V,T,P,R,Q,
C    *                  NSIG,JCAP,NLON,NLATH,
C    *       IRS,IIS,AP,BP,AQR,BQR,GR,SLAT,CLAT,PE0,QE0,RO0,TRIGS,IFAX)
C   INPUT ARGUMENT LIST:
C     ZS       - ZS((JCAP+1)*(JCAP+2)+1,NSIG): VORTICITY COEFFICIENTS
C     DS       - DS((JCAP+1)*(JCAP+2)+1,NSIG): DIVERGENCE COEFFICIENTS
C     TS       - TS((JCAP+1)*(JCAP+2)+1,NSIG): VIRTUAL TEMP COEFS (K)
C     PS       - PS((JCAP+1)*(JCAP+2)+1): LOG(PSF) COEFS  (P IN CB)
C     RS       - RS((JCAP+1)*(JCAP+2)+1): TERRAIN COEFS (M)
C     QS       - QS((JCAP+1)*(JCAP+2)+1,NSIG): SPECIFIC HUMITITY COEFS
C     NSIG     - NUMBER OF SIGMA LEVELS
C     JCAP     - TRIANGULAR TRUNCATION
C     NLON     - NUMBER OF LONGITUDES
C     NLATH    - NUMBER OF GAUSSIAN LATS IN ONE HEMISPHERE
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
C     U        - LONGITUDE COMPONENT OF WINDS
C     V        - LATITUDE COMPONENT OF WINDS
C     T        - TEMPS
C     P        - LOG(PSFC)
C     R        - TERRAIN
C     Q        - MOISTURE VARIABLES
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
         DIMENSION IRS(0:JCAP,0:JCAP),IIS(0:JCAP,0:JCAP)
         double precision AP(0:JCAP,0:JCAP),BP(0:JCAP,0:JCAP)
         double precision AQR(0:JCAP,0:JCAP),BQR(0:JCAP,0:JCAP)
         double precision GR(0:JCAP,0:JCAP)
         double precision SLAT(NLATH),CLAT(NLATH)
         DIMENSION TRIGS(NLON*2),IFAX(10)
         double precision PE0(NLATH,0:JCAP),QE0(NLATH,0:JCAP)
         double precision RO0(NLATH,0:JCAP)
C--------
C-------- INTERNAL SCRATCH DYNAMIC SPACE FOLLOWS:
C--------
         double precision PE(NLATH,0:JCAP),QE(NLATH,0:JCAP)
         double precision RO(NLATH,0:JCAP)
         double precision PO(NLATH,0:JCAP),QO(NLATH,0:JCAP)
         double precision RE(NLATH,0:JCAP)
         DIMENSION WORK(2*NLATH*NLON)
C--------
C-------- MAKE SURE THAT ZONAL IMAGINARY COEFS ARE ZERO, AND
C-------- (0,0) COMPONENT OF Z,D ALSO
C--------
         DO 20 K=1,NSIG
CDIR$ IVDEP
           DO 10 N=0,JCAP
             ZS(IIS(N,0),K)=0.
             DS(IIS(N,0),K)=0.
             TS(IIS(N,0),K)=0.
             QS(IIS(N,0),K)=0.
10         CONTINUE
           ZS(IRS(0,0),K)=0.
           DS(IRS(0,0),K)=0.
20       CONTINUE
CDIR$ IVDEP
         DO 30 N=0,JCAP
           PS(IIS(N,0))=0.
           RS(IIS(N,0))=0.
30       CONTINUE
         PO=0.d0
         QO=0.d0
         RE=0.d0
         PE=PE0
         QE=QE0
         RO=RO0
C--------
C-------- NOW SUM IN LATITUDE (AFTER ZEROING OUTPUT ARRAYS)
C--------
         U=0.
         V=0.
         T=0.
         P=0.
         R=0.
         Q=0.
C--------
         DO 1300 L=0,JCAP
           LR=2*L+1
           LI=2*L+2
           JSTART=1
           IF(L.GT.1) JSTART=2
           DO 900 N=L,JCAP,2
C------------
C------------ FIRST SUM EVEN/ODD TERMS (N=L,L+2,...)
C------------
             DO 200 K=1,NSIG
CDIR$ IVDEP
               DO 100 J=JSTART,NLATH
                 JR=2*NLATH+1-J
                 T(J,LR,K)=T(J,LR,K)+PE(J,L)*TS(IRS(N,L),K)
                 T(J,LI,K)=T(J,LI,K)+PE(J,L)*TS(IIS(N,L),K)
                 Q(J,LR,K)=Q(J,LR,K)+PE(J,L)*QS(IRS(N,L),K)
                 Q(J,LI,K)=Q(J,LI,K)+PE(J,L)*QS(IIS(N,L),K)
                 U(JR,LR,K)=U(JR,LR,K)+RO(J,L)*ZS(IRS(N,L),K)
                 U(J,LR,K)=U(J,LR,K)+QE(J,L)*DS(IIS(N,L),K)
                 U(JR,LI,K)=U(JR,LI,K)+RO(J,L)*ZS(IIS(N,L),K)
                 U(J,LI,K)=U(J,LI,K)-QE(J,L)*DS(IRS(N,L),K)
                 V(J,LR,K)=V(J,LR,K)+QE(J,L)*ZS(IIS(N,L),K)
                 V(JR,LR,K)=V(JR,LR,K)-RO(J,L)*DS(IRS(N,L),K)
                 V(J,LI,K)=V(J,LI,K)-QE(J,L)*ZS(IRS(N,L),K)
                 V(JR,LI,K)=V(JR,LI,K)-RO(J,L)*DS(IIS(N,L),K)
 100           CONTINUE
 200         CONTINUE
CDIR$ IVDEP
             DO 300 J=JSTART,NLATH
               P(J,LR)=P(J,LR)+PE(J,L)*PS(IRS(N,L))
               P(J,LI)=P(J,LI)+PE(J,L)*PS(IIS(N,L))
               R(J,LR)=R(J,LR)+PE(J,L)*RS(IRS(N,L))
               R(J,LI)=R(J,LI)+PE(J,L)*RS(IIS(N,L))
300          CONTINUE
C------------
C------------ NOW DO ODD/EVEN  (N=L+1,L+3,...)
C------------
             IF(N+1.LE.JCAP) THEN
CDIR$ IVDEP
               DO 400 J=JSTART,NLATH
                 PO(J,L)=AP(N,L)*SLAT(J)*PE(J,L)+BP(N,L)*PO(J,L)
                 QO(J,L)=AQR(N,L)*SLAT(J)*QE(J,L)
     *                     +BQR(N,L)*QO(J,L)
                 RE(J,L)=AQR(N,L)*SLAT(J)*RO(J,L)
     *                 +BQR(N,L)*RE(J,L)+GR(N,L)*PE(J,L)*CLAT(J)
400            CONTINUE
               DO 600 K=1,NSIG
CDIR$ IVDEP
                 DO 500 J=JSTART,NLATH
                   JR=2*NLATH+1-J
                   T(JR,LR,K)=T(JR,LR,K)+PO(J,L)*TS(IRS(N+1,L),K)
                   T(JR,LI,K)=T(JR,LI,K)+PO(J,L)*TS(IIS(N+1,L),K)
                   Q(JR,LR,K)=Q(JR,LR,K)+PO(J,L)*QS(IRS(N+1,L),K)
                   Q(JR,LI,K)=Q(JR,LI,K)+PO(J,L)*QS(IIS(N+1,L),K)
                   U(J,LR,K)=U(J,LR,K)+RE(J,L)*ZS(IRS(N+1,L),K)
                   U(JR,LR,K)=U(JR,LR,K)+QO(J,L)*DS(IIS(N+1,L),K)
                   U(J,LI,K)=U(J,LI,K)+RE(J,L)*ZS(IIS(N+1,L),K)
                   U(JR,LI,K)=U(JR,LI,K)-QO(J,L)*DS(IRS(N+1,L),K)
                   V(JR,LR,K)=V(JR,LR,K)+QO(J,L)*ZS(IIS(N+1,L),K)
                   V(J,LR,K)=V(J,LR,K)-RE(J,L)*DS(IRS(N+1,L),K)
                   V(JR,LI,K)=V(JR,LI,K)-QO(J,L)*ZS(IRS(N+1,L),K)
                   V(J,LI,K)=V(J,LI,K)-RE(J,L)*DS(IIS(N+1,L),K)
 500             CONTINUE
 600           CONTINUE
CDIR$ IVDEP
               DO 700 J=JSTART,NLATH
                 JR=2*NLATH+1-J
                 P(JR,LR)=P(JR,LR)+PO(J,L)*PS(IRS(N+1,L))
                 P(JR,LI)=P(JR,LI)+PO(J,L)*PS(IIS(N+1,L))
                 R(JR,LR)=R(JR,LR)+PO(J,L)*RS(IRS(N+1,L))
                 R(JR,LI)=R(JR,LI)+PO(J,L)*RS(IIS(N+1,L))
700            CONTINUE
C--------------
C-------------- GET NEXT PE,QE,RO
C--------------
CDIR$ IVDEP
               DO 800 J=JSTART,NLATH
                 PE(J,L)=AP(N+1,L)*SLAT(J)*PO(J,L)+BP(N+1,L)*PE(J,L)
                 QE(J,L)=AQR(N+1,L)*SLAT(J)*QO(J,L)
     *                     +BQR(N+1,L)*QE(J,L)
                 RO(J,L)=AQR(N+1,L)*SLAT(J)*RE(J,L)
     *                 +BQR(N+1,L)*RO(J,L)+GR(N+1,L)*PO(J,L)*CLAT(J)
800            CONTINUE
             END IF
900        CONTINUE
C----------
C---------- NOW COMBINE EVEN AND ODD PARTS
C----------
           DO 1100 K=1,NSIG
CDIR$ IVDEP
             DO 1000 J=JSTART,NLATH
               JR=2*NLATH+1-J
               TRE=T(J,LR,K)
               TRO=T(JR,LR,K)
               T(J,LR,K)=TRE+TRO
               T(JR,LR,K)=TRE-TRO
               TIE=T(J,LI,K)
               TIO=T(JR,LI,K)
               T(J,LI,K)=TIE+TIO
               T(JR,LI,K)=TIE-TIO
               QRE=Q(J,LR,K)
               QRO=Q(JR,LR,K)
               Q(J,LR,K)=QRE+QRO
               Q(JR,LR,K)=QRE-QRO
               QIE=Q(J,LI,K)
               QIO=Q(JR,LI,K)
               Q(J,LI,K)=QIE+QIO
               Q(JR,LI,K)=QIE-QIO
               URE=U(J,LR,K)
               URO=U(JR,LR,K)
               U(J,LR,K)=URE+URO
               U(JR,LR,K)=URE-URO
               UIE=U(J,LI,K)
               UIO=U(JR,LI,K)
               U(J,LI,K)=UIE+UIO
               U(JR,LI,K)=UIE-UIO
               VRE=V(J,LR,K)
               VRO=V(JR,LR,K)
               V(J,LR,K)=VRE+VRO
               V(JR,LR,K)=VRE-VRO
               VIE=V(J,LI,K)
               VIO=V(JR,LI,K)
               V(J,LI,K)=VIE+VIO
               V(JR,LI,K)=VIE-VIO
 1000        CONTINUE
 1100      CONTINUE
CDIR$ IVDEP
           DO 1200 J=JSTART,NLATH
             JR=2*NLATH+1-J
             PRE=P(J,LR)
             PRO=P(JR,LR)
             P(J,LR)=PRE+PRO
             P(JR,LR)=PRE-PRO
             PIE=P(J,LI)
             PIO=P(JR,LI)
             P(J,LI)=PIE+PIO
             P(JR,LI)=PIE-PIO
             RRE=R(J,LR)
             RRO=R(JR,LR)
             R(J,LR)=RRE+RRO
             R(JR,LR)=RRE-RRO
             RIE=R(J,LI)
             RIO=R(JR,LI)
             R(J,LI)=RIE+RIO
             R(JR,LI)=RIE-RIO
1200       CONTINUE
1300     CONTINUE
C--------
C-------- FINALLY DO FOURIER SUMS IN LONGITUDE
C--------
         LOT=NLATH*2
         DO 1400 K=1,NSIG
           CALL M1FFTM(U(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,1)
           CALL M1FFTM(V(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,1)
           CALL M1FFTM(T(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,1)
           CALL M1FFTM(Q(1,1,K),WORK,TRIGS,IFAX,LOT,1,NLON,LOT,1)
 1400    CONTINUE
         CALL M1FFTM(P,WORK,TRIGS,IFAX,LOT,1,NLON,LOT,1)
         CALL M1FFTM(R,WORK,TRIGS,IFAX,LOT,1,NLON,LOT,1)
       RETURN
       END
