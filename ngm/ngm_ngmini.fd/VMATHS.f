       SUBROUTINE VMATHS(HTBAR,HDSIG,HPR,HCP,HRKAPPA,HGASCON,
     *                   HSIGHAT,HDEPTHS,HEINVT,HEINV,HEINVG,
     *                   HPIBINVE,HTBINVE,HE)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    VMATHS      COMPUTE VERT MODES AND DEPTHS
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-09-08
C
C ABSTRACT: COMPUTE VERTICAL MODES AND SCALE DEPTHS.
C
C PROGRAM HISTORY LOG:
C   88-09-08  PARRISH
C
C USAGE:    CALL VMATHS(HTBAR,HDSIG,HPR,HCP,HRKAPPA,HGASCON,
C    *                   HSIGHAT,HDEPTHS,HEINVT,HEINV,HEINVG,
C    *                   HPIBINVE,HTBINVE,HE)
C   INPUT ARGUMENT LIST:
C     HTBAR    - REF TEMP PROFILE
C     HDSIG    - SIGMA INCREMENTS
C     HPR      - RELATED TO EXNER FUNCTION
C     HSIGHAT  - SIGMA VALUES AT CENTERS OF SIGMA LAYERS.
C     HCP      - SPEC HEAT AT CONST PRESSURE
C     HRKAPPA  - RATIO GAS CONST TO SPEC HEAT
C     HGASCON  - GAS CONST
C
C   OUTPUT ARGUMENT LIST:
C     HDEPTHS  - SCALE DEPTHS
C     HEINVT   - MATRIX OPERATOR
C     HEINV    - INVERSE OF MATRIX OF EIGENVALUES
C     HEINVG   - MATRIX OPERATOR
C     HPIBINVE - MATRIX OPERATOR
C     HTBINVE  - MATRIX OPERATOR
C     HE       - MATRIX OF EIGENVECTORS OF B.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
         include "myparam"
         COMPLEX DEPTHC(INSIG),EC(INSIG,INSIG),CTEMP
         REAL DEPTHS(INSIG)
         REAL HCP,HRKAPPA,HGASCON
         REAL HDEPTHS(INSIG)
         REAL DEPTHCR(INSIG),DEPTHCI(INSIG)
         REAL ECR(INSIG),ECI(INSIG)
         REAL G(INSIG,INSIG),TAU(INSIG,INSIG),PIVERT(INSIG)
         REAL B(INSIG,INSIG),E(INSIG,INSIG)
         REAL HE(INSIG,INSIG)
         REAL SIGHAT(INSIG)
         REAL HSIGHAT(INSIG)
         REAL DSIG(INSIG),TBAR(INSIG)
         REAL HDSIG(INSIG),HTBAR(INSIG)
         REAL HPR(INSIG)
         REAL WORK(INSIG,INSIG),WORK2(INSIG,INSIG)
         REAL EINVG(INSIG,INSIG)
         REAL HEINVG(INSIG,INSIG)
         REAL EINV(INSIG,INSIG),EINVT(INSIG)
         REAL HEINV(INSIG,INSIG),HEINVT(INSIG)
         REAL TBINVE(INSIG,INSIG),PIBINVE(INSIG)
         REAL HTBINVE(INSIG,INSIG),HPIBINVE(INSIG)
         REAL BINV(INSIG,INSIG)
         INTEGER LWORK(INSIG,2)
         REAL ALPHAR(INSIG)
         REAL ALPHAP(INSIG),BETAP(INSIG)
C--------
         N=INSIG
         NHAT=N+1
C--------
C--------START BY COMPUTING PIVERT
C--------
         DO 100 K=1,N
           PIVERT(K)=HDSIG(K)
           DSIG(K)=HDSIG(K)
           SIGHAT(K)=HSIGHAT(K)
           TBAR(K)=HTBAR(K)*HGASCON
100      CONTINUE
         RKAPPA=HRKAPPA
         GASCON=HGASCON
C--------
C--------NOW GET ALPHAR
C--------
         NM1=N-1
         DO 200 K=1,NM1
           ALPHAR(K)=.5*LOG(SIGHAT(K+1)/SIGHAT(K))
200      CONTINUE
         ALPHAR(N)=LOG(1./SIGHAT(N))
C--------
C-------- FOR PSEUDO-ARAKAWA HYDROSTATIC MATRIX NEED ALPHAP, BETAP.
C--------
         DO 210 K=1,NM1
           KR=NHAT-K
           KRP1=KR-1
           ALPHAP(K)=.5*(HPR(KRP1)/HPR(KR)-1.)/RKAPPA
           BETAP(K+1)=.5*(1.-HPR(KR)/HPR(KRP1))/RKAPPA
210      CONTINUE
         BETAP(1)=0.
         ALPHAP(N)=.5*(1./HPR(1)-2.)/RKAPPA
C--------
C--------SET UP HYDROSTATIC MATRIX G
C--------
C        DO 400 J=1,N
C          DO 300 I=1,N
C            IF(I.GT.J) G(I,J)=0.
C            IF(I.EQ.J) G(I,J)=ALPHAR(J)
C            IF(I.LT.J) G(I,J)=ALPHAR(J-1)+ALPHAR(J)
C300       CONTINUE
C400     CONTINUE
C--------
C--------SET UP HYDROSTATIC MATRIX G (PSEUDO-ARAKAWA)
C--------
         DO 400 J=1,N
           DO 300 I=1,N
             IF(I.GT.J) G(I,J)=0.
             IF(I.EQ.J) G(I,J)=ALPHAP(J)
             IF(I.LT.J) G(I,J)=ALPHAP(J-1)+BETAP(J)
300        CONTINUE
400      CONTINUE
C--------
C--------NOW OBTAIN TAU.
C--------
         DO 700 IR=1,N
           SUMR=0.
           DO 500 I=1,IR
             SUMR=SUMR+DSIG(I)
500        CONTINUE
           SUMRM=SUMR-DSIG(IR)
           IRM=IR-1
           IF(IR.EQ.1) IRM=1
           IRP=IR+1
           IF(IR.EQ.N) IRP=N
           DO 600 IS=1,N
             TERM1=DSIG(IS)*SUMR
             IF(IR.GE.IS) TERM1=TERM1-DSIG(IS)
             TERM2=DSIG(IS)*SUMRM
             IF(IR-1.GE.IS) TERM2=TERM2-DSIG(IS)
             TERM3=ALPHAR(IR)
             IF(IS.GT.IR) TERM3=0.
             IF(IS.LE.IR-1) TERM3=TERM3+ALPHAR(IR-1)
             TAU(IR,IS)=((TBAR(IRP)-TBAR(IR))*TERM1
     *                  +(TBAR(IR)-TBAR(IRM))*TERM2
     *        +2.*RKAPPA*TBAR(IR)*DSIG(IS)*TERM3)/(2.*DSIG(IR))
600        CONTINUE
700      CONTINUE
C--------
C--------GENERATE MATRIX B
C--------
         CALL MMULT(G,N,N,N,TAU,N,N,N,B,N,N,N)
         DO 1900 J=1,N
           DO 1800 K=1,N
             B(J,K)=B(J,K)+TBAR(J)*PIVERT(K)
1800       CONTINUE
1900     CONTINUE
C--------
C--------LAST STEP NOW IS TO COMPUTE EIGENVALUES, EIGENVECTORS
C--------
         IVECT=0
         CALL EGVLVE(B,DEPTHCR,DEPTHCI,ECR,ECI,N,IVECT,LWORK,
     *                 WORK,WORK2,IERROR)
         DO 1920 IVECT=1,N
           DEPTHC(IVECT)=CMPLX(DEPTHCR(IVECT),DEPTHCI(IVECT))
1920     CONTINUE
C--------REORDER EIGENVALUES FROM LARGEST TO SMALLEST
C--------
         DO 1980 I=1,N
           DO 1960 J=I,N
             IF  (CABS(DEPTHC(J)).LE.CABS(DEPTHC(I))) GO TO 1960
               CTEMP=DEPTHC(I)
               DEPTHC(I)=DEPTHC(J)
               DEPTHC(J)=CTEMP
1960       CONTINUE
1980     CONTINUE
         DO 1981 I=1,N
           DEPTHCR(I)=REAL(DEPTHC(I))
           DEPTHCI(I)=AIMAG(DEPTHC(I))
1981     CONTINUE
C--------
C--------COMPUTE ALL EIGENVECTORS, EVEN THOUGH ONLY NVERT ARE USED.
C--------
         DO 1983 IVECT=1,N
           CALL EGVLVE(B,DEPTHCR,DEPTHCI,ECR,ECI,N,IVECT,LWORK,
     *                   WORK,WORK2,IERROR)
           DO 1982 I=1,N
             EC(I,IVECT)=CMPLX(ECR(I),ECI(I))
1982       CONTINUE
1983     CONTINUE
         DO 2200 I=1,N
c        WRITE(6,2000)I, DEPTHC(I)
2000     FORMAT('  FOR MODE  ',I3,', REAL AND IMAGINARY PARTS OF ',
     *             'EIGENVALUE = ',2E12.4)
           DEPTHS(I)=DEPTHC(I)
           evecmax=0.
           ievecmax=1
           do 21002 k=1,n
             if(evecmax.lt.abs(ec(k,i))) then
               ievecmax=k
               evecmax=abs(ec(k,i))
             end if
21002      continue
           DO 2100 K=1,N
             E(K,I)=EC(K,I)/ec(ievecmax,i)
2100       CONTINUE
c          WRITE(6,2150)(E(K,I),K=1,N)
2150       FORMAT(1H ,10E13.3)
2200     CONTINUE
C--------
C--------NOW OBTAIN EINV, EINVG, EINVT, TBINVE, PIBINVE.
C--------
         DO 2400 J=1,N
           DO 2300 I=1,N
             EINV(I,J)=E(I,J)
2300       CONTINUE
2400     CONTINUE
         CALL MINV(EINV,N,DET,LWORK,LWORK(1,2))
c        CALL CHECKINV(E,EINV,N)
         DO 2500 J=1,N
           EINVT(J)=0.
2500     CONTINUE
         DO 2700 I=1,N
           DO 2600 J=1,N
             EINVT(J)=EINV(J,I)*TBAR(I)+EINVT(J)
             BINV(J,I)=B(J,I)
2600       CONTINUE
2700     CONTINUE
         CALL MINV(BINV,N,DET,LWORK,LWORK(1,2))
         CALL CHECKINV(B,BINV,N)
         CALL MMULT(BINV,N,N,N,E,N,N,N,WORK,N,N,N)
         CALL MMULT(TAU,N,N,N,WORK,N,N,N,TBINVE,N,N,N)
         DO 2900 J=1,N
           PIBINVE(J)=0.
           DO 2800 I=1,N
             PIBINVE(J)=PIVERT(I)*WORK(I,J)+PIBINVE(J)
2800       CONTINUE
2900     CONTINUE
         CALL MMULT(EINV,N,N,N,G,N,N,N,EINVG,N,N,N)
C-------- CONVERT TO UNITS OF METERS
         DO 1996 I=1,N
           HDEPTHS(I)=DEPTHS(I)/9.8
           HEINVT(I)=EINVT(I)
           HPIBINVE(I)=PIBINVE(I)
1996     CONTINUE
         DO 1998 I=1,N*N
           HEINV(I,1)=EINV(I,1)
           HEINVG(I,1)=EINVG(I,1)
           HTBINVE(I,1)=TBINVE(I,1)
           HE(I,1)=E(I,1)
1998     CONTINUE
       RETURN
       END
