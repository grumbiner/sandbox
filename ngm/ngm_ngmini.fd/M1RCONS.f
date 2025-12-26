       SUBROUTINE M1RCONS(AP,BP,AQR,BQR,GR,JCAP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    M1RCONS    COMPUTE LEGENDRE GENERATOR CONSTANTS
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 90-09-21
C
C ABSTRACT: COMPUTE VARIOUS GENERATOR CONSTANTS NEEDED FOR
C   LEGENDRE FUNCTIONS USED BY SPECTRAL TRANSFORMS.
C
C PROGRAM HISTORY LOG:
C   90-09-21  PARRISH
C
C USAGE:    CALL M1RCONS(AP,BP,AQR,BQR,GR,JCAP)
C   INPUT ARGUMENT LIST:
C     JCAP     - TRIANGULAR TRUNCATION
C
C   OUTPUT ARGUMENT LIST:
C     AP       - VARIOUS
C     BP       -  REQUIRED
C     AQR      -   RECURSION
C     BQR      -    CONSTANTS
C     GR       -
C
C ATTRIBUTES:
C   LANGUAGE: CFT77
C   MACHINE:  CRAY YMP
C
C$$$
         implicit double precision (a-h,o-z)
         DIMENSION AP(0:JCAP,0:JCAP),BP(0:JCAP,0:JCAP)
         DIMENSION AQR(0:JCAP,0:JCAP),BQR(0:JCAP,0:JCAP)
         DIMENSION GR(0:JCAP,0:JCAP)
         real*4 conmc
C--------
         RERTH=CONMC('RERTH$')
         AP=0.d0
         BP=0.d0
         AQR=0.d0
         BQR=0.d0
         GR=0.d0
         DO 20 N=0,JCAP
           DO 10 L=0,N
             AP(N,L)=SQRT((2.d0*N+1.d0)*(2.d0*N+3.d0)/
     *                   ((N-L+1.d0)*(N+L+1.d0)))
             BP(N,L)=-SQRT((N-L)*(N+L)*(2.d0*N+3.d0)/
     *           ((N-L+1.d0)*(N+L+1.d0)*MAX(1.d0,2.d0*N-1.d0)))
             AQR(N,L)=AP(N,L)*N/(N+2.d0)
             BQR(N,L)=BP(N,L)*N*(N-1.d0)/((N+1.d0)*(N+2.d0))
             GR(N,L)=RERTH*AP(N,L)/((N+1.d0)*(N+2.d0))
10         CONTINUE
20       CONTINUE
       RETURN
       END
