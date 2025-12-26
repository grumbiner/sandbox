       SUBROUTINE MMULT(A,MA,NA,IA,B,MB,NB,IB,C,MC,NC,IC)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MMULT       MATRIX MULTIPLY
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-09-08
C
C ABSTRACT: MATRIX MULTIPLY.  MUST HAVE NA=MB, MA=MC, NB=NC.
C
C PROGRAM HISTORY LOG:
C   88-09-08  PARRISH
C
C USAGE:    CALL MMULT(A,MA,NA,IA,B,MB,NB,IB,C,MC,NC,IC)
C   INPUT ARGUMENT LIST:
C     A        - MATRIX, A(MA,NA)
C     MA       - DIMENSIONS OF
C     NA       - MATRIX A.
C     IA       - FIRST DIMENSION IF A IN CALLING PROGRAM
C     B        - MATRIX, B(MB,NB)
C     MB       - DIMENSIONS OF
C     NB       - MATRIX B.
C     IB       - FIRST DIMENSION IF B IN CALLING PROGRAM
C
C   OUTPUT ARGUMENT LIST:
C     C        - MATRIX, C(MC,NC) (  = A*B  )
C     MC       - DIMENSIONS OF
C     NC       - MATRIX C.
C     IC       - FIRST DIMENSION IF C IN CALLING PROGRAM
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
         REAL A(IA,NA),B(IB,NB),C(IC,NC)
C--------
C--------TEST INPUT PARAMETERS
C--------
         IF(NA.NE.MB.OR.MA.NE.MC.OR.NB.NE.NC) GO TO 1000
C--------
C--------DO MULTIPLY
C--------
         DO 300 J=1,NC
           DO 200 I=1,MC
             SUM=0.
             DO 100 K=1,NA
               SUM=SUM+A(I,K)*B(K,J)
100          CONTINUE
             C(I,J)=SUM
200        CONTINUE
300      CONTINUE
       RETURN
C--------
C--------HERE FOR TROUBLE
C--------
1000     CONTINUE
         WRITE(6,1100)MA,NA,MB,NB,MC,NC
1100     FORMAT(' DIMENSIONS DON*T MATCH IN MMULT, MA,NA=',2I4,
     *             '  MB,NB=',2I4,'  MC,NC=',2I4,/,
     *             '  MUST HAVE NA=MB, MA=MC, AND NB=NC')
C      STOP
       RETURN
       END
