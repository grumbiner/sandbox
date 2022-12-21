      SUBROUTINE RESIST(X,K, AF, BF, CF)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -DETERMINATION OF STABILITY FUNCTIONS A,B,AND C (AF,BF,CF)
C  METHOD:
C     -ROSSBY NUMBER SIMILARITY THEORY (KOCH, 1986: 93)
C  INTERFACE:
C     -X: STABILITY PARAMETER (WMUE)
C     -K: MAXIMUM NUMBER OF GRID POINTS TO BE TREATED
C  LAST MODIFIED: 5 January 1993.
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL AF(LMDP), BF(LMDP), CF(LMDP)
      REAL X(LMDP)
      INTEGER K
      INTEGER N
      REAL FLAG
C=======================================================================
      DO 1 N=1,K
       FLAG=.5*(1.+SIGN(1.,X(N)+50.))
       AF(N)=4.5*(1.-FLAG)+(-.00144*X(N)**2-.144*X(N)+.9)*FLAG
       FLAG=.5*(1.+SIGN(1.,X(N)+75.))
       BF(N)=1.0*(1.-FLAG)+(.00062*X(N)**2+.093*X(N)+4.4875)*FLAG
       FLAG=.5*(1.+SIGN(1.,X(N)+100.))
       CF(N)=7.5*(1.-FLAG)+(-.00065*X(N)**2-.13*X(N)+1.)*FLAG
    1 CONTINUE
      RETURN
      END
