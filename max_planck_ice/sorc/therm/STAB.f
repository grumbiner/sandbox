      SUBROUTINE STAB(OL,ZO,K, PH1, PH2)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF THE STABILITY FUNCTIONS PH1 AND PH2
C  METHOD:
C     -THE CALCULATIONS DIFFER ACCORDING TO THE SIGN OF THE MONIN-
C       OBUKHOV LENGTH (KOCH, 1986: 91-92)
C     -IN THIS VERSION ZA IS ASSIGNED FOR THE 10 M WIND
C  INTERFACE:
C     -OL: RECIPROCAL OF THE MONIN-OBUKHOV LENGTH TIMES 2 (LSTAR=2/OL)
C     -ZO: ROUGHNESS LENGTH
C     -K:  MAXIMUM NUMBER OF GRID POINTS TO BE TREATED
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL PH1(LMDP), PH2(LMDP)
      REAL OL(LMDP)
      REAL ZO, R, CRI, AS, BS, CS, DS, FLAG, ZLOG, ZOL
      INTEGER K, N
C=======================================================================
      R=SQRT(2.)
      CRI=-1./16.
C-----------------------------------------------------------------------
C  FIRST DETERMINE THE STABILITY FUNCTION FOR THE HEAT FLUXES
C-----------------------------------------------------------------------
      DO 2 N=1,K
       ZOL=OL(N)*ZO/2.
       IF (ZOL.GT.-(1.E-6)) GOTO 1
       CS=(1.-16.*OL(N))**0.5
       DS=(ABS(1.+16.*ZOL))**0.5
       IF (ZOL.GT.CRI+1.E-6) GOTO 10
       IF (ZOL.LT.CRI-1.E-6) GOTO 11
       PH1(N)=2.*(1.+1./CS)
       GOTO 2
C  FOR UNSTABLE STRATIFICATION:
   10  CONTINUE
       PH1(N)=LOG((CS-DS)*(1.+DS)/(CS+DS)/(1.-DS))/DS
       GOTO 2
   11  CONTINUE
       PH1(N)=2./DS*(ATAN(CS/DS)-ATAN(1./DS))
       GOTO 2
C  FOR NEUTRAL AND STABLE STRATIFICATION:
    1  CONTINUE
       ZLOG=LOG((2.+ZO)/ZO)
       PH1(N)=ZLOG+5.*(OL(N)+ZOL/ZLOG)
    2 CONTINUE
C-----------------------------------------------------------------------
C  NEXT DETERMINE THE STABILITY FUNCTION FOR THE MOMENTUM FLUX
C-----------------------------------------------------------------------
C     FLAG was unitialized as received.  
C     Set equal to zero by BG. 4/22/92.
      FLAG = 0.0
      DO 3 N=1,K
       OL(N)=5.*OL(N)
       ZOL=OL(N)*ZO/10.
       IF (ZOL.GT.-(1.E-6)) GOTO 4
       AS=(1.-16.*OL(N))**0.25
       BS=(ABS(1.+16.*ZOL))**0.25
       IF (ZOL.GT.CRI+1.E-6) GOTO 20
       IF (ZOL.LT.CRI-1.E-6) GOTO 21
       PH2(N)=4.*(1.-1./AS)
       GOTO 3
C  FOR UNSTABLE STRATIFICATION:
   20  CONTINUE
       PH2(N)=2./BS*(-0.5*LOG((BS+AS)*(BS-1.)/(BS-AS)/(1.+BS))
     1        +ATAN(AS/BS)-ATAN(1./BS))
       GOTO 3
   21  CONTINUE
       PH2(N)=(1.-FLAG)*(LOG(SQRT(AS**4+BS**4)*(1.+R*BS+BS*BS)
     1        /SQRT(1.+BS**4)/(AS*AS+R*AS*BS+BS*BS))
     2        +ATAN(AS*BS*R/(BS*BS-AS*AS))-ATAN(BS*R/(BS*BS-1.)))/BS/R
       GOTO 3
C  FOR NEUTRAL AND STABLE STRATIFICATION:
    4  CONTINUE
       ZLOG=LOG((10.+ZO)/ZO)
       PH2(N)=ZLOG+5.*(OL(N)+ZOL/ZLOG)
    3 CONTINUE
      RETURN
      END
