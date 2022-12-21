      SUBROUTINE OBUDGET(FH, QT, LWDN, SWDN, TAIR, TD, PA, UG, TA, 
     1  OM, FLSE, FLLA)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1989
C  PURPOSE:
C     -CALCULATES GROWTH RATES OF NEW ICE IN THE ICE FREE PART OF A GRID
C       CELL WITH BULK FORMULAS
C  METHOD:
C     -HEAT BUDGET EQUATION FOR OPEN WATER
C  INTERFACE:
C     -FH: GROWTH RATE IN METERS OF ICE
C     -QT: SEA SURFACE=OML TEMPERATURE IN CELSIUS
C  EXTERNALS:
C     -VAPOR: CALCULATES VAPOR PRESSURE
C  LAST MODIFIED: 13 September 1994.
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      REAL TAIR(0:L,0:M), TD(0:L,0:M), PA(0:L,0:M), UG(0:L,0:M), 
     1  TA(0:L,0:M)

      REAL OM(0:L, 0:M)

      REAL FLSE(0:L, 0:M), FLLA(0:L, 0:M)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C=======================================================================
      REAL FH(0:L,0:M), QT(0:L,0:M), ESTA(LMDP), ESTW(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -QT:   SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW: SATURATION VAPOR PRESSURE OVER WATER
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1  FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), QT1(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL albedo
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      INTEGER I, J, K, N
      REAL D1, D2W, EA, Q1, Q2, Q3
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  SELECT THE GRID CELLS AND STORE THEM IN SELECTIVE 1-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 93 J=1,MM
      DO 93 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 93
       K=K+1
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J) * (1. - albedo(0., 0., 0., 0.) )
   93 CONTINUE
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      D1=RHOAIR*CPAIR*CSENS
      D2W=RHOAIR*VAPL*CLAT
C-----------------------------------------------------------------------
C  CALCULATE HEAT FLUXES AND GROWTH RATES
C-----------------------------------------------------------------------
      DO 25 N=1,K
       EA=TD1(N)*ESTA(N)
       FLSE1(N)=D1*UG1(N)*(TA1(N)-QT1(N))
C      Limit the latent heat flux so as to ignore situations which
C        assume that there is a large latent flux _in_ to the ocean
C        or ice.  Robert Grumbine 17 October 2002
CBG       FLLA1(N)=D2W*UG1(N)*(EA-ESTW(N))*EPSI/PA1(N)
       FLLA1(N) = D2W*UG1(N)*AMIN1(0.,(EA-ESTW(N)))*EPSI/PA1(N)
       Q1=D3*QT1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       FH1(N)=(Q1-Q2-Q3-FLSE1(N)-FLLA1(N))/CLB
   25 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 81 J=1,MM
      DO 81 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 81
       K=K+1
       FH(I,J)=FH1(K)
       TA(I,J)=TA1(K)
       FLSE(I,J)=FLSE1(K)
       FLLA(I,J)=FLLA1(K)
CBG    Diagnostic output when fluxes are extreme
       IF ( ABS(FH(I,J))*CLO .GT. 1000.) THEN !W/m2
         WRITE (*,9001) I, J, FH(I,J)*CLO, QT1(K), TA(I,J), -FLSE(I,J), 
     1     -FLLA(I,J), -SWDN(I,J), -LWDN(I,J), 
     2     D3*QT1(K)**4 - SWDN(I,J) - LWDN(I,J)
       ENDIF
   81 CONTINUE


CD 9001 FORMAT (2I4, 8F9.1, 
CD     1     ' Ocean, Net, TO, TA, FLSE, FLLA, SWD, LWD, Net rad ')
 9001 FORMAT ('Ocean ',I3,1x,I3,F8.1,2F6.1,1x,2F7.1,F8.1,F7.1,F8.1)

      RETURN
      END
