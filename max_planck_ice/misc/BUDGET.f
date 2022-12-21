      SUBROUTINE BUDGET(FH,T,LRHS,KG, LWDN, SWDN, QS, IIC,
     1 TAIR, TD, PA, UG, TA, OM, FLSE, FLLA, HICE, ALB, A2, H, A, HSN)
C=======================================================================
C  PROGRAMMED BY:
C     A.STOESSEL               MPI, HAMBURG                         1989
C     R. Grumbine              NMC, Camp Springs                    1992
C  PURPOSE:
C     -CALCULATION OF GROWTH RATES FOR THE ICE COVERED PART OF A GRID
C       CELL WITH STANDARD BULK FORMULAS
C  METHOD:
C     -ICE OR SNOW SURFACE TEMPERATURES, RESPECTIVELY, ARE CALCULATED BY
C       ITERATION (REGULA FALSI)
C  INTERFACE:
C     -FH:   ICE GROWTH RATE
C     -T:    ICE OR SNOW SURFACE TEMPERATURE [CELSIUS]
C     -LRHS: RUNNING INDEX VALUE FOR OLD TIME STEP
C     -KG:   INDEX FOR ICE THICKNESS CATEGORIES
C     -LWDN: Downwelling Longwave Robert Grumbine 1992.
C     -SWDN: Downwelling Shortwave Robert Grumbine 1992.
C     -QS  : Sea water salinity (for tfreez) Robert Grumbine 13 Sep 1994.
C  EXTERNALS:
C     -VAPOR: CALCULATES VAPOR PRESSURE
C     -tfreez: Compute the freezing point of salt water
C  Last Modified 26 March 1998 
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      INTEGER LRHS 
C=======================================================================
      INTEGER KG

      INTEGER IIC

      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L, 0:M, 2)

      REAL TAIR(0:L,0:M), TD(0:L,0:M), PA(0:L,0:M), 
     1      UG(0:L,0:M), TA(0:L,0:M)

      REAL OM(0:L, 0:M)

      REAL FLSE(0:L,0:M), FLLA(0:L,0:M)

      REAL HICE(0:L,0:M), ALB(0:L,0:M), A2(0:L,0:M)
C=======================================================================
C     -HICE:  EFFECTIVE ICE THICKNESS (OPTIONALLY FOR SEVEN CATEGORIES)
C     -WRK:   DUMMY ARRAYS
C     -ALB:   ALBEDO
C     -TRK:   DUMMY ARRAY
C     -A2:    FLAG FOR SNOW CONDITIONS
C     -FLAI:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C=======================================================================
      REAL FH(0:L,0:M), T(0:L,0:M), ESTA(LMDP), ESTI(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI: SATURATION VAPOR PRESSURE OVER ICE
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1 FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), T1(LMDP),
     2 ALB1(LMDP), A21(LMDP), HICE1(LMDP), hsnow1(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL QS(0:L, 0:M), TB1(LMDP)
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), DIFF(LMDP),
     1  TT(LMDP), TT1(0:L,0:M), TMYUS1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
C  Declare remaining local variables
      REAL fhb, fhi, q1, q2, q3, albedo, tfreez
      REAL a1, flag, fdiff, ea, d1, d2i
      INTEGER n, iter, imax
      INTEGER i, j, k
      REAL limdel, delmax
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
      PARAMETER (IMAX = 10)
      PARAMETER (limdel = 0.001)
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
C     IF _either_ H or A is zero, skip the point (after resetting the other
C       to be consistent), BG 10/16/96.
      DO 79 J=1,MM
      DO 79 I=0,L
       TMYUS1(I,J)=OM(I,J)+2.
       IF (A(I,J,LRHS).EQ.0. .OR. HICE(I,J) .EQ. 0.) THEN
         TMYUS1(I,J)=2.
         A(I,J,LRHS) = 0.
         HICE(I,J) = 0.
         HSN(I,J,LRHS) = 0.
       ENDIF
   79 CONTINUE
C-----------------------------------------------------------------------
C  STORE EXTERNAL VARIABLES INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K = 0
      DO 82 J = 1,MM
      DO 82 I = 0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 82
       K = K+1
       HICE1(K) = HICE(I,J)
       hsnow1(K) = HSN(I,J, LRHS)
       ALB1(K) = ALB(I,J)
       A21(K) = A2(I,J)
       T1(K) = T(I,J)+TMELT
       TA1(K) = TAIR(I,J)+TMELT
       TD1(K) = MAX(.1,TD(I,J)/100.)
C      Added for tfreez = fn of salinity rather than constant.
       TB1(K) = tfreez(QS(I,J)) + TMELT
       PA1(K) = PA(I,J)
       UG1(K) = MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
C      Albedo computed in GROWTH.  Robert Grumbine 25 October 1994.
       SWDN1(K) = SWDN(I,J) * (1. - ALB1(K))
   82 CONTINUE
C     Note that HSNOW is apparently unused in this routine.  BG 10/25/94.
C     Deleted 9/17/96.  BG
      IF (K.EQ.0) GOTO 87
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(T1,ESTI,2,K)
      D1 = RHOAIR*CPAIR*CSENS
      D2I = RHOAIR*SUBL*CLAT
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 33 N = 1,K
       STP(N) = T1(N)
       EA = TD1(N)*ESTA(N)
       IF (PA1(N) .EQ. 0.0 .OR. HICE1(N) .EQ. 0.0) THEN
         PRINT *,'n, nmax, pa1, hice1 = ',N, K, PA1(N), HICE1(N)
         STOP 
       ENDIF
       FP(N) = D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1       -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2       EPSI/PA1(N)+(STP(N)-TB1(N) )/HICE1(N)*CON
       T1(N) = T1(N)+1.
       TT(N) = 0.
   33 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE SURFACE TEMPERATURE (START OF ITERATION PROCEDURE)
C-----------------------------------------------------------------------
      delmax = 0.
      DO 3 ITER = 1,IMAX
       CALL VAPOR(T1,ESTI,2,K)
       delmax = 0.
       DO 34 N = 1,K
        STPP(N) = STP(N)
        FPP(N) = FP(N)
        STP(N) = T1(N)
        EA = TD1(N)*ESTA(N)
        FP(N) = D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1        -D1*UG1(N)*(TA1(N)-STP(N))-D2I*UG1(N)*(EA-ESTI(N))*
     2        EPSI/PA1(N)+(STP(N)-TB1(N) )/HICE1(N)*CON
        FDIFF = FP(N)-FPP(N)
        T1(N) = STP(N)-(STP(N)-STPP(N))*FP(N)/
     1        MAX(ABS(FDIFF), 1.E-6)*SIGN(1.,FDIFF)
        DIFF(N) = T1(N)-STP(N)
        TT(N) = SIGN(1.,.01-ABS(DIFF(N)))
        delmax = MAX(delmax, ABS(DIFF(N)) )
   34  CONTINUE
       IF (delmax .LT. limdel ) GO TO 9999
    3 CONTINUE
C
C  IF TEMPERATURE ITERATION FAILED TO CONVERGE, RESET T1 TO TMELT.
C  Robert Grumbine 21 February 1997: If T1 is negative (T1 is in K!)
C    reset T1 to tmelt and warn
c
      PRINT *,'Convergence failed (in budget), delta = ',delmax
      DO 35 N = 1, K
        IF (TT(N) .LE. 0.) T1(N) = TMELT
        IF (T1(N) .LE. 0.) THEN
            T1(N) = TMELT
            PRINT *,'Budget - iterated to a negative K temperature'
        ENDIF
 35   CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED HEAT BALANCE EQUATION
C-----------------------------------------------------------------------
 9999 CONTINUE

      DO 83 N = 1,K
       FLAG = .5*(1.+SIGN(1.,T1(N)-TMELT))
       T1(N) = T1(N)*(1.-FLAG)+TMELT*FLAG
       EA = TD1(N)*ESTA(N)
       A1 = 0.5*(1.+SIGN(1.,T1(N)-TMELT))
       FLSE1(N) = D1*UG1(N)*(TA1(N)-T1(N))
C      Limit the latent heat flux so as to ignore situations which
C        assume that there is a large latent flux _in_ to the ocean
C        or ice.  Robert Grumbine 17 October 2002
       FLLA1(N) = D2I*UG1(N)*AMIN1(0.,(EA-ESTI(N)))*EPSI/PA1(N)
       Q1 = D3*T1(N)**4
       Q2 = SWDN1(N) 
       Q3 = LWDN1(N)
       FHI = A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB1(N) -T1(N))/HICE1(N)
     1                 *CON) /CLO
       FHB = ((TB1(N) - T1(N))/HICE1(N)*CON)/CLB
       FH1(N) = FHI+FHB
   83 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K = 0
      DO 84 J = 1,MM
      DO 84 I = 0,L
       IF (OM(I,J).EQ.0.) GOTO 84
       IF (A(I,J,LRHS).EQ.0.) GOTO 84
       K = K+1
       FH(I,J) = FH1(K)
       T(I,J) = T1(K)-TMELT
       TT1(I,J) = TT(K)
       IF (TT1(I,J).LE.0.) WRITE(16,701) IIC
       IF (KG.NE. INT(0.5+(NLEVEL+1)/2) ) GOTO 84
       TA(I,J) = TA1(K)
       FLSE(I,J) = FLSE1(K)
       FLLA(I,J) = FLLA1(K)
       IF ( FH(I,J) *CLO .GT. 350.*2.0 .OR. 
     1      FH(I,J) *CLO .LT. -350.*2.0      ) THEN
         WRITE (*,9001) I, J, FH(I,J)*CLO, T1(K), TA(I,J), -FLSE(I,J), 
     1     -FLLA(I,J), SWDN1(K), LWDN(I,J),
     1     D3*T1(K)**4 - SWDN1(K) -LWDN(I,J)
       ENDIF
   84 CONTINUE

   87 CONTINUE

CD 9001 FORMAT ( 2I4, 8F9.1, 
CD     1         ' Ice, Net, Ti, Ta, FLSE, FLLA, SWD, LWD, Net rad')
 9001 FORMAT ('  Ice ',I3,1X,I3,F8.1,2F6.1,1x,5F7.1)

  701 FORMAT (1X,I4,'Budget iteration exceeded')

      RETURN
      END
