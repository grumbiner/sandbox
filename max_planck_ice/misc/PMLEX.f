      SUBROUTINE PMLEX(QHST,SNOW,QFM,QPR,QTM, bathy,
     1  OM, DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, QV,
     3  QRHO, QW, IEN, FW, MLFIX)
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C     -R. Grumbine            NMC, Camp Springs                   Jul 95
C  PURPOSE:
C     -PROGNOSTIC CALCULATION OF OCEANIC MIXED LAYER VARIABLES (OML-
C       MODEL) AND VERTICAL OCEANIC HEAT FLUX
C  METHOD:
C     -HEAT AND SALT BUDGET OF WATER COLUMN
C     -KRAUS-TURNER TYPE PARAMETERIZATION FOR ENTRAINMENT HEAT FLUX WITH
C       EXPONENTIALLY SHAPED PYCNOCLINE
C     -ENERGY BALANCE FOR CLOSURE
C     -Limit mixed layer depth to the bathymetry.  BG.
C  INTERFACE:
C     -QHST: CHANGE IN ICE THICKNESS OR HEAT STORAGE (IF NEGATIVE)
C     -SNOW: CURRENT SNOW DEPTH
C     -QFM:  AMOUNT OF MELTED ICE [M] OR SURFACE SALT FLUX
C     -QPR:  FRESH WATER INPUT (MELTED SNOW OR ICE + RAIN)
C     -QTM:  VERTICAL OCEANIC (ENTRAINMENT) HEAT FLUX
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMIN:  THE THIRD ARGUMENT IS MINIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMAXC: SAME AS VECMAX, THE SECOND ARGUMENT BEING AN ARRAY
C     -VECMINC: SAME AS VECMIN, THE SECOND ARGUMENT BEING AN ARRAY
C     -VERDIF:  DETERMINES VERTICAL DIFFUSION
C     -ADJUEX:  ADJUSTS OML VARIABLES
C  Last Modified 10 March 1999
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "oml.inc"
      INCLUDE "physical.inc"
C=======================================================================
      REAL DELTAD, SSMIN, TTMIN, HSTC
      PARAMETER (DELTAD=8.0)
      PARAMETER (SSMIN=0.01)
      PARAMETER (TTMIN=0.8)
      PARAMETER (HSTC=120.0)
C=======================================================================
      REAL bathy(0:L, 0:M)

      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M), QHSTO(0:L,0:M)
      REAL HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

      REAL OM(0:L, 0:M)
      REAL DCVM, WUP, COSGAM, RTC, STC, QTOC

C=======================================================================
      REAL TMP(0:L,0:M), TMP2(0:L,0:M), TMP3(0:L,0:M), TMP4(0:L,0:M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C     -TMP4: TEMPORARY ARRAY
C=======================================================================
      REAL DCV(0:L,0:M), QWT(0:L,0:M), RET(0:L,0:M), ENT(0:L,0:M),
     1HOLD(0:L,0:M), QSS(0:L,0:M), FLAGI(0:L,0:M)
C=======================================================================
C     -DCV:   DISSIPATION OF CONVECTIVE ENERGY
C     -QWT:   KINETIC ENERGY * DISSIPATION OF MECHANICAL ENERGY
C     -RET:   AMOUNT OF MIXED LAYER RETREAT
C     -ENT:   ENTRAINMENT VELOCITY
C     -HOLD:  OLD MIXED LAYER DEPTH
C     -QSS:   ADJUSTED MIXED LAYER SALINITY DUE TO ADVECTIVE EFFECTS
C     -FLAGI: FLAG FIELD FOR PRESENCE OF ICE AND/OR SNOW
C=======================================================================
      REAL QHST(0:L,0:M), SNOW(0:L,0:M), QFM(0:L,0:M), QPR(0:L,0:M),
     1QTM(0:L,0:M)
C=======================================================================
C      Local declarations
      INTEGER I, J
      REAL flag, flag1, flag2, flagto, flags, flagtn, flagt
      REAL a1, fnot, f1, ss, tt, ads, adt, sss, ttt, deta, hv
      REAL dh1, shb, thb, stab, sstar, tfreez
      INTEGER iter
C-----------------------------------------------------------------------
C  ARTIFICIAL RESTRICTIONS FOR OML SIMULATION -- MOVED TO PARAMETERS
C-----------------------------------------------------------------------
C  DETERMINE INITIAL CONDITIONS AND UPDATE HEAT AND SALT CONTENT
C-----------------------------------------------------------------------

      DO 10 J=1,MM
      DO 10 I=0,L
       QFM(I,J)=-QFM(I,J)*(QS(I,J)-SICE)-QPR(I,J)*QS(I,J)
       QFM(I,J)=QFM(I,J)*OM(I,J)
       QTM(I,J)= QTM(I,J)*CLO/CC*OM(I,J)
       QRHO(I,J) = (BETAS*QFM(I,J)-BETAT*QTM(I,J) 
     1              + gammat*QTM(I,J)**2)/DT
       QW(I,J)=QV(I,J)*CW*COSGAM
       QWT(I,J)=QW(I,J)*EXP(-QH(I,J)/QHW)*OM(I,J)
       QHSTO(I,J)=QHST(I,J)
       HOLD(I,J)=QH(I,J)
       TMP(I,J)=EXP(-QH(I,J)/QHS)
       TMP4(I,J)=SNOW(I,J)
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
C  PARAMETERIZE ADVECTIVE EFFECTS:
       QFM(I,J)=QFM(I,J)-(QS(I,J)-33.8)*HSTC*OM(I,J)/STC
       HS(I,J)=HS(I,J)+(QFM(I,J)+WUP*DT*(QSB(I,J)-QS(I,J)))*OM(I,J)
       HT(I,J)=HT(I,J)+(QTM(I,J)+WUP*DT*(QTB(I,J)-QT(I,J)))*OM(I,J)
   10 CONTINUE



      IF (MLFIX.NE.1) GOTO 30
C-----------------------------------------------------------------------
C  THE NEXT LOOP ENTERS ONLY FOR FIXED MIXED LAYER DEPTH
C-----------------------------------------------------------------------
C     Following added by Robert Grumbine to avoid infinitely thin
C       mixed layers.  10 October 1995
      CALL VECMAX (QH, 1.0, QH)
      DO 20 J=1,MM
      DO 20 I=0,L
       IEN(I,J)=1
       HT(I,J)=HT(I,J)+QTOC/CC*DT*OM(I,J)
       QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/QH(I,J)+QTB(I,J)
       QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/QH(I,J)+QSB(I,J)
       QTM(I,J)=QTOC
   20 CONTINUE

      GOTO 200
C-----------------------------------------------------------------------
C  DETERMINE SIGN OF ENTRAINMENT
C-----------------------------------------------------------------------
   30 CONTINUE
      CALL VECMAX(TMP,DCVM,DCV)
      DO 40 J=1,MM
      DO 40 I=0,L
       FLAG=(1.+SIGN(1.,QRHO(I,J)))/2.
       DCV(I,J)=1.-FLAG +DCV(I,J)*FLAG
       ENT(I,J)=2.*QWT(I,J)+GRAV*QRHO(I,J)*DCV(I,J)*QH(I,J)
       IEN(I,J)=INT((1.+SIGN(1.,ENT(I,J)))/2.0+0.2)
       TMP(I,J)=QH(I,J)*0.7
   40 CONTINUE
C-----------------------------------------------------------------------
C  DETERMINE MIXED LAYER RETREAT USING THE MONIN-OBUKHOV LENGTH
C-----------------------------------------------------------------------
      CALL VECMIN(TMP,100.0,TMP)
      DO 50 ITER=1,5
      DO 50 J=1,MM
      DO 50 I=0,L
       FLAG=1.0-FLOAT(IEN(I,J))
       A1=EXP(-TMP(I,J)/QHW)
       FNOT=2.*QW(I,J)*A1+GRAV*QRHO(I,J)*TMP(I,J)
       F1=-2.*QW(I,J)*A1/QHW+GRAV*QRHO(I,J)
       TMP(I,J)=TMP(I,J)-FNOT*FLAG/(F1*FLAG+(1.-FLAG))
   50 CONTINUE
      CALL VECMAX(TMP,10.,TMP2)
      CALL VECMINC(TMP2,QH,TMP3)
      DO 70 J=1,MM
      DO 70 I=0,L
       RET(I,J)=(TMP3(I,J)-QH(I,J))*(1.-FLOAT(IEN(I,J)))/RTC
   70 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE ENTRAINMENT VELOCITY
C-----------------------------------------------------------------------
      DO 100 J=1,MM
      DO 100 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
       IF (SS .EQ. 0.) SS = 1.E-3
       IF (TT .EQ. 0.) TT = 1.E-3

       ADS=1.0+(EXP(-DELTAD/QDS(I,J))-1.0)*QDS(I,J)/DELTAD
       ADT=1.0+(EXP(-DELTAD/QDT(I,J))-1.0)*QDT(I,J)/DELTAD
       SSS=SS*ADS
       TTT=TT*ADT
       DETA = GRAV*QH(I,J)*(BETAS*SSS-BETAT*TTT+GAMMAT*TTT*TTT)
       TMP(I,J) = DETA-GRAV*QH(I,J)*TTT*DCV(I,J)
     1   *(BETAT-gammat*TTT
     1          -CC/CLO*BETAS*RHOICE/RHOWAT*(QS(I,J)-SICE))*FLAGI(I,J)
  100 CONTINUE
      CALL VECMAX(TMP,EPSAA,TMP2)
      CALL VECMAX(ENT,0.,TMP)
      DO 110 J=1,MM
      DO 110 I=0,L
       ENT(I,J)=TMP(I,J)/TMP2(I,J)*DT*FLOAT(IEN(I,J))
  110 CONTINUE
      CALL VECMIN(ENT,ENTMAX,TMP)
C-----------------------------------------------------------------------
C  DETERMINE NEW MIXED LAYER DEPTH
C-----------------------------------------------------------------------
      DO 120 J=1,MM
      DO 120 I=0,L
       TMP2(I,J)=QH(I,J)-(WUP*DT-TMP(I,J)*FLOAT(IEN(I,J))-
     1           RET(I,J)*(1.0-FLOAT(IEN(I,J))))*OM(I,J)
       TMP3(I,J)= bathy(i,j)
CD Note that the following used to be inside the loop.  That
CD   hardly seems a good idea since QH is being trimmed to fit
CD   physical constraints.  Assign after bounds have been
CD   placed.  BG 10 October 1995
CD       TMP(I,J)=QH(I,J)
  120 CONTINUE
      CALL VECMINC(TMP2,TMP3,QH)
C     Following added by Robert Grumbine to avoid infinitely thin
C       mixed layers.  10 October 1995
      CALL VECMAX (QH, 1.0, QH)
      DO 121 J = 1, MM
      DO 121 I = 0, L
        TMP(i,j) = QH(i,j)
  121 CONTINUE
 
C-----------------------------------------------------------------------
C  DETERMINE MIXED LAYER AND PYCNOCLINE VARIABLES
C-----------------------------------------------------------------------
      DO 140 J=1,MM
      DO 140 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
CBG    Following added for robustness
       IF (SS .EQ. 0.) SS = 1.E-3
       IF (TT .EQ. 0.) TT = 1.E-3
       IF ( ABS(QDS(I,J)) .LT. 1.E-2) THEN
         PRINT *,'qds ',i,j,qds(i,j)
         QDS(I,J) = SIGN(1.E-2, QDS(I,J) )
       ENDIF
       IF ( ABS(QDT(I,J)) .LT. 1.E-2) THEN
         PRINT *,'qdt ',i,j,qdt(I,J)
         QDT(I,J) = SIGN(1.E-2, QDT(I,J) )
       ENDIF

       FLAGTO=(1.+SIGN(1.,ABS(TT)-TTMIN))/2.*SIGN(1.,-TT)
       ADS=1.0+(EXP(-DELTAD/QDS(I,J))-1.0)*QDS(I,J)/DELTAD
       ADT=1.0+(EXP(-DELTAD/QDT(I,J))-1.0)*QDT(I,J)/DELTAD
       SSS=SS*ADS
       TTT=TT*ADT
       HV=TMP(I,J)
       ENT(I,J)=(QH(I,J)-HV+WUP*DT)*FLOAT(IEN(I,J))
       DH1=QH(I,J)-HV
       HV=2.0*QH(I,J)*HV/(QH(I,J)+HV)
       IF (HV .EQ. 0) STOP "HV = 0"

       SHB=QSB(I,J)*QHB(I,J)
       THB=QTB(I,J)*QHB(I,J)
       QS(I,J)=QS(I,J)+(QFM(I,J)/HV+SSS*ENT(I,J)/HV)*OM(I,J)
       SS=QSB(I,J)-QS(I,J)
       FLAGS=(1.+SIGN(1.,ABS(SS)-SSMIN))/2.
       SS=SS*FLAGS+1.0-FLAGS
       IF (SS .EQ. 0) STOP "SS = 0"
       QDS(I,J)=QDS(I,J)-(QDS(I,J)-(SHB-HS(I,J))/SS+QH(I,J))*FLAGS
     1          *OM(I,J)
       QDS(I,J)=QDS(I,J)-DH1*(1.0-FLAGS)*OM(I,J)
C      BG addition for robustness
       IF (ABS(QH(I,J) + QDS(I,J)) .LT. 1.E-3) THEN
         PRINT *,'resetting QDS to avoid division by zero'
         QDS(I,J) = QH(I,J) - 1.0
       ENDIF
       QS(I,J)=QS(I,J)-(QS(I,J)-(HS(I,J)-SHB)/(QH(I,J)+QDS(I,J))-
     1         QSB(I,J))*(1.0-FLAGS)*OM(I,J)
       QT(I,J)=QT(I,J)+(QTM(I,J)/HV+TTT*ENT(I,J)/HV)*OM(I,J)
       TT=QTB(I,J)-QT(I,J)
       FLAGTN=(1.+SIGN(1.,ABS(TT)-TTMIN))/2.*SIGN(1.,-TT)
       FLAGT=FLAGTO*FLAGTN*(1.+FLAGTO*FLAGTN)/2.
       TT=TT*FLAGT+1.0-FLAGT
       IF (TT .EQ. 0) STOP "TT = 0"
       QDT(I,J)=QDT(I,J)-(QDT(I,J)-(THB-HT(I,J))/TT+QH(I,J))*FLAGT
     1          *OM(I,J)
       QDT(I,J)=QDT(I,J)-DH1*(1.0-FLAGT)*OM(I,J)
C      BG addition for robustness
       IF (ABS(QH(I,J) + QDT(I,J)) .LT. 1.E-3) THEN
         PRINT *,'resetting QDT to avoid division by zero'
         QDT(I,J) = QH(I,J) - 1.0
       ENDIF
       QT(I,J)=QT(I,J)-(QT(I,J)-(HT(I,J)-THB)/(QH(I,J)+QDT(I,J))-
     1         QTB(I,J))*(1.0-FLAGT)*OM(I,J)
       QTM(I,J)=TTT*ENT(I,J)*FLAGT*OM(I,J)
  140 CONTINUE

C-----------------------------------------------------------------------
C  SIMULATE VERTICAL DIFFUSION
C-----------------------------------------------------------------------
      CALL VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)

C-----------------------------------------------------------------------
C  ADJUST MIXED LAYER VARIABLES AFTER THE PREVIOUS MODIFICATIONS
C-----------------------------------------------------------------------
  200 CONTINUE
      CALL ADJUEX(QHST,SNOW,FLAGI, OM, TMP, TMP2, TMP3,
     1  QS, QSB, QT, QTB, QH, QHB, QDS, QDT, HS, HT, MLFIX)
C-----------------------------------------------------------------------
C  STABILITY ADJUSTMENT (CONVECTION)
C-----------------------------------------------------------------------
      IF (MLFIX.EQ.1) GO TO 300
      DO 270 J=1,MM
      DO 270 I=0,L
       STAB = BETAS*(QSB(I,J)-QS(I,J))-BETAT*(QTB(I,J)-QT(I,J))
     1       +GAMMAT*(QTB(I,J)**2 - QT(I,J)**2)
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
       FLAGS=(1.0-SIGN(1.,STAB-EPSAA))/2.
       TMP(I,J)=QH(I,J)
       TMP3(I,J)= bathy(i,j) +10.0
C  PARTIAL OVERTURNING IN ICE COVERED OCEAN:
       SSTAR = QSB(I,J)-BETAT/BETAS*(QTB(I,J)-QT(I,J))-0.01
     1       + GAMMAT*(QTB(I,J)**2 - QT(I,J)**2) /BETAS
       QH(I,J) = QH(I,J)+(QS(I,J)-SSTAR)*(QH(I,J)+QDS(I,J)-QHSTO(I,J))
     1         / AMAX1(0.001, (QTB(I,J)-tfreez(QS(I,J)) ) ) 
     2         / (CC/CLO*(SSTAR-SICE)-BETAT/BETAS)
     2         * FLAGI(I,J)*OM(I,J)*FLAGS
C  OVERTURNING IN OPEN OCEAN:
       QH(I,J)=QH(I,J)+(QH(I,J)-TMP3(I,J))*(1.-FLAGI(I,J))*FLAGS*OM(I,J)
       QS(I,J)=QS(I,J)-(QS(I,J)-SSTAR)*(1.-FLAGI(I,J))*FLAGS*OM(I,J)
       HS(I,J)=HS(I,J)-(HS(I,J)-(SSTAR-QSB(I,J))*(QH(I,J)+QDS(I,J))
     1         -QSB(I,J)*QHB(I,J))*(1.0-FLAGI(I,J))*FLAGS*OM(I,J)
  270 CONTINUE

      CALL VECMAXC(QH,TMP,TMP2)
      CALL VECMINC(TMP2,TMP3,QH)

      CALL VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)
      DO 280 J=1,MM
      DO 280 I=0,L
C      'then' clause added by Robert Grumbine 9 July 1995
C      This handles the case where the mixed layer is bottoming out.
       IF (QH(I,J) -  bathy(i,j)  .GT. -0.1 ) THEN
         TMP2(i,j) = QT(i,j)
         QT(I,J)  = HT(i,j) /  bathy(i,j)  - 1.0 
         QH(I,J) = 2.5
         QS(I,J)  = HS(i,j) /  bathy(i,j) * 0.99 
         QTB(I,J) = HT(i,j) /  bathy(i,j) 
         QSB(I,J) = HS(i,j) /  bathy(i,j) 
         IF (QT(i,j) .LT. tfreez(QS(i,j)) ) THEN
           QT(i,j)  = tfreez(QS(I,j))
           HT(i,j)  = QT(I,j)* QH(i,j) 
         ENDIF 
         IF (QTB(i,j) .LT. tfreez(QSB(i,j)) ) THEN
           QTB(i,j) = tfreez(QSB(I,j))
         ENDIF 
        ELSE       
         TMP2(I,J)=QT(I,J)
         QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/(QH(I,J)+QDT(I,J))+QTB(I,J)
         QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/(QH(I,J)+QDS(I,J))+QSB(I,J)
       ENDIF
       QTM(I,J)=QTM(I,J)+(QT(I,J)-TMP2(I,J))*QH(I,J)
       QTM(I,J)=QTM(I,J)*CC/DT*OM(I,J)
  280 CONTINUE

      CALL ADJUEX(QHST,SNOW,FLAGI, OM, TMP, TMP2, TMP3,
     1  QS, QSB, QT, QTB, QH, QHB, QDS, QDT, HS, HT, MLFIX)

C-----------------------------------------------------------------------
C  FINAL OUTPUT FOR BOTH FIXED AND VARIABLE MIXED LAYER
C-----------------------------------------------------------------------
  300 CONTINUE 
      CALL VECMAX(QHSTO,0.,TMP)
      CALL VECMAX(QHST,0.,TMP2)
      DO 310 J=1,MM
      DO 310 I=0,L
       FLAG1=(1.-SIGN(1.,-QHST(I,J)))/2.
       FLAG2=(1.-SIGN(1.,-SNOW(I,J)))/2.
       FLAGI(I,J)=FLAG1+FLAG2-FLAG1*FLAG2
       FLAGI(I,J)=1.0-FLAGI(I,J)
       QHST(I,J)=QHST(I,J)-(QHST(I,J)+(QT(I,J)-tfreez(QS(I,J)) )
     1           *QH(I,J)*CC/CLO)* FLAGI(I,J)
C  ON OUTPUT: QTM IS ENTRAINMENT HEAT FLUX IN [W/M**2]
       QTM(I,J)=QTM(I,J)*(1.-FLAGI(I,J))*FLOAT(IEN(I,J))
C  ON OUTPUT: QFM = MELTING RATE DUE TO OCEANIC HEAT FLUX IN [M]
       QFM(I,J)=(TMP(I,J)-TMP2(I,J))*OM(I,J)*RHOICE/RHOWAT
       IEN(I,J)=INT(1.0-FLAGI(I,J)+0.3)
  310 CONTINUE
C  PARAMETERIZE ADVECTIVE EFFECTS:
      CALL VECMINC(QDS, bathy ,QDS)
      CALL VECMINC(QDT, bathy ,QDT)
      CALL VECMINC(QH, bathy ,QH)
C  Note: Still apply minimum of HMLREF on ocean grounds, rather than
C    bathymetry grounds.  Definitely need a column ocean rather than
C    present slab ocean.  Robert Grumbine 16 September 1995.
C  And now require that QDS, QDT be at least 1 meter 10 October 1995
      CALL VECMAX(QDT, 1.0, QDT)
      CALL VECMAX(QDS, 1.0, QDS)

      DO 401 J=1,MM
      DO 401 I=0,L
        QSS(I,J)=QSB(I,J)-(BETAT/BETAS)*(QTB(I,J)-QT(I,J))-.01
     1       + GAMMAT*(QTB(I,J)**2 - QT(I,J)**2) /BETAS
  401 CONTINUE
      CALL VECMINC(QS,QSS,QS)
      DO 400 J=1,MM
      DO 400 I=0,L
       HS(I,J)=(QS(I,J)-QSB(I,J))*(QH(I,J)+QDS(I,J))+QSB(I,J)*QHB(I,J)
       HT(I,J)=(QT(I,J)-QTB(I,J))*(QH(I,J)+QDT(I,J))+QTB(I,J)*QHB(I,J)
  400 CONTINUE

CBG   Clean up after singular variables which seem common in this
CBG     method
      DO 500 J = 1, MM
      DO 500 I = 0, L
        IF (ABS(QHST(I,J)) .GT. 350.  ) THEN
          PRINT *,'qhst, qfm ',i,j,QHST(I,j), QFM(I,J)
          QHST(I,J) = 0.0
        ENDIF
        IF (ABS(QT(I,J)) .GT. 350.  ) THEN
          PRINT *,'QT ',i,j,QT(I,j)
          QT(I,J) = 0.0
        ENDIF
        IF (ABS(QS(I,J)) .GT. 350.  ) THEN
          PRINT *,'QS ',i,j,QS(I,j)
          QS(I,J) = 0.0
        ENDIF
  500 CONTINUE

      RETURN
      END
