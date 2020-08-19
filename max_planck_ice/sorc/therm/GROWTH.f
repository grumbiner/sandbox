      SUBROUTINE GROWTH(LRHS, LNEW, LWDN, SWDN, bathy, 
     1  DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2  SNOFLG, TICM, U, V,
     3  QTM, ATMFLX, SURTYP, 
     4  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, 
     5  QV, QRHO, QW, FW, IEN, MLFIX,
     6  IIC, H, A, HSN,
     7  H0, ARMIN, ARMAX, HMIN,
     8  TAIR, TD, ACL, PA, UG, TA, RPREC,
     9  OM, 
     1  PM, PN)
C=======================================================================
C  PROGRAMMED BY:
C     W.D.HIBLER III           CRREL, HANOVER, USA                  1979
C     W.B.OWENS                MPI, HAMBURG                         1987
C     R. W. Grumbine           NMC, Camp Springs, MD                1992
C     R. W. Grumbine           NMC, Camp Springs, MD             Jul. 95
C     R. W. Grumbine           NCEP, Camp Springs, MD            Feb. 97
C  PURPOSE:
C     -CALCULATION OF CHANGES IN ICE THICKNESS, ICE COMPACTNESS AND SNOW
C       THICKNESS DUE TO THERMODYNAMIC EFFECTS
C  METHOD:
C     -CALCULATES HEAT BUDGETS FOR OPEN WATER PART AND ICE COVERED PART
C       OF A GRID CELL, THE LATTER BEING OPTIONAL FOR A SEVEN-LEVEL
C       ICE THICKNESS DISTRIBUTION ACC. TO HIBLER (84)
C     -CALCULATES ADDITIONAL DYNAMIC TERM IN ICE COMPACTNESS EQUATION
C       ACC. TO HIBLER (84)
C     -DETERMINES SNOW THICKNESS ACC. TO OWENS AND LEMKE (90)
C     -TAKES INTO CONSIDERATION THE VERTICAL OCEANIC HEAT FLUX, WHICH
C       IS CALCULATED IN THE OML-ROUTINE
C     -Downwelling radiation accepted as an argument. BG
C     -Ocean bathymetry accepted as an argument. BG
C  OPTIONS:
C     -INCLUSION OF SEVEN-LEVEL ICE THICKNESS DISTRIBUTION
C     -INCLUSION OF Variable number of levels, selected in icegrid.inc
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD TIME STEP
C     -LNEW: RUNNING INDEX FOR NEW TIME STEP
C  EXTERNALS:
C     -SHDEF:   CREATES OPEN WATER DUE TO SHEAR DEFORMATION
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMIN:  THE THIRD ARGUMENT IS MINIMUM OF THE FIRST TWO ARGUMENTS
C     -SHWARA:  CALCULATES SHORT WAVE RADIATION
C     -SHWARA:  Replaced by a read in FORFLD
C     -OBUDGET: CALCULATES OPEN WATER HEAT BUDGET
C     -ECMBDO: CALCULATES OPEN WATER HEAT BUDGET WITH ASL-MODEL
C     -EKMAO:   CALCULATES OPEN WATER HEAT BUDGET WITH ABL-MODEL
C     -BUDGET:  CALCULATES HEAT BUDGET OVER ICE
C     -ECMBDI: CALCULATES HEAT BUDGET OVER ICE WITH ASL-MODEL
C     -EKMAH:   CALCULATES HEAT BUDGET OVER ICE WITH ABL-MODEL
C     -PMLEX:   CALCULATES OML-VARIABLES (OML-MODEL)
C     -VECMINC: SAME AS VECMIN, THE SECOND ARGUMENT BEING AN ARRAY
C     -tfreez:  Compute the freezing point of salt water
C     -albedo:  Compute the albedo of potentially snow covered sea ice
C  Last Modified 10 March 1999
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      REAL H0, ARMIN, ARMAX, HMIN

      REAL U(L, M, 3), V(L, M, 3)

      INTEGER IIC

      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)

      REAL TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
      REAL UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)

      REAL TICM(0:L, 0:M, NLEVEL)

      REAL OM(0:L,0:M)

      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M)
      REAL QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

      REAL SNOFLG

      REAL SURTYP

      REAL TMP(0:L,0:M), RH(0:L,0:M), RA(0:L,0:M), ALB(0:L,0:M),
     1 TMP3(0:L,0:M), QHST(0:L,0:M), QFM(0:L,0:M),
     2 PREC(0:L,0:M), SN(0:L,0:M), QTM(0:L,0:M), ATMFLX(0:L,0:M)

       REAL TMP2(0:L,0:M)
       REAL A2(0:L, 0:M), FLSE(0:L, 0:M), FLLA(0:L, 0:M)

       INTEGER LRHS, LNEW 
       REAL ABLFIX, ECMTYP

       REAL hdraft(0:L, 0:M), bathy(0:L, 0:M), OPEW(0:L, 0:M)
CD       REAL hmlref ! hmlref is in include now
       REAL DCVM, WUP, COSGAM, RTC, STC, QTOC

       REAL PM(0:L,0:M), PN(0:L,0:M)
C=======================================================================
C     -TMP:  TEMP. ARRAY (PASSES THE EFFECTIVE ICE THICKNESS TO BUDGET)
C     -RH:   TEMP. ARRAY (RETURNS GROWTH RATE OF THICK ICE FROM BUDGET)
C     -RA:   TEMP. ARRAY (RETURNS GROWTH RATE OF THIN ICE FROM BUDGET)
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C     -TMP4: TEMPORARY ARRAY
C     -QHST: CHANGE IN ICE THICKNESS OR HEAT STORAGE (IF NEGATIVE)
C     -QFM:  AMOUNT OF ICE MELTED [M]
C     -PREC: FRESH WATER INPUT (SNOW AND ICE MELT + RAIN)
C     -SN:   TEMPORARY ARRAY (EFFECTIVE ICE THICKNESS OR SNOW DEPTH)
C     -QTM:  VERTICAL OCEANIC HEAT FLUX (ON OUTPUT OF PMLEX)
C     -SH:   TEMP.ARR.(7-LEVEL ICE THICKN.AND THERMODYN.THICKN.CHANGE)
C     -SH:   TEMP.ARR.(N-LEVEL ICE THICKN.AND THERMODYN.THICKN.CHANGE)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M)
C     ARMAG is the parameter deciding when ice is compact or not.
      REAL ARMAG
      PARAMETER (ARMAG = 0.85)
      LOGICAL overload
      REAL albedo, tfreez
      INTEGER i, j, k

C-----------------------------------------------------------------------
CBG      PRINT *,'Entered Growth'

      ABLFIX = 0.
      ECMTYP = 1.
C-----------------------------------------------------------------------
C  CREATE OPEN WATER DUE TO SHEAR DEFORMATION
C-----------------------------------------------------------------------
      CALL SHDEF(LNEW, OPEW, U, V, PM, PN)
CBG      PRINT *, "back from shdef"
      CALL VECMAX(A(0,0,LRHS), ARMAG, TMP)
      DO 301 J=1,MM
      DO 301 I=0,L
       TMP2(I,J)=(1.0-TMP(I,J))*(0.5*(1.0+TMP(I,J))-ARMAG)
     1           /((1.-ARMAG)*(0.5*(1.+ARMAG)-ARMAG))
       A(I,J,LNEW)=A(I,J,LNEW)-OPEW(I,J)*(1.0-TMP2(I,J))*DT
  301 CONTINUE
      IF (overload(A(0,0,LNEW), L+1, M+1, 1, .FALSE.) ) THEN
        STOP 'A is too large!'
      ENDIF  
C-----------------------------------------------------------------------
C  CALCULATE COS OF ZENITH DISTANCE AND SOLAR CONSTANT
C   Removed by BG.  Now use externally computed radiation
C-----------------------------------------------------------------------
C  CALCULATE THE GROWTH RATE FOR THIN ICE (OPEN OCEAN)
C-----------------------------------------------------------------------
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP.  According to SURTYP
      IF (SURTYP .EQ. 0.) THEN
CBG        CALL OBUDGET(RA, QT, LWDN, SWDN)
        CALL OBUDGET(RA, QT, LWDN, SWDN, TAIR, TD, PA, UG, TA, OM, 
     1                FLSE, FLLA)
CBG      PRINT *, "back from obudget"
       ELSEIF (SURTYP .EQ. 1.) THEN
CBG 24 Feb 1997       CALL ECMBDO(RA, QT, LWDN, SWDN)
           PRINT *,'Surface type 1 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSEIF (SURTYP .EQ. 2.) THEN
CBG 24 Feb 1997        CALL EKMAO(RA, QT, LWDN, SWDN, QS)
           PRINT *,'Surface type 2 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSE
        PRINT *,'Selected surtyp out of range.  surtyp = ',surtyp
        STOP
      ENDIF
C-----------------------------------------------------------------------
C  CALCULATE EFFECTIVE ICE THICKNESS
C-----------------------------------------------------------------------
C  MAKE SURE WE HAVE NON-ZERO COMPACTNESS:
      CALL VECMAX(A(0,0,LRHS), ARMIN,TMP)
      DO 5 J=1,MM
      DO 5 I=0,L
C  INCLUDE SNOW THICKNESS FOR CONDUCTION EFFECT THROUGH SNOW:
       TMP(I,J)=(H(I,J,LRHS)+HSN(I,J,LRHS)*CON/CONSN)/TMP(I,J)
C**FOR 7 LAYER THERMODYNAMICS, INSERT THE FOLLOWING STATEMENT:
C**FOR N LAYER THERMODYNAMICS, INSERT THE FOLLOWING STATEMENT:
       RH(I,J)=0.0
    5 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES FOR THICK ICE
C-----------------------------------------------------------------------
C  MAKE SURE WE HAVE NON-ZERO THICKNESS FOR CONDUCTION TERM IN BUDGET:
      CALL VECMAX(TMP,HMIN,TMP)
C**FOR 7 LEVEL MODEL OF ICE, INSERT THE FOLLOWING 2 STATEMENTS:
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING 2 STATEMENTS:
      CALL VECMAX(TMP,HMIN,SN)

C Set Snow condition flags.  Robert Grumbine 24 February 1997
      DO 8000 J = 0, M
      DO 8000 I = 0, L
        IF (HSN(I,J,LRHS) .GT. 0.) THEN
          A2(I,J) = 1.0
         ELSE
          A2(I,J) = 0.0
        ENDIF
 8000 CONTINUE

      DO 8 K=1, NLEVEL
       DO 6 J=1,MM
       DO 6 I=0,L
C  SET ALBEDO ACC.TO PRESENCE OF SNOW(TMP4) AND MELTING COND.(TMP3):
CBG        TMP4(I,J)=0.5*(1.-SIGN(1.,-HSN(I,J,LRHS)))
CBG        TMP3(I,J)=0.5*(1.+SIGN(1.,TICE(I,J)))
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP3(I,J)=0.5*(1.+SIGN(1.,TICM(I,J,K)))
CBG     Change over to a called albedo routine
CBG     Robert Grumbine 25 October 1994.
COLD?        ALB(I,J) = albedo(tmp3(I,j), TAIR(I,J), 
        ALB(I,J) = albedo(TICM(i,j,k), TAIR(I,J), 
     1                  HSN(i,j,LRHS), H(i,j,LRHS) ) 
CBG        TMP2(I,J)=TMP4(I,J)*(TMP3(I,J)*ALBSNM+(1.-TMP3(I,J))*ALBSN)
CBG     1           +(1.-TMP4(I,J))*(TMP3(I,J)*ALBM+(1.-TMP3(I,J))*ALBI)
C**FOR N LEVEL MODEL OF ICE, INSERT THE FOLLOWING STATEMENT:
        TMP(I,J)=(2*K-1)*SN(I,J)/FLOAT(NLEVEL)
    6  CONTINUE
C      CALL EKMAH (RH,TICE, LWDN, SWDN)
C**FOR N LEVEL MODEL,INS. THE FOLL.6 STATEM.AND COMM.OUT PREV.STATEM.:
C**CHOICE OF ABL ACC. TO ABLFIX AND ECMTYP:
CD      PRINT *,'Calling ice surface budget'
      IF (SURTYP .EQ. 0.) THEN
CBG        CALL BUDGET(SH, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS)
C       NOTE that tmp holds HICE for budget
        CALL BUDGET(ATMFLX, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS, 
     1      IIC, TAIR, TD, PA, UG, TA, OM, FLSE, FLLA, TMP, ALB, A2,
     2      H, A, HSN)
       ELSEIF (SURTYP .EQ. 1.) THEN
CBG 24 Feb 1997        CALL ECMBDI(SH, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS)
           PRINT *,'Surface type 1 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSEIF (SURTYP .EQ. 2.) THEN
CBG 24 Feb 1997        CALL EKMAH(SH, TICM(0,0,K), LRHS, K, LWDN, SWDN, QS)
           PRINT *,'Surface type 2 has been disabled.  Please restart'
           PRINT *,' with surtyp = 0'
           STOP
       ELSE
        PRINT *,'Selected surtyp out of range.  surtyp = ',surtyp
        STOP
      ENDIF
C  GET MEAN GROWTH RATE OF THICK ICE:
       DO 7 J=1,MM
       DO 7 I=0,L
        RH(I,J)=RH(I,J)+ATMFLX(I,J)/FLOAT(NLEVEL)
    7  CONTINUE
    8 CONTINUE


      DO 10 J=1,MM
      DO 10 I=0,L
C-----------------------------------------------------------------------
C  DETERMINE THERMODYNAMIC ICE THICKNESS CHANGE FOR CONTINUITY EQUATION
C-----------------------------------------------------------------------
       RA(I,J)=RA(I,J)*OM(I,J)*DT
       RH(I,J)=RH(I,J)*OM(I,J)*DT
       ATMFLX(I,J)=RH(I,J)*A(I,J,LRHS)+(1.0-A(I,J,LRHS))*RA(I,J)
C-----------------------------------------------------------------------
C  ADD SNOWFALL TO SNOW LAYER, OR ADD PRECIPITATION FOR MIXED LAYER
C-----------------------------------------------------------------------
       TMP3(I,J)=0.5*(1.-SIGN(1.,TAIR(I,J)))*A(I,J,LRHS)*SNOFLG
       TMP(I,J)=RPREC(I,J)*DT*OM(I,J)
       PREC(I,J)=(1.-TMP3(I,J))*TMP(I,J)
       HSN(I,J,LNEW)=HSN(I,J,LNEW)+TMP3(I,J)*TMP(I,J)*RHOWAT/RHOSNO
       TMP(I,J)=RH(I,J)*A(I,J,LRHS)
   10 CONTINUE
C-----------------------------------------------------------------------
C  IF ATMFLX BECOMES NEGATIVE, FIRST MELT ANY SNOW THAT IS PRESENT
C-----------------------------------------------------------------------
C     TMP at this point holds concentration-weighted growth rate (m/step).
      CALL VECMIN(TMP,0.,TMP2)
      DO 12 J=1,MM
      DO 12 I=0,L
        TMP2(I,J)=HSN(I,J,LNEW)+TMP2(I,J)*RHOICE/RHOSNO
   12 CONTINUE
C  MAKE SURE WE DO NOT END UP WITH NEGATIVE SNOW THICKNESS:
      CALL VECMAX(TMP2,0.,SN)
      DO 15 J=1,MM
      DO 15 I=0,L
       TMP2(I,J)=HSN(I,J,LNEW)-SN(I,J)
       HSN(I,J,LNEW)=SN(I,J)
C  MODIFY THE ICE MELT AND PRECIPITATION TO ACCOUNT FOR SNOW MELT:
       RH(I,J)=ATMFLX(I,J)+TMP2(I,J)*RHOSNO/RHOICE
       PREC(I,J)=PREC(I,J)+TMP2(I,J)*RHOSNO/RHOWAT
C-----------------------------------------------------------------------
C  COMPUTE CHANGE IN ICE MASS (OR HEAT STORAGE IN OML) DUE TO ATM.FORC.
C-----------------------------------------------------------------------
       QHST(I,J)=H(I,J,LNEW)-(QT(I,J)-tfreez(QS(I,J)) ) *QH(I,J)*CC/CLO
     1  + RH(I,J)
   15 CONTINUE
C-----------------------------------------------------------------------
C  TAKE CARE OF RESIDUAL SNOW TO BE MELTED WHEN ICE HAS DISAPPEARED
C-----------------------------------------------------------------------
      CALL VECMAX(QHST,0.,TMP)
      CALL VECMIN(QHST,0.,TMP3)
      DO 17 J=1,MM
      DO 17 I=0,L
        SN(I,J)=SN(I,J)+TMP3(I,J)*RHOICE/RHOSNO
   17 CONTINUE
C-----------------------------------------------------------------------
C  UPDATE FRESH WATER INPUT, HEAT STORAGE, HEAT FLUX AND FREEZING RATE
C-----------------------------------------------------------------------
      CALL VECMIN(SN,0.,TMP2)
      CALL VECMAX(SN,0.,SN)
      CALL VECMAX(H(0,0,LNEW), 0.,QFM)
      DO 20 J=1,MM
      DO 20 I=0,L
       PREC(I,J)=PREC(I,J)+(HSN(I,J,LNEW)-SN(I,J))*RHOSNO/RHOWAT
       QHST(I,J)=QHST(I,J)+(HSN(I,J,LNEW)-SN(I,J))*RHOSNO/RHOICE
       TMP2(I,J)=TMP2(I,J)*RHOSNO/RHOICE
CBG       QTM(I,J)=( -SN(I,J)*RHOSNO/RHOICE 
       QTM(I,J)=( - TMP2(I,J)
     1            -(QT(I,J)-tfreez(QS(I,J)) ) *QH(I,J)*CC/CLO )
     1     *OM(I,J)
       IF (ABS(QTM(I,J))*CLO/CC .GE. 400. .OR. SN(I,J) .LT. 0) THEN
         WRITE (*,9001) I, J, QTM(I,J)*CLO/CC, TMP2(I,J),
     2       QH(I,J)
 9001    FORMAT ('QTM overload QTM, TMP2, QH', 2I4, F9.1, F8.1, 
     1                F7.1)
         IF (QTM(I,J) .GT. 0.) THEN
           QTM(I,J) = 10.*CC/CLO
          ELSE
           QTM(I,J) = -10.*CC/CLO
         ENDIF
       ENDIF
       QFM(I,J)=(QFM(I,J)-TMP(I,J))*OM(I,J)*RHOICE/RHOWAT
       FW(I,J)=-QFM(I,J)
   20 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE OML VARIABLES
C-----------------------------------------------------------------------
C  CALCULATE KINETIC ENERGY FOR MIXED LAYER
      DO 1000 J = 1, MM
        DO 1100 I = 1, LM
          QV(I,J) = (0.25* (SQRT(U(I,J,LNEW)**2+V(I,J,LNEW)**2)
     1                     +SQRT(U(I+1,J,LNEW)**2+V(I+1,J,LNEW)**2)
     2                     +SQRT(U(I,J+1,LNEW)**2+V(I,J+1,LNEW)**2)
     3                     +SQRT(U(I+1,J+1,LNEW)**2+V(I+1,J+1,LNEW)**2))
     4               + 0.01 )**3 * OM(I,J)
 1100   CONTINUE
 1000 CONTINUE

      IF (overload(QS, L+1, M+1, 350, .FALSE.)) THEN
        PRINT *,'QS over 350 before call to PMLEX'
      ENDIF
      IF (overload(QT, L+1, M+1, 350, .FALSE.)) THEN
        PRINT *,'QT over 350 before call to PMLEX'
      ENDIF
CBG      CALL PMLEX(QHST, SN, QFM, PREC, QTM, bathy)
      CALL PMLEX(QHST, SN, QFM, PREC, QTM, bathy,
     1  OM, DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, QV, QRHO,
     3  QW, IEN, FW, MLFIX)
C-----------------------------------------------------------------------
C  UPDATE VARIABLES DUE TO OCEANIC HEAT FLUX
C-----------------------------------------------------------------------
      DO 120 J=1,MM
      DO 120 I=0,L
C  UPDATE ICE THICKNESS OR HEAT STORAGE, RESPECTIVELY:
       QHST(I,J)=QHSTO(I,J)+OM(I,J)*(QHST(I,J)-QHSTO(I,J))
C  UPDATE NET FREEZING RATE:
       FW(I,J)=FW(I,J)-QFM(I,J)
C  UPDATE SNOW THICKNESS: BG: THIS is where we need to add dt*prate
       HSN(I,J,LNEW)=SN(I,J)
C  UPDATE THERMODYNAMIC THICKNESS CHANGE OF THIN AND THICK ICE:
       RH(I,J)=-(RH(I,J)-QTM(I,J)*DT/CLO*OM(I,J))
       RA(I,J)=RA(I,J)-QTM(I,J)*DT/CLO*OM(I,J)
  120 CONTINUE
C-----------------------------------------------------------------------
C  UPDATE THERMODYNAMIC ICE THICKNESS CHANGE (EQ.9 IN OWENS & LEMKE 90)
C-----------------------------------------------------------------------
      CALL VECMAX(QHST,0.,H(0,0,LNEW))
C  MAKE SURE WE DON'T TRY TO MELT MORE ICE THAN IS AVAILABLE:
      CALL VECMINC(RH,H(0,0,LNEW), RH)
      DO 125 J=1,MM
      DO 125 I=0,L
        RH(I,J)=-RH(I,J)
  125 CONTINUE
C-----------------------------------------------------------------------
C  UPDATE THERMODYNAMIC CHANGE IN ICE COMPACTNESS (EQ.16 IN HIBLER 79)
C-----------------------------------------------------------------------
C  MAKE SURE WE DO NOT DIVIDE BY 0 IF H=0:
      CALL VECMAX(H(0,0,LRHS), HMIN,TMP3)
C  IF MELTING THICK ICE, THEN EVALUATE THE MELTING TERM:
      CALL VECMIN(RH,0.,TMP2)
C  IF FREEZING THIN ICE, THEN EVALUATE THE FREEZING TERM:
      CALL VECMAX(RA,0.,TMP)
      DO 130 J=1,MM
      DO 130 I=0,L
       RA(I,J)=0.5*TMP2(I,J)*A(I,J,LRHS)/TMP3(I,J)
     1        +TMP(I,J)*(1.-A(I,J,LRHS))/H0
       A(I,J,LNEW)=A(I,J,LNEW)+RA(I,J)
       TMP(I,J)=H(I,J,LNEW)*1.E+06
  130 CONTINUE
C  SET COMPACTNESS TO 0 WHERE THERE IS NO ICE AND TRUNCATE:
      CALL VECMINC(A(0,0,LNEW), TMP,A(0,0,LNEW))
      CALL VECMIN(A(0,0,LNEW), ARMAX,A(0,0,LNEW))
      CALL VECMAX(A(0,0,LNEW), 0.,A(0,0,LNEW))

C-----------------------------------------------------------------------
C   Determine ice draft and snow to ice conversion.  Stossel, 1992 e-mail
C-----------------------------------------------------------------------
      DO 140 J = 1, MM
      DO 140 I = 0, L
        HDRAFT(i,j) = (RHOSNO*HSN(i,j,lnew)+RHOICE*H(i,j,lnew))/rhowat
 140  CONTINUE
      CALL VECMINC(HDRAFT, H(0,0,lnew), TMP2)
      CALL VECMAXC(HDRAFT, H(0,0,lnew), TMP)
      DO 141 J = 1, MM
      DO 141 I = 0, L
        HSN(i,j,lnew) = HSN(i,j,lnew) - 
     1                 (HDRAFT(i,j) - TMP2(i,j))*RHOICE/RHOSNO
        H(i,j,lnew) = TMP(i,j)
 141  CONTINUE

C-----------------------------------------------------------------------
C   Re-set ATMFLX from meters of ice per delta t to a heat flux
C     Robert Grumbine 28 February 1997.
C-----------------------------------------------------------------------
      DO 150 J = 0, M
      DO 150 I = 0, L
        ATMFLX(I,J) = ATMFLX(I,J)/DT * CLO
  150 CONTINUE

      RETURN
      END
