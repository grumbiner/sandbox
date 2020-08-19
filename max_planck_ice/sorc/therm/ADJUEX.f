      SUBROUTINE ADJUEX(QHST, SNOW, FLAGI, OM, TMP, TMP2, TMP3,
     1  QS, QSB, QT, QTB, QH, QHB, QDS, QDT, HS, HT, MLFIX)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C     -Robert Grumbine        NMC Camp Springs                      1994
C  PURPOSE:
C     -ADJUSTMENT OF MIXED LAYER VARIABLES WHICH WHERE MODIFIED BY EN-
C       TRAINMENT OR DETRAINMENT AND BY ADVECTION
C  INTERFACE:
C     -QHST:  CHANGE IN ICE THICKNESS OR HEAT STORAGE (IF NEGATIVE)
C     -SNOW:  CURRENT SNOW DEPTH
C     -FLAGI: FLAG FIELD FOR PRESENCE OF ICE AND/OR SNOW
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C     -VECMIN:  THE THIRD ARGUMENT IS MINIMUM OF THE FIRST TWO ARGUMENTS
C     -VERDIF:  DETERMINES VERTICAL DIFFUSION
C  Last Modified 18 September 1996
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
      INCLUDE "oml.inc"
C=======================================================================
      REAL QS(0:L,0:M), QSB(0:L,0:M), QT(0:L,0:M), QTB(0:L,0:M) 
      REAL QH(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M)
      REAL HS(0:L,0:M), HT(0:L,0:M)
      INTEGER  MLFIX

      REAL OM(0:L, 0:M)

      REAL TMP(0:L, 0:M), TMP2(0:L,0:M), TMP3(0:L, 0:M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -TMP2: TEMPORARY ARRAY
C     -TMP3: TEMPORARY ARRAY
C=======================================================================
      REAL QHST(0:L,0:M), SNOW(0:L,0:M), FLAGI(0:L,0:M), TMP4(0:L,0:M)
     1  ,TMP5(0:L,0:M)
      REAL FLAGM, SS, TT, QFMO1, QSFO, QSM, QOC
      REAL tfreez
      INTEGER I, J
C=======================================================================
      FLAGM=1.0-FLOAT(MLFIX)
      CALL VECMAX(QHST,0.,TMP)
C     Ensure that mixed layer thickness is non-zero.  Robert Grumbine 
      CALL VECMAX(QH, minmix, QH)

C Correction - Stoessel - 17 Feb 1993 - QTOC->QOC to avoid modifying
C    QTOC, which is a common block variable.  Has effect if MLFIX=1.
C    Change Made 20 March 1995. Robert Grumbine
      DO 210 J=1,MM
      DO 210 I=0,L
       QOC=( tfreez(QS(I,J)) -QT(I,J) )*QH(I,J)*FLAGI(I,J)*OM(I,J)
       HT(I,J)=HT(I,J)+QOC*FLAGI(I,J)
       QHST(I,J)=QHST(I,J)+CC/CLO*QOC*FLAGI(I,J)
  210 CONTINUE
      CALL VECMAX(QHST,0.,TMP2)
      CALL VECMIN(QHST,0.,TMP3)
      DO 215 J=1,MM
      DO 215 I=0,L
       TMP4(I,J)=SNOW(I,J)
       SNOW(I,J)=SNOW(I,J)+RHOICE/RHOSNO*TMP3(I,J)*OM(I,J)
  215 CONTINUE
      CALL VECMIN(SNOW,0.,TMP3)
      CALL VECMAX(SNOW,0.,TMP5)
      DO 220 J=1,MM
      DO 220 I=0,L
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)
       SS=SS*FLAGI(I,J)+1.0-FLAGI(I,J)
       TT=TT*FLAGI(I,J)+1.0-FLAGI(I,J)
       SNOW(I,J)=TMP5(I,J)
       TMP3(I,J)=TMP3(I,J)*RHOSNO/RHOICE
       QFMO1=(TMP(I,J)-TMP2(I,J))*RHOICE/RHOWAT
       QSM=(TMP4(I,J)-SNOW(I,J))*RHOSNO/RHOWAT
       QSFO=-QFMO1*(QS(I,J)-SICE)-QSM*QS(I,J)
       HS(I,J)=HS(I,J)+QSFO*FLAGI(I,J)*OM(I,J)
       HT(I,J)=HT(I,J)-TMP3(I,J)*CLO/CC*FLAGI(I,J)*OM(I,J)
CBG       QT(I,J)=QT(I,J)-(QT(I,J)-TFREZ+TMP3(I,J)*CL/CC/QH(I,J))
C      BG  Original line is above.  CL is unitialized, but CLB and
C        CLO are initialized (COMMON /ABLM/).  They are ice density
C        times latent heat of fusion at bottom and top, respectively
C        of the ice surface.  Insert CLO here, as that is the
C        variable used elsewhere (and CLO and CLB should be
C        fairly close to each other.  BG 4/22/92.
       IF (QH(I,J) .EQ. 0.) STOP "QH = 0"
       QT(I,J)=QT(I,J)- (QT(I,J)- tfreez(QS(I,J)) + TMP3(I,J)
     1         *CLO/CC/QH(I,J)     )
     1         *FLAGI(I,J)*OM(I,J)
       QS(I,J)=QS(I,J)+QSFO/QH(I,J)*FLAGI(I,J)*OM(I,J)
       SS=QSB(I,J)-QS(I,J)
       TT=QTB(I,J)-QT(I,J)

C      Following two lines added by Robert Grumbine to ensure against
C        divisions by zero.  19 September 1994.
       IF (SS .EQ. 0.) SS = 1.E-2
       IF (TT .EQ. 0.) TT = 1.E-2

       SS=SS*FLAGI(I,J)+1.0-FLAGI(I,J)
       TT=TT*FLAGI(I,J)+1.0-FLAGI(I,J)

CD       IF (ss .LT. 0.1) THEN
CD         PRINT *,'ss = ',i,j, ss
CD       ENDIF
CD       IF (tt .LT. 0.1) THEN
CD         PRINT *,'tt = ',i,j, tt
CD       ENDIF

       QDT(I,J)=QDT(I,J)-(QDT(I,J)-(QTB(I,J)*QHB(I,J)-HT(I,J))/TT
     1          +QH(I,J))*FLAGI(I,J)*FLAGM*OM(I,J)
       QDS(I,J)=QDS(I,J)-(QDS(I,J)-(QSB(I,J)*QHB(I,J)-HS(I,J))/SS
     1          +QH(I,J))*FLAGI(I,J)*FLAGM*OM(I,J)
  220 CONTINUE
      IF (MLFIX.EQ.1) GOTO 300
      CALL VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)

  300 CONTINUE

      RETURN
      END
