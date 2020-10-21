      PROGRAM ICEMODEL
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
      INCLUDE "rheology.inc"
      INCLUDE "oml.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      REAL FM, F, COSPHI, SINPHI
C=======================================================================
      COMMON/IPARM/H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN
C=======================================================================
      COMMON/DRV/DXSQ, DYSQ, SX2, SY2, SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
C=======================================================================
      COMMON/STP/T, NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      REAL T
      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
C=======================================================================
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
C=======================================================================
      REAL U(L,M,3), V(L,M,3)
C=======================================================================
      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
C=======================================================================
      COMMON/FRWND/CDWIN, SINWIN, COSWIN, UWIN(L,M), VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN 
C=======================================================================
      COMMON/FRWAT/SINWAT, COSWAT, UWAT(L,M), VWAT(L,M)
      REAL SINWAT, COSWAT, UWAT, VWAT
C=======================================================================
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1  ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL TAIR, TD, ACL, PA, UG, TA, RPREC
C=======================================================================
      REAL TICE(0:L, 0:M)
C=======================================================================
      REAL TICM(0:L, 0:M, NLEVEL)
C=======================================================================
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
C=======================================================================
      REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M)
      REAL QSB(0:L,0:M), QTB(0:L,0:M), QHB(0:L,0:M)
      REAL QDS(0:L,0:M), QDT(0:L,0:M)
      REAL QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M)
      REAL QRHO(0:L,0:M), QW(0:L,0:M), FW(0:L,0:M)
      INTEGER IEN(0:L,0:M), MLFIX

      REAL bathy(0:L, 0:M)
      
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M)
C=======================================================================
      REAL SURTYP, SURFWIN
C=======================================================================
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      REAL CD, SINBET, COSBET, BETA, TAUX, TAUY
C=======================================================================
      COMMON/OUTFLOW/NOUT, IOUT(LDO), JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
C=======================================================================
      REAL DCVM, WUP, COSGAM, RTC, STC, QTOC
C=======================================================================
      REAL SNOFLG
C=======================================================================
      REAL QTM(0:L, 0:M), ATMFLX(0:L,0:M)
C     Output fields.  BG
      REAL FLAGI1(L,M), FLAGI2(L,M), FLAGI(0:L,0:M)
C=======================================================================
C      Local declarations
      INTEGER NOLRREC, INTYP
      LOGICAL noice, range
      INTEGER I, LOLD, LNEW

C-----------------------------------------------------------------------
C  SET INITIAL VALUES OF RUNNING INDICES
C-----------------------------------------------------------------------
      LOLD=1
      LNEW=2
C-----------------------------------------------------------------------
C  CALL SUBROUTINE INIT TO SET UP PARAMETERS OF THE MODEL
C-----------------------------------------------------------------------
      CALL INIT(INTYP, bathy, DCVM, WUP, COSGAM, RTC, STC, QTOC,
     1     SNOFLG,
     2     TICE, TICM,
     3     U, V,
     4     SURTYP, SURFWIN,
     5     QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, 
     6     QV, QRHO, QW, FW, IEN, MLFIX, 
     7     H, A, HSN)
C-----------------------------------------------------------------------
C  INITIAL CALL FOR CYCLIC BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
      CALL BCSH(H, LOLD, OM)
      CALL BCSH(A, LOLD, OM)
      CALL BCSH(HSN, LOLD, OM)
      CALL BCSV(U, V, LOLD, VM)
      
C-----------------------------------------------------------------------
      CALL outstr(
     1    FLAGI1, FLAGI2, FLAGI,
     2    OM, TAUX, TAUY, TA, TICM, QH ,
     3    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     4    CLO, T, 
     5    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS)
C-----------------------------------------------------------------------
C----------------------- MAIN COMPUTATIONAL LOOP -----------------------
C-----------------------------------------------------------------------
      DO 400 IIC=1, NTMES
       T=T+DT
C-----------------------------------------------------------------------
C  FIRST GET THE FORCING FIELDS FOR THIS TIME STEP
C-----------------------------------------------------------------------
       CALL FORFLD(LWDN, SWDN, INTYP, IIC, UWIN, VWIN)

C-----------------------------------------------------------------------
C      IF there is no ice cover, skip to the thermodynamics
C-----------------------------------------------------------------------
       IF (noice(A, LP, MP, 2, LOLD)) GO TO 9999

 9999  CONTINUE
C-----------------------------------------------------------------------
C  NOW DO THE THERMODYNAMIC GROWTH CALCULATIONS FOR H AND A
C-----------------------------------------------------------------------
       CALL GROWTH(LOLD, LNEW, LWDN, SWDN, bathy, 
     1  DCVM, WUP, COSGAM, RTC, STC, QTOC, SNOFLG, TICM,
     2  U, V,
     3  QTM, ATMFLX, SURTYP,
     5  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, 
     6  QV, QRHO, QW, FW, IEN, MLFIX,
     7  IIC, H, A, HSN, 
     8  H0, ARMIN, ARMAX, HMIN,
     9  TAIR, TD, ACL, PA, UG, TA, RPREC,
     1  OM, PM, PN)
C-----------------------------------------------------------------------
C  RESET HORIZONTAL BOUNDARY CONDITIONS FOR H AND A
C-----------------------------------------------------------------------
       CALL BCSH(H, LNEW, OM)
       CALL BCSH(A, LNEW, OM)
       CALL BCSH(HSN, LNEW, OM)

C-----------------------------------------------------------------------
C----- THE REST OF THE COMP. LOOP IS RESERVED FOR VARIOUS OUTPUTS ------
C-----------------------------------------------------------------------
      CALL output(
     1    FLAGI1, FLAGI2, FLAGI, 
     2    OM, TAUX, TAUY, TA, TICM, QH ,
     3    QTM, ATMFLX, A, FW, H, U, V, VM, PN, PM, HSN, QS, QT,
     4    CLO, T, 
     5    IIC, LNEW, LOLD, NFLD, NPLT, NSTA, NSTAT, NTMES, NRST, NRREC,
     6    TICE, QTB, QSB, QHB, QDT, QDS)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  GET READY FOR NEW TIME STEP: SWITCH OLD AND NEW INDICES
C-----------------------------------------------------------------------
       LOLD=LNEW
       LNEW=3-LOLD
  400 CONTINUE
C-----------------------------------------------------------------------
C---------------------- END OF COMPUTATIONAL LOOP ----------------------
C-----------------------------------------------------------------------
      NOLRREC = 0
      WRITE (*, 980) NRREC-NOLRREC
      STOP
  980 FORMAT (' ',I5,' RESTART RECORDS WRITTEN')
      END
