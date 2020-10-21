      PROGRAM TEST

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
      REAL QTM(0:L,0:M), ATMFLX(0:L,0:M)

       INTEGER LRHS, LNEW 

       REAL bathy(0:L, 0:M)
       REAL DCVM, WUP, COSGAM, RTC, STC, QTOC

      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M)
C-----------------------------------------------------------------------

      CALL GROWTH(LRHS, LNEW, LWDN, SWDN, bathy, 
     1  DCVM, WUP, COSGAM, RTC, STC, QTOC,
     2  SNOFLG, TICM, U, V,
     3  QTM, ATMFLX, SURTYP, 
     4  QS, QT, QH, QSB, QTB, QHB, QDS, QDT, QHSTO, HS, HT, 
     5  QV, QRHO, QW, FW, IEN, MLFIX,
     6  IIC, H, A, HSN,
     7  H0, ARMIN, ARMAX, HMIN,
     8  TAIR, TD, ACL, PA, UG, TA, RPREC,
     9  OM)

      STOP
      END
