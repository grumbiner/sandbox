      PROGRAM revise
C     Program to revise the output of a previous edition of the 
C       ice model to conform with the newer edition.
C     Robert Grumbine
C     Last Modified 5 December 1995

      INCLUDE "icegrid.inc"
C     Include physical constants, rheology parameters, and mixed layer
C       parameters
      INCLUDE "physical.inc"
      INCLUDE "rheology.inc"
      INCLUDE "oml.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      COMMON/VEL/U(L,M,3), V(L,M,3)
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/TEMP/TICE(0:L,0:M)
      COMMON/TEMPM/TICM(0:L,0:M,NLEVEL)
C     Commons viscp and relaxp moved to rheology.inc
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/PML/QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2 QHSTO(0:L,0:M), HS(0:L,0:M), HT(0:L,0:M), QV(0:L,0:M), 
     3 QRHO(0:L,0:M), QW(0:L,0:M), IEN(0:L,0:M), FW(0:L,0:M), MLFIX
      COMMON/GEO/PI, RAD
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1 BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/PMLPARM/DCVM, WUP, COSGAM, RTC, STC, QTOC
      COMMON/SNOFLG/SNOFLG

      REAL tempor(0:L, 0:M)

C-----------------------------------------------------------------------
C      Read in the old form:
      READ (8) U
      READ (8) V
      READ (8) H
      READ (8) A
      READ (8) HSN
      READ (8) TICE
      READ (8) QT
      READ (8) QS
      READ (8) QH
      READ (8) QTB
      READ (8) QSB
      READ (8) QHB
      READ (8) QDT
      READ (8) QDS
      READ (8) TICM
      READ (8) QSB
      READ (8) QSB
      
C-----------------------------------------------------------------------
C        Write out in the newer format
         DO 9800 j = 0, M
           DO 9801 i = 0, L
             tempor(i,j) = H(i,j, 1) 
 9801      CONTINUE
 9800    CONTINUE
         WRITE (9) tempor
         
         DO 9802 j = 0, M
           DO 9803 i = 0, L
             tempor(i,j) = A(i,j, 1) 
 9803      CONTINUE
 9802    CONTINUE
         WRITE (9) tempor
        
         DO 9804 j = 0, M
           DO 9805 i = 0, L
             tempor(i,j) = HSN(i,j, 1) 
 9805      CONTINUE
 9804    CONTINUE
         WRITE (9) tempor

         WRITE (9) TICE
         WRITE (9) QT
         WRITE (9) QS
         WRITE (9) QH
         WRITE (9) QTB
         WRITE (9) QSB
         WRITE (9) QHB
         WRITE (9) QDT
         WRITE (9) QDS
         WRITE (9) TICM
         WRITE (9) U
         WRITE (9) V
C  Done writing out the fields.

      STOP
      END
