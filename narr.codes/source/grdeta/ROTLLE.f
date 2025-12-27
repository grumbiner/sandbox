C----------------------------------------------------------------------
      SUBROUTINE ROTLLE(U,V,VLAT,VLON)
C
C SUBPROGRAM: ROTLLE        ROTATE WINDS ON LAT/LONG GRID TO E-GRID
C   PRGMMR: T.BLACK         ORG: W/NMC22    DATE: ??-??-??
C
C ABSTRACT: ROTATES WINDS ON THE LAT/LONG GRID TO THE ETA MODEL GRID
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T.BLACK
C   98-06-08  M.BALDWIN - CONVERT TO 2-D CODE
C
C USAGE     CALL ROTLLE(U,V,VLAT,VLON)
C   INPUT ARGUMENT LIST:
C     U        - LAT/LONG U-COMPONENT
C     V        - LAT/LONG V-COMPONENT
C     VLAT     - LATITUDE OF E-GRID V POINTS (DEGREES)
C     VLON     - LONGITUDE OF E-GRID V POINTS (DEGREES)
C
C   OUTPUT ARGUMENT LIST:
C     U        - ETA GRID U-COMPONENT
C     V        - ETA GRID V-COMPONENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C----------------------------------------------------------------------
C***
C*** ROTATE THE LAT-LON WINDS TO THE E-GRID
C***
C
C    N O T E : INPUT LAT/LONG MUST BE CONVERTED TO RADIANS !!!
C
C----------------------------------------------------------------------
      INCLUDE "parmeta.h"
C----------------------------------------------------------------------
                         D I M E N S I O N
     1 U(IM,JM,LMAX),V(IM,JM,LMAX),VLAT(IM,JM),VLON(IM,JM)
     2,CRAY(IM,JM),DRAY(IM,JM)
C----------------------------------------------------------------------
      SPHI0 = SIN(ERPHI0)
      CPHI0 = COS(ERPHI0)
      DO J = 1, JM
      DO I = 1, IM
        TLAT = VLAT(I,J) * D2RAD
        TLON = VLON(I,J) * D2RAD
        RELM = TLON - ERLAM0
        SRLM = SIN(RELM)
        CRLM = COS(RELM)
        SPH = SIN(TLAT)
        CPH = COS(TLAT)
        CC = CPH * CRLM
        TPH = ASIN(CPHI0 * SPH - SPHI0 * CC)
        RCTPH = H1 / COS(TPH)
        CRAY(I,J) = SPHI0 * SRLM * RCTPH
        DRAY(I,J) = (CPHI0 * CPH + SPHI0 * SPH * CRLM) * RCTPH
      ENDDO
      ENDDO
      DO 100 L = 1, LMAX
      DO J = 1, JM
      DO I = 1, IM
        RU = DRAY(I,J) * U(I,J,L) - CRAY(I,J) * V(I,J,L)
        RV = CRAY(I,J) * U(I,J,L) + DRAY(I,J) * V(I,J,L)
        U(I,J,L) = RU
        V(I,J,L) = RV
      ENDDO
      ENDDO
  100 CONTINUE
C----------------------------------------------------------------------
      RETURN
      END
