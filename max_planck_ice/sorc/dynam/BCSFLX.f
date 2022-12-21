      SUBROUTINE BCSFLX(FX,FY)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -SETS FLUXES NORMAL TO BOUNDARIES TO ZERO EXCEPT WHERE CYCLIC
C       BOUNDARY CONDITIONS APPLY
C  METHOD:
C     -USES FLUX MASK DETERMINED IN SUBROUTINE BCSINIT
C  INTERFACE:
C     -FX: FLUX IN X-DIRECTION
C     -FY: FLUX IN Y-DIRECTION
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM
      REAL FX(0:L,0:M), FY(0:L,0:M)
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  SET DIFFUSIVE FLUXES ACROSS THE BOUNDARIES TO 0
C-----------------------------------------------------------------------
      DO 10 J=0,M
      DO 10 I=0,L
       FX(I,J)=FLM(I,J,1)*FX(I,J)
       FY(I,J)=FLM(I,J,2)*FY(I,J)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BOUNDARY CONDITIONS AS NEEDED
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 3) GO TO 9999
      DO 30 J=2,MM
       FX(1,J)=FX(LM,J)
       FY(1,J)=FY(LM,J)
       FX(L,J)=FX(2,J)
       FY(L,J)=FY(2,J)
   30 CONTINUE

 9999 CONTINUE

      RETURN
      END
