      SUBROUTINE PLAST(LRHS, E11, E22, E12, ZETA, ETA, U, V, H, A, HSN)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE PLAST IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  PURPOSE:
C     -CALCULATION OF VISCOSITIES
C  METHOD:
C     -USES STRAIN RATES CALCULATED IN SUBROUTINE STRAIN
C     -USES ICE STRENGTH DETERMINED IN SUBROUTINE PRESSB
C     -DETERMINES STRAIN RATE INVARIANTS (EQ.(9) IN HIBLER (1979))
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C  EXTERNALS:
C     -STRAIN: CALCULATES STRAIN RATE TENSOR
C     -OUTBCS: SETS VISCOSITIES AND ICE STRENGTH TO 0 AT OUTFLOW POINTS
C  LAST MODIFIED: 2 September 1993.
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      INTEGER LRHS
      REAL H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      REAL E11(L,M), E22(L,M), E12(L,M), ZETA(L,M), ETA(L,M)
      REAL U(L,M,3), V(L,M,3)

      COMMON/PRESS/P(L,M)
      REAL P
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
C=======================================================================
C**REMARK: THIS FORM OF THE COMMON BLOCK INSURES THAT ZETA AND ETA ARE
C    CORRECTLY PASSED TO SUBROUTINES RELCON AND RELAX
C     -TRK:  DUMMY REGISTER
C     -E11:  X-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E22:  Y-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E12:  DEVIATORIC (SHEAR) STRAIN RATE
C     -WRK:  DUMMY REGISTERS
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      REAL TMP(L,M), FLG(L,M)
C=======================================================================
C     -TMP: TEMPORARY REGISTER
C     -FLG: FLAGS FOR CHANGING APPLICATIONS
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  CALCULATE STRAIN RATE TENSOR
C-----------------------------------------------------------------------
CD      PRINT *,'Entered plast'
      CALL STRAIN(LRHS, E11, E12, E22, U, V, PM, PN)
      DO 10 J=1,MM
      DO 10 I=1,L
C-----------------------------------------------------------------------
C  DETERMINE THE 'DELTA' IN EQ.9 OF HIBLER (79)
C-----------------------------------------------------------------------
       TMP(I,J)=SQRT((E11(I,J)**2+E22(I,J)**2)*(1.0+ECM2)
     1 +4.0*ECM2*E12(I,J)**2+2.*E11(I,J)*E22(I,J)*(1.-ECM2))
C-----------------------------------------------------------------------
C  USE THE MAXIMUM VISCOUS CREEP RATE IF DELTA IS SMALLER
C-----------------------------------------------------------------------
       FLG(I,J)=0.5*(1.+SIGN(1.,(TMP(I,J)-GMIN)))
       TMP(I,J)=FLG(I,J)*TMP(I,J)+(1.-FLG(I,J))*GMIN
C  BULK VISCOSITY:
       ZETA(I,J)=0.5*P(I,J)/TMP(I,J)
C-----------------------------------------------------------------------
C  USE MIN AND MAX BULK VISCOSITIES TO CONSTRAIN THE VISCOSITIES - 
C   Relict experiment, deleted per A. Stossel comment of 11 Mar 1993.
C-----------------------------------------------------------------------
C  SHEAR VISCOSITY (EQ.11 IN HIBLER (79)):
       ETA(I,J)=ECM2*ZETA(I,J)
   10 CONTINUE
C-----------------------------------------------------------------------
C  NOW SET VISCOSITIES AND PRESSURE EQUAL TO ZERO AT OUTFLOW PTS
C-----------------------------------------------------------------------
      CALL OUTBCS(ETA, ZETA)

CD      PRINT *,'Leaving plast'
      RETURN
      END
