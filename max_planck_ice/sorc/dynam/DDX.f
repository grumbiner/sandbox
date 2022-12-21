C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINES FELLD,FELLD1,FELLIP IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     Robert Grumbine          NCEP                       16 August 1994
C     Robert Grumbine          NCEP                       24 July   2002
C  LAST MODIFIED: 24 July 2002
C
C  PURPOSE:
C     -CALCULATES THE X-DERIVATIVES OF THAT PART OF THE INTERNAL ICE
C       STRESS WHICH DEPENDS ON THE CURRENT VELOCITIES
C  METHOD:
C     -SYMMETRIC DERIVATIVES ARE PARTLY SOLVED (THE REST WAS SOLVED IN
C       SUBROUTINE BCOEF), WHILE THE MIXED DERIVATIVES ARE COMPLETELY
C       SOLVED
C     -NOTE: FOR THE MIXED DERIVATIVES THE METRIC TERMS ASSOCIATED WITH
C       1/DY AND THE SIDE OF THE CELL USED FOR THE FLUX CANCEL, SO THIS
C       IS THE SAME AS THE CARTESIAN VERSION
C  INTERFACE:
C     -U:   X-COMPONENT OF ICE VELOCITY
C     -V:   Y-COMPONENT OF ICE VELOCITY
C     -ETA: BULK OR SHEAR VISCOSITY
C     -LN:  RUNNING INDEX FOR CURRENT TIME STEP
C     -K:   INDEX FOR ALTERNATING PATTERN
C  Modified for computational efficiency 7/24/2002
C=======================================================================
      SUBROUTINE DDX(U, V, ETA, LN, K, TMP, PN, PM)
      IMPLICIT none
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL U(L,M,3), V(L,M,3), ETA(L,M)
      INTEGER LN, K
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
      REAL PN(0:L,0:M), PM(0:L,0:M)
      REAL TMP(L,M)
C=======================================================================
C     -TMP: TEMPORARY ARRAY
C=======================================================================
      INTEGER I, J, IB
C=======================================================================
C  CALCULATE THE CONTRIBUTION TO D[ETA*D(U)/DX]/DX:
      DO J=2,MM
       IB=MOD(J+K,2)+2
       DO I=IB,LM,2
        TMP(I,J)=SX2*((ETA(I,J)  +ETA(I,J-1))  *U(I+1,J,LN)
     1               *(PM(I,J)  +PM(I,J-1))  /(PN(I,J)  +PN(I,J-1))
     2               +(ETA(I-1,J)+ETA(I-1,J-1))*U(I-1,J,LN)
     3               *(PM(I-1,J)+PM(I-1,J-1))/(PN(I-1,J)+PN(I-1,J-1)))
C  CALCULATE D[ETA*D(V)/DY]/DX:
        TMP(I+1,J)=SXY
     1             *(ETA(I,J)    *((V(I,J+1,LN)  +V(I+1,J+1,LN))
     2                            -(V(I,J,LN)    +V(I+1,J,LN)))
     3              +ETA(I,J-1)  *((V(I,J,LN)    +V(I+1,J,LN))
     4                            -(V(I,J-1,LN)  +V(I+1,J-1,LN)))
     5              -ETA(I-1,J)  *((V(I-1,J+1,LN)+V(I,J+1,LN))
     6                            -(V(I-1,J,LN)  +V(I,J,LN)))
     7              -ETA(I-1,J-1)*((V(I-1,J,LN)  +V(I,J,LN))
     8                            -(V(I-1,J-1,LN)+V(I,J-1,LN))))
       ENDDO
      ENDDO
   10 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE DDY(U, V, ETA, LN, K, TMP, PN, PM)
      IMPLICIT none
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL U(L,M,3), V(L,M,3), ETA(L,M)
      INTEGER LN, K
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY

      REAL PN(0:L,0:M), PM(0:L,0:M)
      REAL TMP(L,M)
C=======================================================================
C     -TMP: TEMPORARY ARRAY
C=======================================================================
      INTEGER I, J, IB
C=======================================================================
C  PURPOSE:
C     -CALCULATES THE Y-DERIVATIVES OF THAT PART OF THE INTERNAL ICE
C       STRESS WHICH DEPENDS ON THE CURRENT VELOCITIES
C=======================================================================
      DO 110 J=2,MM
       IB=MOD(J+K,2)+2
       DO 110 I=IB,LM,2
C  CALCULATE THE CONTRIBUTION TO D[ETA*D(U)/DY]/DY:
        TMP(I,J)=SY2*((ETA(I,J)  +ETA(I-1,J))  *U(I,J+1,LN)
     1               *(PN(I,J)   +PN(I-1,J))  /(PM(I,J)+PM(I-1,J))
     2               +(ETA(I,J-1)+ETA(I-1,J-1))*U(I,J-1,LN)
     3               *(PN(I,J-1) +PN(I-1,J-1))/(PM(I,J-1)+PM(I-1,J-1)))
C  CALCULATE D[ETA*D(V)/DX]/DY:
        TMP(I+1,J)=SXY
     1             *(ETA(I,J)    *((V(I+1,J,LN)  +V(I+1,J+1,LN))
     2                            -(V(I,J,LN)    +V(I,J+1,LN)))
     3              +ETA(I-1,J)  *((V(I,J,LN)    +V(I,J+1,LN))
     4                            -(V(I-1,J,LN)  +V(I-1,J+1,LN)))
     5              -ETA(I,J-1)  *((V(I+1,J-1,LN)+V(I+1,J,LN))
     6                            -(V(I,J-1,LN)  +V(I,J,LN)))
     7              -ETA(I-1,J-1)*((V(I,J-1,LN)  +V(I,J,LN))
     8                            -(V(I-1,J-1,LN)+V(I-1,J,LN))))
  110 CONTINUE
      RETURN
      END
