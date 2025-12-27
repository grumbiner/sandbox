      SUBROUTINE P2FLTV(MKOUNT,VBM2,FIELD)
CFPP$ NOCONCUR R
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    P2FLTV      4-TH ORDER SMOOTHER for vel pts
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-02
C     
C ABSTRACT:
C     THIS ROUTINE WILL APPLY A FOURTH OTHER FILTER (SMOOTHER)
C     OF A CONTINOUS FIELD AT VELOCITY POINTS ON THE STAGGERED
C     E-GRID.  MULTIPLE APPLICATIONS OF THIS FILTER MAY BE
C     MADE.  
C     IN THIS VERSION, EACH TASK WORKS ON THE DATA IT OWNS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-03-02  RUSS TREADON - MODIFIED SUBROUTINE P2FILT.
C   98-06-04  BLACK - CONVERSION TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL P2FLTV(MKOUNT,VBM2,FIELD)
C   INPUT ARGUMENT LIST:
C     MKOUNT   - NUMBER OF APPLICATIONS OF THE SMOOTHER.
C     HBM2     - VELOCITY POINT BOUNDARY MASK.
C     FIELD    - VELOCITY POINT FIELD TO BE SMOOTHED.
C
C   OUTPUT ARGUMENT LIST: 
C     FIELD    - SMOOTHED VELOCITY POINT FIELD.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY Y-MP
C$$$  
C     
C     
C     INCLUDE GLOBAL PARAMETERS.  SET DEPENDENT VARIABLES.
C
C----------------------------------------------------------------------
      INCLUDE "parmeta"
C
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C----------------------------------------------------------------------
                            D I M E N S I O N
     & VBM2(IM,JM),FIELD(IM,JM),WFIELD(IM,JM),FNE(IM,JM),FSE(IM,JM)
C     
C***********************************************************************
C     START P2FLTV HERE.
C     
C     IF NUMBER OF PASSES IS LESS THAN 1, EXIT ROUTINE.
C
      IF(MKOUNT.LT.1)        RETURN
C     
C     OTHERWISE, APPLY SMOOTHER MKOUND TIMES.
C
C-----------------------------------------------------------------------
      DO 400 KOUNT=1,MKOUNT
C-----------------------------------------------------------------------
C
C        FILTERING STARTS
C
C-----------------------------------------------------------------------
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        FNE   (I,J)=0.0
        FSE   (I,J)=0.0
        WFIELD(I,J)=0.0
      ENDDO
      ENDDO
C
C
      CALL EXCH(FIELD)
C
C        2-ND ORDER DIAGONAL CONTRIBUTIONS
C
!$omp  parallel do
      DO J=JSTA,JEND_M
      DO I=1,IM-1
        FNE(I,J)=FIELD(I+IVE(J),J+1)-FIELD(I,J)
      ENDDO
      ENDDO
C
!$omp  parallel do
      DO J=JSTA_M,JEND
      DO I=1,IM-1
        FSE(I,J)=FIELD(I+IVE(J),J-1)-FIELD(I,J)
      ENDDO
      ENDDO
C
      CALL EXCH(FNE)
      CALL EXCH(FSE)
C
!$omp  parallel do
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        WFIELD(I,J)=(FNE(I,J)-FNE(I+IVW(J),J-1)
     1              +FSE(I,J)-FSE(I+IVW(J),J+1))
     2              *VBM2(I,J)*0.125
      ENDDO
      ENDDO
C
C        4-TH ORDER DIAGONAL CONTRIBUTIONS
C
       CALL EXCH(WFIELD)
C
!$omp  parallel do
      DO J=JSTA,JEND_M
      DO I=1,IM-1
        FNE(I,J)=WFIELD(I+IHE(J),J+1)-WFIELD(I,J)
      ENDDO
      ENDDO
C
!$omp  parallel do
      DO J=JSTA_M,JEND
      DO I=1,IM-1
        FSE(I,J)=WFIELD(I+IVE(J),J-1)-WFIELD(I,J)
      ENDDO
      ENDDO
C
      CALL EXCH(FNE)
      CALL EXCH(FSE)
C
!$omp  parallel do
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        FIELD(I,J)=FIELD(I,J)-(FNE(I,J)-FNE(I+IVW(J),J-1)
     1                        +FSE(I,J)-FSE(I+IVW(J),J+1))
     2                        *VBM2(I,J)*0.125
      ENDDO
      ENDDO
C
 400  CONTINUE
C     
      CALL EXCH(FIELD)
C
C     END OF ROUTINE
      RETURN
      END
