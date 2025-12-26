      SUBROUTINE P2FILT(MKOUNT,HBM2,FIELD)
CFPP$ NOCONCUR R
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    P2FILT      4-TH ORDER SMOOTHER FOR MASS PTS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-02
C     
C ABSTRACT:
C     THIS ROUTINE WILL APPLY A FOURTH OTHER FILTER (SMOOTHER)
C     OF A CONTINOUS FIELD AT MASS POINTS ON THE STAGGERED
C     E-GRID.  MULTIPLE APPLICATIONS OF THIS FILTER MAY BE
C     MADE.  
C
C     IN THIS VERSION, EACH MPI TASK WORKS ON ITS OWN DATA.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  ??? - SUBROUTINE P2FILT IN ETA MODEL.
C   93-03-02  RUSS TREADON - ADDED DOCBLOC
C   98-06-04  BLACK - CONVERSION TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL P2FILT(MKOUNT,HBM2,FIELD)
C   INPUT ARGUMENT LIST:
C     MKOUNT   - NUMBER OF APPLICATIONS OF THE SMOOTHER.
C     HBM2     - MASS POINT BOUNDARY MASK.
C     FIELD    - MASS POINT FIELD TO BE SMOOTHED.
C
C   OUTPUT ARGUMENT LIST: 
C     FIELD    - SMOOTHED MASS POINT FIELD.
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
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE GLOBAL PARAMETERS.  SET DEPENDENT VARIABLES.
C
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
                            D I M E N S I O N
     1 HBM2(IM,JM),FIELD(IM,JM),WFIELD(IM,JM),FNE(IM,JM),FSE(IM,JM)
C     
C***********************************************************************
C     START P2FILT HERE.
C     
C     IF NUMBER OF PASSES IS LESS THAN 1, EXIT ROUTINE.
C
      IF(MKOUNT.LT.1)        RETURN
C     
C     OTHERWISE, APPLY SMOOTHER MKOUNT TIMES.
C
      DO 400 KOUNT=1,MKOUNT
C
C        FILTERING STARTS
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        FNE   (I,J)=0.0
        FSE   (I,J)=0.0
        WFIELD(I,J)=0.0
      ENDDO
      ENDDO
C
C        2-ND ORDER DIAGONAL CONTRIBUTIONS
C
       CALL EXCH(FIELD)
C
!$omp  parallel do
      DO J=JSTA,JEND_M
      DO I=1,IM-1
        FNE(I,J)=FIELD(I+IHE(J),J+1)-FIELD(I,J)
      ENDDO
      ENDDO
C
!$omp  parallel do
      DO J=JSTA_M,JEND
      DO I=1,IM-1
        FSE(I,J)=FIELD(I+IHE(J),J-1)-FIELD(I,J)
      ENDDO
      ENDDO
C
      CALL EXCH(FNE)
      CALL EXCH(FSE)
C
!$omp  parallel do
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        WFIELD(I,J)=(FNE(I,J)-FNE(I+IHW(J),J-1)
     1           +FSE(I,J)-FSE(I+IHW(J),J+1))*HBM2(I,J)*0.125
      ENDDO
      ENDDO
C
C     4-TH ORDER DIAGONAL CONTRIBUTIONS
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
        FSE(I,J)=WFIELD(I+IHE(J),J-1)-WFIELD(I,J)
      ENDDO
      ENDDO
C
      CALL EXCH(FNE)
      CALL EXCH(FSE)
C
!$omp  parallel do
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        FIELD(I,J)=FIELD(I,J)-(FNE(I,J)-FNE(I+IHW(J),J-1)
     1                        +FSE(I,J)-FSE(I+IHW(J),J+1))
     2                        *HBM2(I,J)*0.125
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
