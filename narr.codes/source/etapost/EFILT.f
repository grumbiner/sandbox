      SUBROUTINE EFILT(EGRID)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    EFILT       HEAVY HANDED E-GRID SMOOTHER
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-24       
C     
C ABSTRACT:  
C     THIS ROUTINE IS A HEAVY HANDED SMOOTHER THAT WORKS ON 
C     E-GRID MASS POINTS.  IT IS BASED ON CODE IN THE OLD
C     ETA POST PROCESSOR SUBROUTINE, OUTMAP.  CURRENTLY,
C     THIS SMOOTHER IS HARDCODED TO BE USED ONLY ON VORTICITY
C     FIELDS IF THE USER ACTIVATES E-GRID SMOOTHING OF 
C     VORTICITY. IT DOES A VERY NICE JOB OF SMOOTHING THE
C     VORTICITY FIELD.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-03-24  RUSS TREADON
C   98-06-04  BLACK - CONVERSION TO 2-D
C     
C USAGE:    CALL EFILT(EGRID)
C   INPUT ARGUMENT LIST:
C     EGRID    - ARRAY OF DATA ON E-GRID
C
C   OUTPUT ARGUMENT LIST: 
C     EGRID    - ARRAY OF SMOOTHED DATA ON E-GRID
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - MASKS
C                  DYNAMD
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY Y-MP
C$$$  
C     
C     
C     INCLUDE PARAMETERS.
      INCLUDE "parmeta"
      INCLUDE "params"
C     
C     DECLARE VARIABLES.
      REAL EGRID(IM,JM),WORK1(IM,JM),WORK2(IM,JM),WORK3(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "MASKS.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "INDX.comm"
C     
C*******************************************************************
C     START EFILT HERE.
C     
C     LOAD PASSED E-GRID FIELD INTO WORK ARRAY.

!$omp  parallel do
      DO J=1,JM
      DO I=1,IM
        WORK1(I,J)=EGRID(I,J)
      ENDDO
      ENDDO
C     
C     LOAD WEIGHT ARRAY.
!$omp  parallel do
      DO J=3,JM-2
      DO I=2,IM-1
         WORK3(I,J)=1./(4.* HBM2(I,J)+
     1        2.*(HBM2(I+IHE(J),J+1)+HBM2(I+IHW(J),J+1)+
     2        HBM2(I+IHE(J),J-1)+HBM2(I+IHW(J),J-1))+
     3        HBM2(I+1,J)+HBM2(I-1,J)+
     4        HBM2(I,J+2)+HBM2(I,J-2))
      ENDDO
      ENDDO
C     
C     SMOOTHING LOOP.  SMTHA AND SMTHB ARE PARAMETERS SET IN
C     INCLUDE FILE PARAMS.
      MXFILT = NINT(SMTHA*DY+SMTHB)
C
      DO 50 NFILT = 1,MXFILT
!$omp  parallel do
         DO J=3,JM-2
         DO I=2,IM-1
            WORK2(I,J) = 4.*WORK1(I,J)+
     1           2.*(WORK1(I+IHE(J),J+1)+WORK1(I+IHW(J),J+1)+
     2           WORK1(I+IHE(J),J-1)+WORK1(I+IHW(J),J-1))+
     3           WORK1(I+1,J)+WORK1(I-1,J)+
     4           WORK1(I,J+2)+WORK1(I,J-2)
         ENDDO
         ENDDO
C
!$omp  parallel do
         DO J=3,JM-2
         DO I=2,IM-1
           WORK1(I,J)=WORK3(I,J)*WORK2(I,J)*HBM2(I,J)
         ENDDO
         ENDDO
   50 CONTINUE
C     
C     LOAD SMOOTHED EGRID INTO OUTPUT ARRAY.
!$omp  parallel do
      DO J=1,JM
      DO I=1,IM
        EGRID(I,J)=WORK1(I,J)*HBM2(I,J)
      ENDDO
      ENDDO
C     
C     END OF ROUTINE.
      RETURN
      END



