      SUBROUTINE CALVOR(UWND,VWND,ABSV)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALVOR      COMPUTES ABSOLUTE VORTICITY
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES THE ABSOLUTE VORTICITY USING
C     SECOND ORDER FINITE DIFFERENCES ON THE STAGGERED
C     E-GRID.
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL CALVOR(UWND,VWND,ABSV)
C   INPUT ARGUMENT LIST:
C     UWND     - U WIND (M/S)
C     VWND     - V WIND (M/S)
C
C   OUTPUT ARGUMENT LIST: 
C     ABSV     - ABSOLUTE VORTICITY (1/S)
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  DYNAMD
C                  MAPOT
C                  MASKS
C                  OPTIONS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C
C     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "params"
      PARAMETER (OMEGA=7.292E-5,TWOMG=2.*OMEGA)
C
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STRD
      REAL ABSV(IM,JM), UWND(IM,JM), VWND(IM,JM)
C     
C     DECLARE COMMONS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "INDX.comm"
C     
C***************************************************************************
C     START CALVOR HERE.
C     
C     LOOP TO COMPUTE ABSOLUTE VORTICITY FROM WINDS.
C     
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        ABSV(I,J) = D00
      ENDDO
      ENDDO
C
      CALL EXCH(UWND)
      CALL EXCH(F)
!$omp  parallel do
!$omp& private(favg,tphi,uavg)
      DO J=JSTA_M2,JEND_M2
        TPHI=(J-JMT)*DPHD*DTR
        DO I=2,IM-1
          UAVG=0.25*(UWND(I+IHE(J),J)+UWND(I+IHW(J),J)
     1              +UWND(I,J+1)+UWND(I,J-1))
          FAVG=0.25*(F(I+IHE(J),J)+F(I+IHW(J),J)
     1              +F(I,J+1)+F(I,J-1))
          ABSV(I,J)=((VWND(I+IHE(J),J)-VWND(I+IHW(J),J))/(2.*DX(I,J))
     1              -(UWND(I,J+1)-UWND(I,J-1))/(2.*DY)
     2              +UAVG*TAN(TPHI)/ERAD+2.*FAVG/DT)*HBM2(I,J)
        ENDDO
      ENDDO
C     
C     END OF ROUTINE.
C     
      RETURN
      END
