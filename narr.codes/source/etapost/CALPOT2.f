      SUBROUTINE CALPOT2(P1D,T1D,THETA,IM,JM)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALPOT2     COMPUTES POTENTIAL TEMPERATURE
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-24
C     
C ABSTRACT: 
C     GIVEN PRESSURE AND TEMPERATURE THIS ROUTINE RETURNS
C     THE POTENTIAL TEMPERATURE.
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-24  RUSS TREADON
C   98-06-15  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION            
C     
C USAGE:    CALL CALPOT2(P1D,T1D,THETA,IM,JM)
C   INPUT ARGUMENT LIST:
C     P1D      - PRESSURE (PA)
C     T1D      - TEMPERATURE (K)
C     IM,JM    - DIMENSIONS OF ARRAYS.
C
C   OUTPUT ARGUMENT LIST: 
C     THETA    - POTENTIAL TEMPERATURE (K)
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
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     SET REQUIRED CONSTANTS.
      PARAMETER (CAPA=0.28589641,P1000=1000.E2)
C
      INCLUDE "CTLBLK.comm"
C
C     DECLARE VARIABLES.
C     
      REAL FAC,P1D(IM,JM),T1D(IM,JM),THETA(IM,JM)
C     
C**********************************************************************
C     START CALPOT2 HERE.
C     
C     COMPUTE THETA
C     
      DO J=JSTA,JEND
      DO I=1,IM
        IF(ABS(P1D(I,J)).GT.1)THEN
          FAC=(P1000/P1D(I,J))**CAPA
          THETA(I,J)=FAC*T1D(I,J)
        ELSE
          THETA(I,J)=0.0
        ENDIF
      ENDDO
      ENDDO
c     do j = 180, 185
c        print *, ' me, j, p1d,t1d,theta = ',
c    *   me, j, p1d(10,j),t1d(10,j),theta (10,j)
c     end do
c       stop
C     
C     END OF ROUTINE.
C     
      RETURN
      END
