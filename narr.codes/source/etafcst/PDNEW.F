C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE PDNEW
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    PDNEW       UPDATE SURFACE PRESSURE
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08       
C     
C ABSTRACT:
C     PDNEW UPDATES THE SURFACE PRESSURE FROM THE TENDENCY
C     COMPUTED IN PDTE.
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL PDNEW FROM MAIN PROGRAM EBU
C
C   INPUT ARGUMENT LIST:
C       NONE     
C  
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE: NONE
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  VRBLS
C                  CONTIN
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
C--------------UPDATING PRESSURE DIFFERENCE-----------------------------
C-----------------------------------------------------------------------
!OMP parallel do 
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        PD(I,J)=PSDT(I,J)*DT+PD(I,J)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
                             RETURN
                             END
