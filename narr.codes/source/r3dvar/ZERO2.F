c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        SUBROUTINE ZERO2(ARRAY)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    ZERO2       ZERO OUT 2-D ARRAYS
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 96-03-28       
C     
C ABSTRACT:
C     SET THE VALUES OF THE ARTIFICIAL EXTERNAL (OUT-OF-BOUNDS) EDGES
C     TO ZERO
C
C PROGRAM HISTORY LOG:
C   96-03-28  BLACK      - ORIGINATOR
C   97-06-??  MEYS       - MODIFIED FOR DISTRIBUTED MEMORY
C   99-07-06  BLACK      - FULL ARRAY RATHER THAN JUST EDGES
C     
C USAGE: CALL ZERO2 FROM ANY ROUTINE NEEDING THIS PROCEDURE
C   INPUT ARGUMENT LIST:
C       THE DUMMY ARRAY NAME
C  
C   OUTPUT ARGUMENT LIST: 
C     THE DUMMY ARRAY NAME
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
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
C-----------------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & ARRAY(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DO J=JDIM1,JDIM2
      DO I=IDIM1,IDIM2
        ARRAY(I,J)=0.
      ENDDO
      ENDDO
C----------------------------------------------------------------
C----------------------------------------------------------------
      RETURN
      END
