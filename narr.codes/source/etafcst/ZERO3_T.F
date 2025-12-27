c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        SUBROUTINE ZERO3_T(ARRAY,LL)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    ZERO3_T     ZERO OUT TRANSPOSED 3-D ARRAYS
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 99-07-06       
C     
C ABSTRACT:
C     SET THE VALUES OF THE ARTIFICIAL EXTERNAL (OUT-OF-BOUNDS) EDGES
C     TO ZERO
C
C PROGRAM HISTORY LOG:
C   99-07-06  BLACK      - ORIGINATOR
C     
C USAGE: CALL ZERO3_T FROM ANY ROUTINE NEEDING THIS PROCEDURE
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
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & ARRAY(LL,IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DO J=JDIM1,JDIM2
      DO I=IDIM1,IDIM2
        DO L=1,LL
          ARRAY(L,I,J)=0.
        ENDDO
      ENDDO
      ENDDO
C----------------------------------------------------------------
C----------------------------------------------------------------
      RETURN
      END
