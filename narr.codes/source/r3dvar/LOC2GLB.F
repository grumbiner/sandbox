      SUBROUTINE LOC2GLB(ARRL,ARRG)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                               .
C SUBPROGRAM:    LOC2GLB     REATE GLOBAL ARRAYS
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 97-10-28
C
C ABSTRACT:
C     LOC2GLB CREATES A SINGLE GLOBAL ARRAY FROM MANY LOCAL ONES
C
C PROGRAM HISTORY LOG:
C   97-10-28  BLACK      - ORIGINATOR
C
C USAGE: CALL LOC2GLB FROM SUBROUTINE CHKOUT
C
C   INPUT ARGUMENT LIST:
C       ARRL   - THE LOCAL ARRAY
C
C   OUTPUT ARGUMENT LIST:
C       ARRG   - THE GLOBAL ARRAYS
C
C   INPUT FILES:
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
C   COMMON BLOCKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C************************************************************
C------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
      include "my_comm.h"
!#include "sp.h"
C------------------------------------------------------------
      REAL ARRL(IDIM1:IDIM2,JDIM1:JDIM2),ARRG(IM,JM)
      REAL ARRX(IDIM1:IDIM2,JDIM1:JDIM2)
C------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
C------------------------------------------------------------
      NUMVAL=(IDIM2-IDIM1+1)*(JDIM2-JDIM1+1)
C
      IF(MYPE.NE.0)THEN
        CALL MPI_SEND(ARRL,NUMVAL,MPI_REAL,0,MYPE
     1,               my_comm,ISEND)
C
      ELSE
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          ARRG(I+MY_IS_GLB-1,J+MY_JS_GLB-1)=ARRL(I,J)
        ENDDO
        ENDDO
C
        DO IPE=1,NPES-1
          CALL MPI_RECV(ARRX,NUMVAL,MPI_REAL,IPE,IPE
     1,                 my_comm,ISTAT,IRECV)
C
          JKNT=0
          DO J=JS_LOC_TABLE(IPE),JE_LOC_TABLE(IPE)
            JGLB=JS_GLB_TABLE(IPE)+JKNT
C
            IKNT=0
            DO I=IS_LOC_TABLE(IPE),IE_LOC_TABLE(IPE)
              IGLB=IS_GLB_TABLE(IPE)+IKNT
              ARRG(IGLB,JGLB)=ARRX(I,J)
              IKNT=IKNT+1
            ENDDO
            JKNT=JKNT+1
          ENDDO
C
        ENDDO
C
      ENDIF
C
      RETURN
      END
