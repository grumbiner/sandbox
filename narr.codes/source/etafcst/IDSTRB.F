C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                     SUBROUTINE IDSTRB(ARRG,ARRL)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    IDSTRB      DISTRIBUTE INTEGER GLOBAL ARRAY TO LOCAL ONES
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 97-08-29
C
C ABSTRACT:
C     IDSTRB DISTRIBUTES THE ELEMENTS OF INTEGER GLOBAL ARRAY ARRG TO 
C     THE INTEGER LOCAL ARRAYS ARRL.  LG IS THE VERTICAL DIMENSION OF THE
C     GLOBAL ARRAY.  LL IS THE VERTICAL DIMENSION OF THE LOCAL ARRAY.
C     L1 IS THE SPECIFIC LEVEL OF ARRL THAT IS BEING FILLED DURING
C     THIS CALL (PERTINENT WHEN LG=1 AND LL>1).
C
C PROGRAM HISTORY LOG:
C   97-08-29  BLACK      - ORIGINATOR
C
C USAGE: CALL READ_NFCST FROM SUBROUTINE INIT
C   INPUT ARGUMENT LIST:
C     ARRG - GLOBAL ARRAY
C
C   OUTPUT ARGUMENT LIST:
C     ARRL - LOCAL ARRAY
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE: NONE
C
C     LIBRARY: NONE
C
C   COMMON BLOCKS: NONE
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$
C---------------------------------------------------------------------------
C***  DISTRIBUTE ARRAYS FROM GLOBAL TO LOCAL
C---------------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
#include "sp.h"
C---------------------------------------------------------------------------
      INTEGER ARRG(IM,JM),ARRX(IM,JM)
     1,       ARRL(IDIM1:IDIM2,JDIM1:JDIM2)
      INTEGER ISTAT(MPI_STATUS_SIZE)
C---------------------------------------------------------------------------
C***
C***  PE0 FILLS ITS OWN LOCAL DOMAIN THEN PARCELS OUT ALL THE OTHER PIECES
C***  TO THE OTHER PEs.
C***
      IF(MYPE.EQ.0)THEN
C
          DO JGLB=JS_GLB_TABLE(0),JE_GLB_TABLE(0)
            LOCJ=G2LJ(JGLB)
            DO IGLB=IS_GLB_TABLE(0),IE_GLB_TABLE(0)
              LOCI=G2LI(IGLB)
              ARRL(LOCI,LOCJ)=ARRG(IGLB,JGLB)
            ENDDO
          ENDDO
C
        DO IPE=1,NPES-1
          KNT=0
C
          DO JGLB=JS_GLB_TABLE(IPE),JE_GLB_TABLE(IPE)
          DO IGLB=IS_GLB_TABLE(IPE),IE_GLB_TABLE(IPE)
            KNT=KNT+1
            ARRX(KNT,1)=ARRG(IGLB,JGLB)
          ENDDO
          ENDDO
C
          CALL MPI_SEND(ARRX,KNT,MPI_INTEGER,IPE,IPE
     1,                 MPI_COMM_COMP,ISEND)
        ENDDO
C--------------------------------------------------------------------
C***
C***  ALL OTHER PEs RECEIVE THEIR PIECE FROM PE0 AND THEN FILL
C***  THEIR LOCAL ARRAY.
C***
      ELSE
        NUMVALS=(IE_GLB_TABLE(MYPE)-IS_GLB_TABLE(MYPE)+1)
     1         *(JE_GLB_TABLE(MYPE)-JS_GLB_TABLE(MYPE)+1)
        CALL MPI_RECV(ARRX,NUMVALS,MPI_INTEGER,0,MYPE
     1,               MPI_COMM_COMP,ISTAT,IRECV)
C
        KNT=0
        DO J=MY_JS_LOC,MY_JE_LOC
        DO I=MY_IS_LOC,MY_IE_LOC
          KNT=KNT+1
          ARRL(I,J)=ARRX(KNT,1)
        ENDDO
        ENDDO
C
      ENDIF
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
      RETURN
      END
