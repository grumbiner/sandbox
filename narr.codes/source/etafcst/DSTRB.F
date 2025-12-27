C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                     SUBROUTINE DSTRB(ARRG,ARRL,LG,LL,L1)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    DSTRB       DISTRIBUTE GLOBAL ARRAY TO LOCAL ARRAYS
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 97-08-29
C
C ABSTRACT:
C     DSTRB DISTRIBUTES THE ELEMENTS OF REAL GLOBAL ARRAY ARRG TO THE
C     REAL LOCAL ARRAYS ARRL.  LG IS THE VERTICAL DIMENSION OF THE
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
C     LG   - VERTICAL DIMENSION OF GLOBAL ARRAY
C     LL   - VERTICAL DIMENSION OF LOCAL ARRAY
C     L1   - VERTICAL LEVEL OF ARRL BEING FILLED IN THIS CALL
C            (USED ONLY WHEN LG=1 AND LL>1, I.E. WHEN THE GLOBAL
C             ARRAY IS ACTUALLY JUST ONE LEVEL OF A MULTI_LEVEL
C             ARRAY)
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
      REAL ARRG(IM,JM,LG),ARRX(IM,JM,LG)
     1,    ARRL(IDIM1:IDIM2,JDIM1:JDIM2,LL)
      INTEGER ISTAT(MPI_STATUS_SIZE)
C---------------------------------------------------------------------------
C***
C***  PE0 FILLS ITS OWN LOCAL DOMAIN THEN PARCELS OUT ALL THE OTHER PIECES
C***  TO THE OTHER PEs.
C***
      IF(MYPE.EQ.0)THEN
C
        IF(LG.EQ.1)THEN
          DO JGLB=JS_GLB_TABLE(0),JE_GLB_TABLE(0)
          LOCJ=G2LJ(JGLB)
          DO IGLB=IS_GLB_TABLE(0),IE_GLB_TABLE(0)
            LOCI=G2LI(IGLB)
            ARRL(LOCI,LOCJ,L1)=ARRG(IGLB,JGLB,1)
          ENDDO
          ENDDO
C
        ELSE
C
          DO L=1,LG
            DO JGLB=JS_GLB_TABLE(0),JE_GLB_TABLE(0)
            LOCJ=G2LJ(JGLB)
            DO IGLB=IS_GLB_TABLE(0),IE_GLB_TABLE(0)
              LOCI=G2LI(IGLB)
              ARRL(LOCI,LOCJ,L)=ARRG(IGLB,JGLB,L)
            ENDDO
            ENDDO
          ENDDO
        ENDIF
C
        DO IPE=1,NPES-1
          KNT=0
C
          DO L=1,LG
          DO JGLB=JS_GLB_TABLE(IPE),JE_GLB_TABLE(IPE)
          DO IGLB=IS_GLB_TABLE(IPE),IE_GLB_TABLE(IPE)
            KNT=KNT+1
            ARRX(KNT,1,1)=ARRG(IGLB,JGLB,L)
          ENDDO
          ENDDO
          ENDDO
C
          CALL MPI_SEND(ARRX,KNT,MPI_REAL,IPE,IPE
     1,                 MPI_COMM_COMP,ISEND)
        ENDDO
C--------------------------------------------------------------------
C***
C***  ALL OTHER PEs RECEIVE THEIR PIECE FROM PE0 AND THEN FILL
C***  THEIR LOCAL ARRAY.
C***
      ELSE
        NUMVALS=(IE_GLB_TABLE(MYPE)-IS_GLB_TABLE(MYPE)+1)
     1         *(JE_GLB_TABLE(MYPE)-JS_GLB_TABLE(MYPE)+1)*LG
        CALL MPI_RECV(ARRX,NUMVALS,MPI_REAL,0,MYPE
     1,               MPI_COMM_COMP,ISTAT,IRECV)
C
        KNT=0
        IF(LG.EQ.1)THEN
          DO J=MY_JS_LOC,MY_JE_LOC
          DO I=MY_IS_LOC,MY_IE_LOC
            KNT=KNT+1
            ARRL(I,J,L1)=ARRX(KNT,1,1)
          ENDDO
          ENDDO
        ELSE
          DO L=1,LG
          DO J=MY_JS_LOC,MY_JE_LOC
          DO I=MY_IS_LOC,MY_IE_LOC
            KNT=KNT+1
            ARRL(I,J,L)=ARRX(KNT,1,1)
          ENDDO
          ENDDO
          ENDDO
        ENDIF
C
      ENDIF
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
      RETURN
      END
