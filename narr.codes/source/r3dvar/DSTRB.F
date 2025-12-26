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
      INCLUDE "PARMETA.comm"
      include "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
      include "my_comm.h"
!#include "sp.h"
C---------------------------------------------------------------------------
      REAL ARRG(IM,JM,LG),ARRX(IM*JM*NSOIL)
     1,    ARRL(IDIM1:IDIM2,JDIM1:JDIM2,LL)
      INTEGER ISTAT(MPI_STATUS_SIZE)
      integer disp(0:npes-1)
      integer cnts(0:npes-1)
!     save arrx      
C---------------------------------------------------------------------------
        if(lg.gt.nsoil) then
          write(0,*)' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          write(0,*)' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          write(0,*)' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          write(0,*)' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          write(0,*)' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          write(0,*)' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          print *,' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          print *,' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          print *,' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          print *,' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          print *,' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          print *,' PROBLEM IN DSTRB--LG > NSOIL.  PROGRAM STOPPED'
          call mpi_abort(my_comm,100,ierr)
          stop
        end if
C
        IF ( MYPE .EQ. 0 ) THEN
C
        KNT=0
        DO IPE=0,NPES-1
          DO L=1,LG
             DO JGLB=JS_GLB_TABLE(IPE),JE_GLB_TABLE(IPE)
                DO IGLB=IS_GLB_TABLE(IPE),IE_GLB_TABLE(IPE)
                   KNT=KNT+1
                   ARRX(KNT)=ARRG(IGLB,JGLB,L)
                ENDDO
             ENDDO
          ENDDO
          cnts(ipe) =((JE_GLB_TABLE(IPE)-JS_GLB_TABLE(IPE)+1)*
     &                (IE_GLB_TABLE(IPE)-IS_GLB_TABLE(IPE)+1))*LG
         ENDDO
         disp(0)=0
         do ipe=1,npes-1
            disp(ipe) = disp(ipe-1)+cnts(ipe-1)
         end do
            
C
         END IF
        NUMVALS=(IE_GLB_TABLE(MYPE)-IS_GLB_TABLE(MYPE)+1)
     1         *(JE_GLB_TABLE(MYPE)-JS_GLB_TABLE(MYPE)+1)*LG
         call mpi_scatterv(arrx,cnts,disp,mpi_real,
     &   arrg,numvals,mpi_real,0,my_comm,irecv)
C
C***
C***  PE0 FILLS ITS OWN LOCAL DOMAIN THEN PARCELS OUT ALL THE OTHER PIECES
C***  TO THE OTHER PEs.
C***
c     IF(MYPE.EQ.0)THEN
C
c       IF(LG.EQ.1)THEN
c         DO JGLB=JS_GLB_TABLE(0),JE_GLB_TABLE(0)
c         LOCJ=G2LJ(JGLB)
c         DO IGLB=IS_GLB_TABLE(0),IE_GLB_TABLE(0)
c           LOCI=G2LI(IGLB)
c           ARRL(LOCI,LOCJ,L1)=ARRG(IGLB,JGLB,1)
c         ENDDO
c         ENDDO
c
c       ELSE
c
c         DO L=1,LG
c           DO JGLB=JS_GLB_TABLE(0),JE_GLB_TABLE(0)
c           LOCJ=G2LJ(JGLB)
c           DO IGLB=IS_GLB_TABLE(0),IE_GLB_TABLE(0)
c             LOCI=G2LI(IGLB)
c             ARRL(LOCI,LOCJ,L)=ARRG(IGLB,JGLB,L)
c           ENDDO
c           ENDDO
c         ENDDO
c       ENDIF
c
c       DO IPE=1,NPES-1
c         KNT=0
c
c         DO L=1,LG
c         DO JGLB=JS_GLB_TABLE(IPE),JE_GLB_TABLE(IPE)
c         DO IGLB=IS_GLB_TABLE(IPE),IE_GLB_TABLE(IPE)
c           KNT=KNT+1
c           ARRX(KNT,1,1)=ARRG(IGLB,JGLB,L)
c         ENDDO
c         ENDDO
c         ENDDO
c
c         CALL MPI_SEND(ARRX,KNT,MPI_REAL,IPE,IPE
c    1,                 MPI_COMM_world,ISEND)
c       ENDDO
C--------------------------------------------------------------------
C***
C***  ALL OTHER PEs RECEIVE THEIR PIECE FROM PE0 AND THEN FILL
C***  THEIR LOCAL ARRAY.
C***
c     ELSE
c       NUMVALS=(IE_GLB_TABLE(MYPE)-IS_GLB_TABLE(MYPE)+1)
c    1         *(JE_GLB_TABLE(MYPE)-JS_GLB_TABLE(MYPE)+1)*LG
c       CALL MPI_RECV(ARRX,NUMVALS,MPI_REAL,0,MYPE
c    1,               MPI_COMM_world,ISTAT,IRECV)
C
        KNT=0
        IF(LG.EQ.1)THEN
          DO J=MY_JS_LOC,MY_JE_LOC
          DO I=MY_IS_LOC,MY_IE_LOC
            KNT=KNT+1
            ARRL(I,J,L1)=ARRG(KNT,1,1)
          ENDDO
          ENDDO
        ELSE
          DO L=1,LG
          DO J=MY_JS_LOC,MY_JE_LOC
          DO I=MY_IS_LOC,MY_IE_LOC
            KNT=KNT+1
            ARRL(I,J,L)=ARRG(KNT,1,1)
          ENDDO
          ENDDO
          ENDDO
        ENDIF
C
c     ENDIF
C
c     CALL MPI_BARRIER(MPI_COMM_world,IRTN)
C
      RETURN
      END
