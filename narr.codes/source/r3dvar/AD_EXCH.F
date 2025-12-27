C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                       SUBROUTINE AD_EXCH(ARR,LL,IHALO,JHALO)
C*************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    EXCH1       ADJOINT OF FUNDAMENTAL EXCHANGE ROUTINE
C   PRGRMMR: D.ZUPANSKI      ORG: W/NP2      DATE: 99-12-23
C
C ABSTRACT:
C     SUBROUTINE EXCH1 IS USED TO EXCHANGE A 1-ROW HALO BETWEEN PROCESSORS
C
C PROGRAM HISTORY LOG:
C   97-05-??  MEYS       - ORIGINATOR
C   97-06-25  BLACK      - CONVERTED FROM SHMEM TO MPI
C   98-??-??  TUCCILLO   - REMOVED EXPLICIT EXCHANGES OF CORNERS
C
C USAGE: CALL EXCH FROM SUBROUTINE GOSSIP
C   INPUT ARGUMENT LIST:
C       ARR - THE ARRAY TO BE EXCHANGED
C       LL  - THE VERTICAL DIMENSION OF ARR
C     IHALO - THE NUMBER OF POINTS IN THE X DIRECTION TO EXCHANGE
C             IN THE HALO
C     JHALO - THE NUMBER OF POINTS IN THE Y DIRECTION TO EXCHANGE
C             IN THE HALO
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
C   COMMON BLOCKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C***********************************************************************
      INCLUDE "PARMETA.comm"
      INCLUDE "mpif.h"
      include "my_comm.h"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR(IDIM1:IDIM2,JDIM1:JDIM2,LL)
!#ifdef TRAN32
!      REAL(4),ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
!#else
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
!#endif
C***********************************************************************
C
!#ifdef TRAN32
!      ITYPE=MPI_REAL4
!#else
      ITYPE=MPI_REAL
!#endif
C
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF2,KNT,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(3),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF3,KNT,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(4),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do 
        DO K=1,LL
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF0(I,J,K)=ARR(I,J,K)
          ARR(I,J,K)=0.0
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF0,ISIZE,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(1),ISEND)
CD!!    DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do 
        DO K=1,LL
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF1(I,J,K)=ARR(I,J,K)
          ARR(I,J,K)=0.0
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF1,KNT,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(2),ISEND)
CD!!    DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
!$omp parallel do 
        DO K=1,LL
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR(I,J,K)=ARR(I,J,K)+BUF2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
!$omp parallel do 
        DO K=1,LL
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR(I,J,K)=ARR(I,J,K)+BUF3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C   
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
        CALL MPI_IRECV(BUF2,KNT,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(3),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
        CALL MPI_IRECV(BUF3,KNT,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(4),IRECV)
      ENDIF
C    
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
!$omp parallel do 
        DO K=1,LL
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF0(I,J,K)=ARR(I,MYJE+J+1,K)
          ARR(I,MYJE+J+1,K)=0.0
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF0,ISIZE,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(1),ISEND)
      ENDIF
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
!$omp parallel do 
        DO K=1,LL
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF1(I,J,K)=ARR(I,MYJS-J-1,K)
          ARR(I,MYJS-J-1,K)=0.0
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF1,ISIZE,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(2),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
!$omp parallel do 
        DO K=1,LL
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR(I,MYJS+J,K)=ARR(I,MYJS+J,K)+BUF3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
!$omp parallel do 
        DO K=1,LL
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR(I,MYJE-J,K)=ARR(I,MYJE-J,K)+BUF2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
      RETURN
      END

