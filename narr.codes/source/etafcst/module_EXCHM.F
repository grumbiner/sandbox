      MODULE EXCHM
      CONTAINS
      SUBROUTINE EXCH0(ARR1,LL1,IHALO,JHALO)
C*************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    EXCHxxx     FUNDAMENTAL EXCHANGE ROUTINES
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 97-06-25
C
C ABSTRACT:
C     SUBROUTINE EXCH0 IS USED TO EXCHANGE HALOS BETWEEN PROCESSORS
C
C     CURRENTLY SUPPORTED INTERFACES
C
C     0,00,01,011,0001111
C     1,11,111,1111,11111,111111
C
C     WHERE 0 REFERS TO A 2-D ARRAY AND 1 REFERS TO A 3-D ARRAY
C
C PROGRAM HISTORY LOG:
C   97-05-??  MEYS       - ORIGINATOR
C   97-06-25  BLACK      - CONVERTED FROM SHMEM TO MPI
C   98-??-??  TUCCILLO   - REMOVED EXPLICIT EXCHANGES OF CORNERS
C   98-??-??  TUCCILLO   - REWROTE TO USE NON_BLOCKING MPI ROUTINES
C   99-??-??  BLACK      - ADDED VARIABLE HALO SIZES
C   00-03-10  TUCCILLO   - CHANGED TO USE MODULE PROCDURES FOR 
C                          INCREASED MESSAGE SIZES AND A UNIFORM
C                          INTERFACE FOR ALL CALLS
C   01-02-25  TUCCILLO   - SOME PERFORMANCE IMPROVEMENTS
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
      USE EXCH_BUF_REAL
C***********************************************************************
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO 
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE-IHALO+1
        IEND = MYIE
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS
        IEND = MYIS+IHALO-1
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE+1
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH1(ARR1,LL1,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO 
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE-IHALO+1
        IEND = MYIE
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS
        IEND = MYIS+IHALO-1
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIS-1
        IC = 0 
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE+1
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH01(ARR1,LL1,ARR2,LL2,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE-IHALO+1
        IEND = MYIE
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS
        IEND = MYIS+IHALO-1
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIS-1 
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE+1
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH00(ARR1,LL1,ARR2,LL2,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J)
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH11(ARR1,LL1,ARR2,LL2,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE-IHALO+1
        IEND = MYIE
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS
        IEND = MYIS+IHALO-1
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG = MYIS-IHALO
        IEND = MYIS-1 
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG = MYIE+1
        IEND = MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH111(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH1111(ARR1,LL1,ARR2,LL2,ARR3,LL3,
     *                     ARR4,LL4,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH11111(ARR1,LL1,ARR2,LL2,ARR3,LL3,
     *                      ARR4,LL4,ARR5,LL5,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR5(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR5(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR5(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH111111(ARR1,LL1,ARR2,LL2,ARR3,LL3,ARR4,LL4
     1,                     ARR5,LL5,ARR6,LL6,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR5(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR6(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR5(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR6(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR5(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR6(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH011(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE IEXCH(ARR1,LL1,IHALO,JHALO)
      USE EXCH_BUF_INTEGER
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      INTEGER ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
C
C***********************************************************************
C
      ITYPE=MPI_INTEGER
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE

      SUBROUTINE EXCH0001111(ARR1,LL1,ARR2,LL2,ARR3,LL3,
     *                        ARR4,LL4,ARR5,LL5,
     *                        ARR6,LL6,ARR7,LL7,IHALO,JHALO)
      USE EXCH_BUF_REAL
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR5(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR6(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR7(IDIM1:IDIM2,JDIM1:JDIM2,*)
C
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,MYJE-J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,MYJE-J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR5(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR6(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR7(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,MYJS+J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,MYJS+J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR5(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR6(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR7(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJS-J-1)=BUF1(IC)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR7(I,MYJS-J-1,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,MYJE+J+1)=BUF0(IC)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC = IC + 1
          ARR7(I,MYJE+J+1,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR2(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR3(I,J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF2(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        IC = 0
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR2(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR3(I,J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          BUF3(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        IC = 0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J)=BUF0(IC)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR7(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C   
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        IC = 0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR3(I,J)=BUF1(IC)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          IC = IC + 1
          ARR7(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      END MODULE
