       SUBROUTINE refresh_grid(ARR,nx,ny,np,igrid)

!   update halo rows on analysis computational grid
!       patterned after EXCH.F from eta model

!   <--> arr:      array to have halo points updated
!    --> nx,ny,np: dimensions of arr
!    --> igrid:    which grid is being used
!                =1 -- fine grid
!                =2 -- coarse grid

         include 'types.h'
         INCLUDE "PARMETA.comm"
         INCLUDE "mpif.h"
      include "my_comm.h"
         INCLUDE "mpp.h"
         include "r3dv_data.comm"

         real arr(nx,ny,np)

         INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
         INTEGER IHANDLE(4)
         REAL(4),ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3

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
        NEBPE=MY_NEB(1)
        jhalo=ky10_loc(mype,igrid)-ky9_loc(mype,igrid)+1
        nxint=kx8_loc(mype,igrid)-kx3_loc(mype,igrid)+1
        ISIZE=nxint*jhalo*np
        ALLOCATE(BUF0(nxint,jhalo,np),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        jhalo=ky2_loc(mype,igrid)-ky1_loc(mype,igrid)+1
        nxint=kx8_loc(mype,igrid)-kx3_loc(mype,igrid)+1
        ISIZE=nxint*JHALO*np
        ALLOCATE(BUF1(nxint,jhalo,np),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C     
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        jhalo=ky8_loc(mype,igrid)-ky7_loc(mype,igrid)+1
        nxint=kx8_loc(mype,igrid)-kx3_loc(mype,igrid)+1
        ALLOCATE(BUF2(nxint,jhalo,np),STAT=I)
        KNT=nxint*jhalo*np
!$omp parallel do 
        DO K=1,np
         jj=0
         DO J=ky7_loc(mype,igrid),ky8_loc(mype,igrid)
          jj=jj+1
          ii=0
          DO I=kx3_loc(mype,igrid),kx8_loc(mype,igrid)
           ii=ii+1
           BUF2(Ii,jj,K)=ARR(I,J,K)
          ENDDO
         ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C    
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        jhalo=ky4_loc(mype,igrid)-ky3_loc(mype,igrid)+1
        nxint=kx8_loc(mype,igrid)-kx3_loc(mype,igrid)+1
        ALLOCATE(BUF3(nxint,jhalo,np),STAT=I)
        KNT=nxint*jhalo*np
!$omp parallel do 
        DO K=1,np
         jj=0
         DO J=ky3_loc(mype,igrid),ky4_loc(mype,igrid)
          jj=jj+1
          ii=0
          DO I=kx3_loc(mype,igrid),kx8_loc(mype,igrid)
           ii=ii+1
           BUF3(Ii,jj,K)=ARR(I,J,K)
          ENDDO
         ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(4),ISEND)
      ENDIF
C    
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do 
        DO K=1,np
         jj=0
         DO J=ky1_loc(mype,igrid),ky2_loc(mype,igrid)
          jj=jj+1
          ii=0
          DO I=kx3_loc(mype,igrid),kx8_loc(mype,igrid)
           ii=ii+1
           ARR(I,J,K)=BUF1(Ii,jj,K)
          ENDDO
         ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do 
        DO K=1,np
         jj=0
         DO J=ky9_loc(mype,igrid),ky10_loc(mype,igrid)
          jj=jj+1
          ii=0
          DO I=kx3_loc(mype,igrid),kx8_loc(mype,igrid)
           ii=ii+1
           ARR(I,J,K)=BUF0(Ii,jj,K)
          ENDDO
         ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
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
        NEBPE=MY_NEB(4)
        ihalo=kx2_loc(mype,igrid)-kx1_loc(mype,igrid)+1
        ISIZE=ihalo*ny*np
        ALLOCATE(BUF0(ihalo,ny,np),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        ihalo=kx10_loc(mype,igrid)-kx9_loc(mype,igrid)+1
        ISIZE=ihalo*ny*np
        ALLOCATE(BUF1(ihalo,ny,np),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               my_comm,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C      
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        ihalo=kx8_loc(mype,igrid)-kx7_loc(mype,igrid)+1
        KNT=ihalo*ny*np
        ALLOCATE(BUF2(ihalo,ny,np),STAT=I)
!$omp parallel do 
        DO K=1,np
         DO j=1,ny
          ii=0
          DO i=kx7_loc(mype,igrid),kx8_loc(mype,igrid)
           ii=ii+1
           BUF2(ii,J,K)=ARR(I,J,K)
          ENDDO
         ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C       
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        ihalo=kx4_loc(mype,igrid)-kx3_loc(mype,igrid)+1
        KNT=ihalo*ny*np
        ALLOCATE(BUF3(ihalo,ny,np),STAT=I)
!$omp parallel do 
        DO K=1,np
         DO j=1,ny
          ii=0
          do i=kx3_loc(mype,igrid),kx4_loc(mype,igrid)
           ii=ii+1
           BUF3(ii,J,K)=ARR(I,J,K)
          ENDDO
         ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               my_comm,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do 
        DO K=1,np
         DO J=1,ny
          ii=0
          DO I=kx1_loc(mype,igrid),kx2_loc(mype,igrid)
           ii=ii+1
           ARR(I,J,K)=BUF0(ii,J,K)
          ENDDO
         ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C    
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do 
        DO K=1,np
         DO J=1,ny
          ii=0
          DO I=kx9_loc(mype,igrid),kx10_loc(mype,igrid)
           ii=ii+1
           ARR(I,J,K)=BUF1(ii,J,K)
          ENDDO
         ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      RETURN
      END

