      SUBROUTINE DIST ( A ) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    DIST        SCATTERS FROM TASK 0        
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     SCATTER FROM TASK 0 TO ALL OTHER TASKS    
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C
C USAGE:    CALL DIST(A)
C   INPUT ARGUMENT LIST:
C     A        - ARRAY BEING SCATTERED - VALID ONLY ON TASK 0
C
C   OUTPUT ARGUMENT LIST:
C     A        - SCATTERD ARRAY 
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       MPI_SCATTERV
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$

      include "parmeta"
      include "CTLBLK.comm"
      include 'mpif.h'
      real a ( im, jm ) 
      real b ( im, jsta:jend ) 
      integer i, j
      integer ierr
c
      if ( num_procs .eq. 1 ) return
      call mpi_scatterv(a,icnt,idsp,MPI_REAL,
     &    b(1,jsta),icnt(me),MPI_REAL,0,MPI_COMM_WORLD, ierr )
       do j = jsta,jend
          do i = 1, im
             a ( i, j ) = b ( i, j )
          end do
       end do
      end               
