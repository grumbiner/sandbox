      SUBROUTINE EXCH2(A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    EXCH2       EXCHANGE TWO HALO ROWS
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     EXCHANGE TWO HALO ROWS
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C
C USAGE:    CALL EXCH2(A)
C   INPUT ARGUMENT LIST:
C      A - ARRAY TO HAVE HALOS EXCHANGED
C
C   OUTPUT ARGUMENT LIST:
C      A - ARRAY WITH HALOS EXCHANGED
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       MPI_SENDRECV
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$
      include "parmeta"
      include "CTLBLK.comm"
      include 'mpif.h'
      real a ( im, jm )
      integer status(MPI_STATUS_SIZE)
c
      if ( num_procs .eq. 1 ) return
C
      call mpi_sendrecv(a(1,jend-1),2*im,MPI_REAL,iup,1,
     &                  a(1,jsta-2),2*im,MPI_REAL,idn,1,
     &                  MPI_COMM_WORLD,status,ierr)
      if ( ierr .ne. 0 ) then
         print *, ' problem with first sendrecv in exch2, ierr = ',ierr
         stop
      end if
      call mpi_sendrecv(a(1,jsta  ),2*im,MPI_REAL,idn,1,
     &                  a(1,jend+1),2*im,MPI_REAL,iup,1,
     &                  MPI_COMM_WORLD,status,ierr)
      if ( ierr .ne. 0 ) then
         print *, ' problem with second sendrecv in exch2, ierr = ',ierr
         stop
      end if
c
      end
