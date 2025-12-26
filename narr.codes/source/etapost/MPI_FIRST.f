      SUBROUTINE MPI_FIRST
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    MPI_FIRST   SET UP MESSGAE PASSING INFO
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     SETS UP MESSAGE PASSING INFO
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C
C USAGE:    CALL MPI_FIRST
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       MPI_INIT
C       MPI_COMM_SIZE
C       MPI_COMM_RANK
C       PARA_RANGE
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$
c
      include "CTLBLK.comm"
      include "parmeta"
      include 'mpif.h'
c
      integer ierr
c
      call mpi_init(ierr)
      call mpi_comm_size(MPI_COMM_WORLD,NUM_PROCS, ierr )
      call mpi_comm_rank(MPI_COMM_WORLD,ME, ierr )
c
      if ( me .eq. 0 ) then
c        print *, ' NUM_PROCS = ',num_procs
      end if

      if ( num_procs .gt. 1024 ) then
         print *, ' too many MPI tasks, max is 1024, stopping'
         call mpi_last
         stop
      end if
c
c     error check
c
      if ( num_procs .gt. JM/2 ) then
         print *, ' too many MPI tasks, max is ',jm/2,' stopping'
         call mpi_last
         stop
      end if
c
c     global loop ranges
c
      call para_range(1,jm,num_procs,me,jsta,jend)
      jsta_m  = jsta
      jsta_m2 = jsta
      jend_m  = jend
      jend_m2 = jend
      if ( me .eq. 0 ) then
         jsta_m  = 2
         jsta_m2 = 3
      end if
      if ( me .eq. num_procs - 1 ) then
         jend_m  = jm - 1
         jend_m2 = jm - 2
      end if
c
c     neighbors
c
      iup = me + 1
      idn = me - 1
      if ( me .eq. 0 ) then
         idn = MPI_PROC_NULL
      end if
      if ( me .eq. num_procs - 1 ) then
         iup = MPI_PROC_NULL
      end if
C
c     print *, ' ME, NUM_PROCS = ',me,num_procs
c     print *, ' ME, JSTA, JSTA_M, JSTA_M2 = ',me,jsta,jsta_m,jsta_m2
c     print *, ' ME, JEND, JEND_M, JEND_M2 = ',me,jend,jend_m,jend_m2
c     print *, ' ME, IUP, IDN = ',me,iup,idn
c
c     counts, disps for gatherv and scatterv
c
      do i = 0, num_procs - 1
         call para_range(1,jm,num_procs,i,jsx,jex) 
         icnt(i) = (jex-jsx+1)*im
         idsp(i) = (jsx-1)*im
         if ( me .eq. 0 ) then
c           print *, ' i, icnt(i),idsp(i) = ',i,icnt(i),idsp(i)
         end if
      end do
c
c     extraction limits    
c
      jsta_2l = max(jsta - 1,  1 )
      jend_2u = min(jend + 1, jm )
c     print *, ' me, jsta_2l, jend_2u = ',me,jsta_2l, jend_2u
c
      end
