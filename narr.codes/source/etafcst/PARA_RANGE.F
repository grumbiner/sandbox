      SUBROUTINE PARA_RANGE (N1,N2,NPROCS,IRANK,ISTA,IEND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    PARA_RANGE  SET UP DECOMPOSITION VALUES
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     SETS UP DECOMOSITION VALUES
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C
C USAGE:    CALL COLLECT(A)
C   INPUT ARGUMENT LIST:
C     N1 - FIRST INTERATE VALUE
C     N2 - LAST INTERATE VALUE
C     NPROCS - NUMBER OF MPI TASKS
C     IRANK - MY TAKS ID
C
C   OUTPUT ARGUMENT LIST:
C     ISTA - FIRST LOOP VALUE
C     IEND - LAST LOOP VALUE
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$
      implicit none
      integer n1,n2,nprocs,irank,ista,iend
      integer iwork1, iwork2
      iwork1 = ( n2 - n1 + 1 ) / nprocs
      iwork2 = mod ( n2 - n1 + 1, nprocs )
      ista = irank * iwork1 + n1 + min ( irank, iwork2 )
      iend = ista + iwork1 - 1
      if ( iwork2 .gt. irank ) iend = iend + 1
      end

