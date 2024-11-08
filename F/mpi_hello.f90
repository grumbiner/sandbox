PROGRAM hello
  IMPLICIT none
  INCLUDE "mpif.h"

  INTEGER              :: I, MPI_COMM = -99
  INTEGER, ALLOCATABLE :: TEND(:,:)
  INTEGER              :: IERR_MPI, NMPROC, IMPROC


  CALL MPI_INIT(IERR_MPI)
  MPI_COMM = MPI_COMM_WORLD
  CALL MPI_COMM_SIZE ( MPI_COMM, NMPROC, IERR_MPI )
  CALL MPI_COMM_RANK ( MPI_COMM, IMPROC, IERR_MPI )

  PRINT *,'comm size = ',NMPROC
  PRINT *,' rank = ',IMPROC

  CALL MPI_FINALIZE(IERR_MPI)

  END
