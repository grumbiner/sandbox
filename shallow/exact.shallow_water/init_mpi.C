#include <stdio.h>
#include <mpi.h>

int init_mpi(int &myrank, int &nprocs, float *sendp, float *sendm, float *recvp, float *recvm, MPI_Request *requests, int &nx) {
// MPI related:
  int retcode, argc = 0;
  char **argv;

  retcode = MPI_Init(&argc, &argv);
  MPI_Comm_size (MPI_COMM_WORLD, &nprocs);
  MPI_Comm_rank (MPI_COMM_WORLD, &myrank);
  printf("process %5d of %5d\n",myrank, nprocs);

// Comm buffers:
  int psendp = myrank + 1, psendm = myrank - 1; // processors to send to
  int precvp = myrank + 1, precvm = myrank - 1; // processors to receive from
  int itag1 = 0;

  //MPI_Request requests[4];
// Note: 3*nx because we're passing eta, u, v (in that order), rather than just eta
   sendp = new float[nx*3];
   sendm = new float[nx*3];
   recvm = new float[nx*3];
   recvp = new float[nx*3];

  return retcode;
}
