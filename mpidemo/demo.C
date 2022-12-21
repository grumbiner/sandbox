//required MPI include file  
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[]) {

// initialize MPI  
  int  numtasks, rank, len, rc; 
  char hostname[MPI_MAX_PROCESSOR_NAME];
  MPI_Request requests;
  MPI_Status  status;

  MPI_Comm comm;

MPI_Init(&argc,&argv);

// get number of tasks 
MPI_Comm_size(MPI_COMM_WORLD,&numtasks);

// get my rank  
MPI_Comm_rank(MPI_COMM_WORLD,&rank);

// this one is obvious  
MPI_Get_processor_name(hostname, &len);
printf ("Number of tasks= %d My rank= %d Running on %s\n", numtasks,rank,hostname);
//printf("max processor name %d\n",MPI_MAX_PROCESSOR_NAME);
//printf("hostname %s\n",hostname);


// do some work with message passing 
// send to processor n+1, receive from n-1

  int count = 1;
  int mess = rank, psendp, precvp, itag1, back = -1;
  psendp = (rank + 1) % numtasks;
  precvp = (rank - 1) ; 
  if (precvp < 0) precvp = numtasks - 1;

  printf("on proc %d mess = %d and back = %d send = %d recv = %d\n",
           rank, mess, back, psendp, precvp);
  fflush(stdout);

  MPI_Isend(&mess, 1, MPI_INT, psendp, itag1, MPI_COMM_WORLD, &requests);
  MPI_Recv(&back,  1, MPI_INT, precvp, itag1, MPI_COMM_WORLD, &status);
  // see also Irecv
  MPI_Waitall(count, &requests, MPI_STATUSES_IGNORE);
  printf("processor %d got message %d\n",rank, back);


// done with MPI  
MPI_Finalize();
}
