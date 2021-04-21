#include <mpi.h>
#include <iostream>
#include <sstream>
#include <string>
#include <cstdlib>

// Desktop 25 May 2020

int main (int argc, char *argv[]) {
  int task_count, task_rank;
  MPI_Comm comm;

  printf("mpi_init result %d\n",MPI_Init (&argc, &argv)) ;

  MPI_Comm_rank (MPI_COMM_WORLD, &task_rank);
  MPI_Comm_size (MPI_COMM_WORLD, &task_count);

  std::ostringstream stream;
  stream << "Hello, world!  This is MPI process "
         << task_rank
         << " of "
         << task_count
         << ".\n";
  std::cout << stream.str ();

  MPI_Finalize ();

  return EXIT_SUCCESS;
}
