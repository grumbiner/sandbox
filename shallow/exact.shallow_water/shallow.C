#include "grid_math.h"
#include <mpi.h>
using namespace std;

// started long ago
// Update for mpi version of 25 May 2020 

// Something like lake michigan
#define dx  (1000./(float) SCALE)
#define dy  (1000./(float) SCALE)
#define dt  (20./(float) SCALE)

#define NX (125.e3 / dx )
#define NY (450.e3 / dy )
#define gee 9.8
#define NSTEP 2500
#define freq   500

#define DTYPE float

void step(int nbase, grid2<DTYPE> &u, grid2<DTYPE> &v, grid2<DTYPE> *eta, 
          grid2<DTYPE> &tmpeta, grid2<DTYPE> &h);
#include "step.C"

void init_fields(int nx, int ny, grid2<DTYPE> &h, grid2<DTYPE> &u,
                 grid2<DTYPE> &v, grid2<DTYPE> *eta, grid2<DTYPE> &tmpeta,
                 int &myrank, int &global_nx, int &global_ny) ;
#include "init_fields.C"


//------------------------------------------------------------------------------
int main(int argc, char *argv[]) {
  grid2<DTYPE> u, v, h, eta[3], tmpeta;
  int nx, ny, global_nx, global_ny;

  grid2<DTYPE> tmp;
  int i, j, nbase, np;
  ijpt loc, center;
// if argument, then run for spec'd number of steps:
  int nstep = NSTEP;

// MPI related  ---------------------------------------------
  int myrank, nprocs;
  MPI_Request requests[4];
  // Comm buffers:
  float *sendp, *sendm, *recvp, *recvm;

  MPI_Init(&argc, &argv);
  MPI_Comm_size (MPI_COMM_WORLD, &nprocs);
  MPI_Comm_rank (MPI_COMM_WORLD, &myrank);
  printf("process %5d of %5d\n",myrank, nprocs);

  int psendp = myrank + 1, psendm = myrank - 1; // processors to send to 
  int precvp = myrank + 1, precvm = myrank - 1; // processors to receive from
  int itag1 = 0;

  // Note: 3*nx because we're passing eta, u, v (in that order), rather than just eta
  sendp = new float[nx*3];
  sendm = new float[nx*3];
  recvm = new float[nx*3];
  recvp = new float[nx*3];

// ------------------------------------
  if (argc == 2) nstep = atoi(argv[1]);

  global_nx = (int) (0.5 + NX);
  global_ny = (int) (0.5 + NY);
  if (global_ny%nprocs != 0) global_ny += nprocs - global_ny%nprocs ;
  printf("run length is %f seconds, %f minutes\n",nstep*dt, nstep*dt/60 );
  printf("global_nx, global_ny = %f %f rounded %d %d\n",NX, NY, global_nx, global_ny);

// Here we would figure out where we are in the stencil and parcel out the
//   correct sizes for each processor.  For now, blindly pass out local=global
  nx = global_nx;
  if (nprocs == 1) {
    ny = global_ny;
  }
  else if (nprocs >= 2) {
    ny = global_ny/nprocs ;
  }
  else {
    printf("nprocs not greater or equal to 1\n");
    return 1;
  }
  printf("processor %5d has grid %d by %d vs globals of %d by %d\n",myrank, nx, ny, global_nx, global_ny);

// Now on with the universalities ------------------------------------
  init_fields(nx, ny, h, u, v, eta, tmpeta, myrank, global_nx, global_ny);


// Main loop ---------------------------------------------------------
  ijpt sloc, rloc; // indices
  char fname[900];
  palette<unsigned char> gg(19, 65);
  for (i = 0; i <= nstep; i++) {
    nbase = (i+2)%3;
    np = (nbase + 1)%3;
    step(nbase, u, v, eta, tmpeta, h);

    // Communications in here
    if (nprocs == 1 ) { 
       // do nothing
    }
    else {
      if (myrank == 0 ) {
        // swap info with processor myrank + 1
        sloc.j = ny - 2;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          sendp[sloc.i] = eta[np][sloc];
          sendp[sloc.i+nx] = u[sloc];
          sendp[sloc.i+2*nx] = v[sloc];
        }
        MPI_Isend(sendp, 3*nx, MPI_FLOAT, psendp, itag1, MPI_COMM_WORLD, &requests[0]);
        MPI_Irecv(recvp, 3*nx, MPI_FLOAT, precvp, itag1, MPI_COMM_WORLD, &requests[1]);

        MPI_Waitall(1, &requests[0], MPI_STATUSES_IGNORE);
        MPI_Waitall(1, &requests[1], MPI_STATUSES_IGNORE);
        
        sloc.j = ny - 1;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          eta[np][sloc] = recvp[sloc.i]; 
          u[sloc]       = recvp[sloc.i+nx];
          v[sloc]       = recvp[sloc.i+2*nx];
        }
      }
      else if (myrank == (nprocs - 1 ) ) {
        // swap info with processor myrank - 1
        sloc.j = 0;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          sendm[sloc.i] = eta[np][sloc];
          sendm[sloc.i+nx] = u[sloc];
          sendm[sloc.i+nx*2] = v[sloc];
        }
        MPI_Irecv(recvm, 3*nx, MPI_FLOAT, precvm, itag1, MPI_COMM_WORLD, &requests[0]);
        MPI_Isend(sendm, 3*nx, MPI_FLOAT, psendm, itag1, MPI_COMM_WORLD, &requests[1]);
        MPI_Waitall(1, &requests[0], MPI_STATUSES_IGNORE);
        MPI_Waitall(1, &requests[1], MPI_STATUSES_IGNORE);

        sloc.j = 1;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          eta[np][sloc] = recvm[sloc.i];
          u[sloc]       = recvm[sloc.i+nx];
          v[sloc]       = recvm[sloc.i+nx*2];
        }
      }
      else {
        // swap info with processors myrank + 1 and - 1
        // note: not doing all sends/receives and then waiting.  This may be less efficient
        // extract data to send:
        sloc.j = ny - 2;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          sendp[sloc.i]      = eta[np][sloc];
          sendp[sloc.i+nx]   = u[sloc];
          sendp[sloc.i+nx*2] = v[sloc];
        }
        sloc.j = 0;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          sendm[sloc.i]      = eta[np][sloc];
          sendm[sloc.i+nx]   = u[sloc];
          sendm[sloc.i+nx*2] = v[sloc];
        }

        // swap info with processor myrank + 1
        MPI_Isend(sendp, 3*nx, MPI_FLOAT, psendp, itag1, MPI_COMM_WORLD, &requests[0]);
        MPI_Irecv(recvp, 3*nx, MPI_FLOAT, precvp, itag1, MPI_COMM_WORLD, &requests[1]);

        // swap with processor myrank - 1
        MPI_Irecv(recvm, 3*nx, MPI_FLOAT, precvm, itag1, MPI_COMM_WORLD, &requests[2]);
        MPI_Isend(sendm, 3*nx, MPI_FLOAT, psendm, itag1, MPI_COMM_WORLD, &requests[3]);

        MPI_Waitall(1, &requests[0], MPI_STATUSES_IGNORE);
        MPI_Waitall(1, &requests[1], MPI_STATUSES_IGNORE);
        MPI_Waitall(1, &requests[2], MPI_STATUSES_IGNORE);
        MPI_Waitall(1, &requests[3], MPI_STATUSES_IGNORE);

        // Insert received data:
        sloc.j = ny - 1;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          eta[np][sloc] = recvp[sloc.i];
          u[sloc]       = recvp[sloc.i+nx];
          v[sloc]       = recvp[sloc.i+nx*2];
        }
        sloc.j = 1;
        for (sloc.i = 0; sloc.i < nx; sloc.i ++) {
          eta[np][sloc] = recvm[sloc.i];
          u[sloc]       = recvm[sloc.i+nx];
          v[sloc]       = recvm[sloc.i+nx*2];
        }

      }

    }
// Done communicating / time step ------------------------------------

// Each processor prints out its view of the world:
    if (i >= 0 && (i % freq) == 0) {
      // now start to produce graphics
      tmp = eta[nbase];
      tmp.scale();
      printf("proc %5d step %d eta max, min, avg, rms %f %f %e %e\n",
                 myrank,i, 
                eta[nbase].gridmax(), eta[nbase].gridmin(), eta[nbase].average(), eta[nbase].rms() );
      fflush(stdout);
      sprintf(fname,"eta%05d%05d.xpm",i,myrank);
      tmp.xpm(fname, 7, gg); 
    }
    
  }


  MPI_Finalize();
  return 0;
}
