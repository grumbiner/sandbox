#include <stdio.h>

#include <omp.h>

int main(void) {
 int nthreads, tid, omb_get_num_threads, omb_get_thread_num;

/*  Fork a team of threads giving them their own copies of variables */
#pragma omp parallel private(nthreads, tid)

/* Obtain thread number */
  tid = omp_get_thread_num();
  printf("hello from thread %d\n",tid);
  
/*  Only master thread does this */
  if (tid == 0) {
    nthreads = omp_get_num_threads();
    printf("number of threads = %d\n",nthreads);
  }

  return 0;
}
