#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#define ICE 0x01

int main(void) {
  key_t key;
  int size, permflags, shm_id;
  float *iceptr;
  FILE *fin;

  size = 385*465*sizeof(float);
  key  = ICE;
  permflags = 0600 | IPC_EXCL;

  shm_id = shmget(key, size, permflags);
  printf("shm_id = %d\n",shm_id); fflush(stdout);
  iceptr = shmat(shm_id, (char *)0 , SHM_RDONLY);
  if (iceptr < 0) {
    printf("shmat failed, bad pointer \n");
    return -1;
  }

/* Note that we have done _nothing_ to read in a value to the array */
/* We are relying on the shared memory management to get values in to */
/*   the array for us.  All we're going to do is play with the values */
/*   that are there. */
  while (1==1) {
    printf("%f \n",iceptr[190 + 235*385 ] ); fflush(stdout);
    sleep (5);
  }
 
  return 0; 
}
