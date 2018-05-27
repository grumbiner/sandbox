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
  permflags = 0600 | IPC_CREAT | IPC_EXCL;

  shm_id = shmget(key, size, permflags);
  printf("shm_id = %d\n",shm_id);
  iceptr = shmat(shm_id, (char *)0 , SHM_RND);

  fin = fopen("nh","r");
  if (fin == NULL) { 
    printf("failed to open fin\n");
    return -1;
  }
  printf("about to fread\n"); fflush(stdout); 
  fread(iceptr, sizeof(float), 385*465, fin);
  printf("finished fread\n"); fflush(stdout); 
  printf("%f \n",iceptr[190 + 235*385 ] ); fflush(stdout);

  while (1==1) {
    iceptr[190 + 235*385] += 0.01;
    sleep(1);
  }
 
  return 0; 
}
