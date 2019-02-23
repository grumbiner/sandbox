#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

int BAOPEN_RONLY = 1;
int BAOPEN_WONLY = 2;
int BAOPEN_RW    = 4;
int BACLOSE      = 8;
int BAREAD      = 16;
int BAWRITE     = 32;

#define NITER 100

#include <math.h>
#define totlen (1024 * 1 )
int main(void) {
   int mode, start, size, no, nactual, fdes, namelen, datanamelen;
   char fname[800], datary[1024 * 1024];

   int sizewise, i, j, k;
   int tstart;

   no = 1;
     for (j = 0; j < 16; j++) {
       sizewise = pow(2, j);
       tstart = clock();
   for (k = 0; k < NITER; k++) {
       mode = BAOPEN_RONLY;
       bacio_(&mode, &start, &sizewise, &no, &nactual, &fdes, "file1", 
              datary, &namelen, &datanamelen); 
       mode = BAREAD;
       for (i = 0; i < totlen / sizewise; i++) {
          bacio_(&mode, &start, &sizewise, &no, &nactual, &fdes, "file1", 
                 datary, &namelen, &datanamelen); 
       }
       mode = BACLOSE;
       bacio_(&mode, &start, &sizewise, &no, &nactual, &fdes, "file1", 
              datary, &namelen, &datanamelen); 
   }
       printf("size %6d, %9.3f time \n",sizewise, (float)(clock() - tstart)/(float)CLOCKS_PER_SEC);
     }
     

   return 0;
}


int bacio_(int * mode, int * start, int * size, int * no, int * nactual, 
          int * fdes, const char *fname, char *data, int * namelen, 
          int * datanamelen) {
  int jret, seekret;


  if (( BAOPEN_RONLY & *mode) && (BAOPEN_WONLY & *mode) ) {
     printf("illegal -- trying to open both read only and write only\n");
     return -1;
  }

  if (BAOPEN_RONLY & *mode) {
     /* printf("open read only %s\n", fname); */
     *fdes = open(fname, O_RDONLY | O_CREAT , S_IRWXU);
  }
  else if (BAOPEN_WONLY & *mode ) {
     /* printf("open write only %s\n", fname); */
     *fdes = open(fname, O_WRONLY);
  }
  else if (BAOPEN_RW & *mode) {
     /* printf("open read-write %s\n", fname); */
     *fdes = open(fname, O_RDWR | O_CREAT , S_IRWXU);
  }
  else {
     /* printf("no openings\n"); */
  }
  if (*fdes < 0) {
    printf("error in file descriptor! *fdes\n", *fdes);
    return -2;
  }
  else {
    /* printf("file descriptor = %d\n",*fdes ); */
  }

  if (BAREAD & *mode && (BAOPEN_WONLY & *mode) ) {
    /* Error, trying to read while in write only mode! */
    return -3;
  }
  else if (BAREAD & *mode ) {
  /* Read in some data */
    seekret = lseek(*fdes, *start, SEEK_SET);
    if (seekret == -1) {
       printf("error in seeking to %d\n",start);
       return -4;
    }
    jret = read(*fdes, data, *no);
    if (jret != *no) {
      printf("did not read in the requested number of bytes\n");
      printf("read in %d bytes of %d \n",jret, *no);
      *nactual = jret;
    }  
    /*printf("read in %d bytes \n", jret); */
    *nactual = jret;
  }
/* Done with reading */
 
/* See if we should be writing */
  if ( BAWRITE & *mode && BAOPEN_RONLY & *mode ) {
     printf("Trying to write on a read only file \n");
     return -5;
  }
  else if ( BAWRITE & *mode ) {
     seekret = lseek(*fdes, *start, SEEK_SET);
     if (seekret == -1) {
       printf("error in seeking to %d\n",start);
       return -4;
     }
     jret = write(*fdes, data, *no);
     if (jret != *no) {
       printf("did not write out the requested number of bytes\n");
       printf("wrote %d bytes instead\n", jret);
       *nactual = jret;
     }
     else {
        printf("wrote %d bytes \n", jret);
        *nactual = jret;
     }
  }
    

  if (BACLOSE & *mode ) {
    jret = close(*fdes);
    if (jret != 0) { 
      printf("close failed! jret = %d\n",jret);
      return jret;
    }
  }

  return 0;
} 
