#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int BAOPEN_RONLY = 1;
int BAOPEN_WONLY = 2;
int BAOPEN_RW    = 4;
int BACLOSE      = 8;
int BAREAD      = 16;
int BAWRITE     = 32;

int bacio(int * mode, int * start, int * size, int * no, int * nactual, 
          int * fdes, const char *fname, int * namelen, 
          char *data, int * datanamelen) ;

int main(void) {
  int mode, start, size, no, nactual, fdes, namelen, datanamelen;
  char fname[800], data[80*1024];
  int jret;

  printf("bufsiz = %d\n",BUFSIZ);

  mode = BAOPEN_RW | BACLOSE | BAREAD | BAWRITE ;
  sprintf(fname, "file1");
  start = 0;
  size  = 1;
  no    = 1024 * 3;
  jret = bacio(&mode, &start, &size, &no, &nactual, &fdes, fname, &namelen,
                 data, &datanamelen);
  printf("nactual = %d\n", nactual);

  return 0;
}
  

int bacio(int * mode, int * start, int * size, int * no, int * nactual, 
          int * fdes, const char *fname, int * namelen, 
          char *data, int * datanamelen) {
  int jret, seekret;

  if (( BAOPEN_RONLY & *mode) && (BAOPEN_WONLY & *mode) ) {
     printf("illegal -- trying to open both read only and write only\n");
     return -1;
  }

  if (BAOPEN_RONLY & *mode) {
     printf("open read only %s\n", fname);
     *fdes = open(fname, O_RDONLY | O_CREAT , S_IRWXU);
  }
  else if (BAOPEN_WONLY & *mode ) {
     printf("open write only %s\n", fname);
     *fdes = open(fname, O_WRONLY);
  }
  else if (BAOPEN_RW & *mode) {
     printf("open read-write %s\n", fname);
     *fdes = open(fname, O_RDWR | O_CREAT , S_IRWXU);
  }
  else {
     printf("no openings\n");
  }
  if (*fdes < 0) {
    printf("error in file descriptor!\n");
    return -2;
  }
  else {
    printf("file descriptor = %d\n",*fdes );
  }

  if (BAREAD & *mode && (BAOPEN_WONLY & *mode) ) {
    //Error, trying to read while in write only mode!
    return -3;
  }
  else if (BAREAD & *mode ) {
  // Read in some data
    seekret = lseek(*fdes, *start, SEEK_SET);
    if (seekret == -1) {
       printf("error in seeking to %d\n",start);
       return -4;
    }
    jret = read(*fdes, data, *no);
    if (jret != *no) {
      printf("did not read in the requested number of bytes\n");
      printf("read in %d bytes instead \n",jret);
      *nactual = jret;
    }  
    printf("read in %d bytes \n", jret);
    *nactual = jret;
  }
// Done with reading
 
// See if we should be writing
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
