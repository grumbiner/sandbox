#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <malloc.h>
#include <ctype.h>
#include <string.h>

int BAOPEN_RONLY = 1;
int BAOPEN_WONLY = 2;
int BAOPEN_RW    = 4;
int BACLOSE      = 8;
int BAREAD      = 16;
int BAWRITE     = 32;

int bacio_(int * mode, int * start, int * size, int * no, int * nactual, 
          int * fdes, const char *fname, char *data, int  namelen, 
          int  datanamelen) {
  int i, j, jret, seekret, tlen;
  char *realname, *tempchar, blank=' ', slash='/', dash='-', period='.', underscore='_';
  int tcharval;

  if (( BAOPEN_RONLY & *mode) && (BAOPEN_WONLY & *mode) ) {
     printf("illegal -- trying to open both read only and write only\n");
     return -1;
  }
  if ( (BAREAD & *mode ) && (BAWRITE & *mode) ) {
     printf("illegal -- trying to both read and write in the same call\n");
     return -5;
  }

  if ( (BAOPEN_RONLY & *mode) | (BAOPEN_WONLY & *mode) | (BAOPEN_RW & *mode) ) {
    printf("Will be opening a file %d\n", namelen); fflush(stdout);
    realname = (char *) malloc( namelen * sizeof(char) ) ;
    if (realname == NULL) { 
      printf("failed to mallocate realname %d = namelen\n", namelen);
      fflush(stdout);
      return -6;
    }
    /* printf("about to copy\n"); fflush(stdout); */
    tempchar = (char *) malloc(sizeof(char) * 1 ) ;
    i = 0;
    j = 0;
    *tempchar = fname[i];
    tcharval = *tempchar;
    /* printf("tempchar %c\n", *tempchar); fflush(stdout); */
    while (i < namelen ) {
       /*printf("i = %d tempchar %c strcmp %d isalnum %d\n",i, tempchar, */ 
       /*                       strcmp(tempchar,&blank), isalnum(tcharval) ); */
       fflush(stdout); 
       if ( strcmp(tempchar, &blank) > 0 && (isalnum(tcharval) || 
               strcmp(tempchar, &period) || 
               strcmp(tempchar, &dash) || 
               strcmp(tempchar, &slash) || 
               strcmp(tempchar, &underscore) ) ) {
         realname[j] = fname[i];
         j += 1;
       }
       i += 1;
       *tempchar = fname[i];
       tcharval = *tempchar;
       /* printf("i %d tempchar %c\n", i, *tempchar); fflush(stdout); */
    }
    printf("i,j = %d %d\n",i,j); fflush(stdout);
    realname[j] = '\0';
  } 
   

  if (BAOPEN_RONLY & *mode) {
     printf("open read only %s\n", realname);
     *fdes = open(realname, O_RDONLY | O_CREAT , S_IRWXU);
  }
  else if (BAOPEN_WONLY & *mode ) {
     printf("open write only %s\n", realname);
     *fdes = open(realname, O_WRONLY | O_CREAT , S_IRWXU);
  }
  else if (BAOPEN_RW & *mode) {
     printf("open read-write %s\n", realname);
     *fdes = open(realname, O_RDWR | O_CREAT , S_IRWXU);
  }
  else {
     printf("no openings\n");
  }
  if (*fdes < 0) {
    printf("error in file descriptor! *fdes %d\n", *fdes);
    return -2;
  }
  else {
    printf("file descriptor = %d\n",*fdes );
  }

  if (BAREAD & *mode && (BAOPEN_WONLY & *mode) ) {
    printf("Error, trying to read while in write only mode!\n");
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
    printf("read in %d bytes \n", jret);
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
