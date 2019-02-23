int banio_(int * mode, int * start, int * size, int * no, int * nactual, 
          int * fdes, const char *fname, char *data, int  namelen ) {
  int i, j, jret, seekret;
  char *realname, *tempchar, blank=' ';
  int tcharval;
/* These are the special (i.e., other than alphanumeric) characters which */
/*   are permissable in file names */
  char slash='/', dash='-', period='.', underscore='_';


/* Check for illegal combinations of options */
  if (( BAOPEN_RONLY & *mode) && (BAOPEN_WONLY & *mode) ) {
     printf("illegal -- trying to open both read only and write only\n");
     return -1;
  }
  if ( (BAREAD & *mode ) && (BAWRITE & *mode) ) {
     printf("illegal -- trying to both read and write in the same call\n");
     return -2;
  }

/* This section handles Fortran to C translation of strings so as to */
/*   be able to open the files Fortran is expecting to be opened.    */
  if ( (BAOPEN_RONLY & *mode) | (BAOPEN_WONLY & *mode) | (BAOPEN_RW & *mode) ) {
    printf("Will be opening a file %d\n", namelen); fflush(stdout);
    realname = (char *) malloc( namelen * sizeof(char) ) ;
    if (realname == NULL) { 
      printf("failed to mallocate realname %d = namelen\n", namelen);
      fflush(stdout);
      return -3;
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
   
/* Open files with correct read/write and file permission. */
  if (BAOPEN_RONLY & *mode) {
     printf("open read only %s\n", realname);
     *fdes = open(realname, O_RDONLY | O_CREAT , S_IRWXU | S_IRWXG | S_IRWXO );
  }
  else if (BAOPEN_WONLY & *mode ) {
     printf("open write only %s\n", realname);
     *fdes = open(realname, O_WRONLY | O_CREAT , S_IRWXU | S_IRWXG | S_IRWXO );
  }
  else if (BAOPEN_RW & *mode) {
     printf("open read-write %s\n", realname);
     *fdes = open(realname, O_RDWR | O_CREAT , S_IRWXU | S_IRWXG | S_IRWXO );
  }
  else {
     printf("no openings\n");
  }
  if (*fdes < 0) {
    printf("error in file descriptor! *fdes %d\n", *fdes);
    return -4;
  }
  else {
    printf("file descriptor = %d\n",*fdes );
  }


/* Read data as requested */
  if (BAREAD & *mode && (BAOPEN_WONLY & *mode) ) {
    printf("Error, trying to read while in write only mode!\n");
    return -5;
  }
  else if (BAREAD & *mode ) {
  /* Read in some data */
    seekret = lseek(*fdes, *start, SEEK_SET);
    if (seekret == -1) {
       printf("error in seeking to %d\n",*start);
       return -6;
    }
    jret = read(*fdes, data, *no*(*size) );
    if (jret != *no*(*size) ) {
      printf("did not read in the requested number of items\n");
      printf("read in %d items of %d \n",jret/(*size), *no);
      *nactual = jret/(*size);
    }  
    printf("read in %d items \n", jret/(*size));
    *nactual = jret/(*size);
  }
/* Done with reading */
 
/* See if we should be writing */
  if ( BAWRITE & *mode && BAOPEN_RONLY & *mode ) {
     printf("Trying to write on a read only file \n");
     return -7;
  }
  else if ( BAWRITE & *mode ) {
     seekret = lseek(*fdes, *start, SEEK_SET);
     if (seekret == -1) {
       printf("error in seeking to %d\n",*start);
       return -8;
     }
     jret = write(*fdes, data, *no*(*size));
     if (jret != *no*(*size)) {
       printf("did not write out the requested number of items\n");
       printf("wrote %d bytes instead\n", jret/(*size) );
       *nactual = jret/(*size) ;
     }
     else {
        printf("wrote %d items \n", jret/(*size) );
        *nactual = jret/(*size) ;
     }
  }
/* Done with writing */
    

/* Close file if requested */
  if (BACLOSE & *mode ) {
    jret = close(*fdes);
    if (jret != 0) { 
      printf("close failed! jret = %d\n",jret);
      return -9;
    }
  }
/* Done closing */

  return 0;
} 
