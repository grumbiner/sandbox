/* Set the directory time to the date of the most recent file/directory contained therein */
/* Robert Grumbine 25 January 2006 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <utime.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>

int my_ls(char *name) ;

int main(int argc, char *argv[]) {
  char name[900];

  sprintf(name,"%s",argv[1]);
  my_ls(name);

  return 0;
}

int my_ls(char *name) {
  struct dirent *d;
  int count = 0; 
  int fd;
  DIR *dirpt;
  struct stat dbuf, fbuf;
  char fname[90000];
  struct utimbuf timing;
  time_t atime = 0, mtime = 0;

  dirpt = opendir(name);
  if ( (fd = dirfd(dirpt)) < 0) {
    return -1;
  }

  fstat(fd, &dbuf);

  while ( (d = readdir(dirpt)) != 0 ) {
    count += 1;
  }

/* Subtract 2 for . and .. */
  printf(" %d things found in %s\n",count-2, name); 

  closedir(dirpt);
  return 0;

}
