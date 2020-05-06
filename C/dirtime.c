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

  /* printf("name = %s\n",name); */
  dirpt = opendir(name);
  if ( (fd = dirfd(dirpt)) < 0) {
    return -1;
  }

  fstat(fd, &dbuf);
  /* printf("%d uid %d size %d atime %d mtime %d ctime\n",
      (int)dbuf.st_uid, (int)dbuf.st_size, (int)dbuf.st_atime, 
      (int)dbuf.st_mtime, (int)dbuf.st_ctime);
  */

  while ( (d = readdir(dirpt)) != 0 ) {
    sprintf(fname, "%s/%s",name, d->d_name);
    stat(fname, &fbuf);
    /* printf("%s atime %d mtime %d ctime %d \n",
         d->d_name, (int) fbuf.st_atime, (int) fbuf.st_mtime, 
                    (int) fbuf.st_ctime);
    */

/* if d_name != . or .., then seek for maximum atime, mtime */
    if ((strcmp(d->d_name,".") != 0) && (strcmp(d->d_name,"..") != 0)  ) {
      if (fbuf.st_atime > atime) atime = fbuf.st_atime;
      if (fbuf.st_mtime > mtime) mtime = fbuf.st_mtime;
    }

    /*count += 1; */
  }

  /*printf("%d things found\n",count); */ 
/*  Now check */
    sprintf(fname, "%s/%s",name, ".");
    stat(fname, &fbuf);
    if (mtime < fbuf.st_mtime) {
      printf("updating %s \nmtime is %d while cwd mtime is %d, delta = %f\n",
            fname, (int) mtime, (int) fbuf.st_mtime, (float) (fbuf.st_mtime - mtime)/86400.);
      timing.actime = atime;
      timing.modtime = mtime;
      utime(fname, &timing);
    } 
    /*  printf("mtime is %d while cwd mtime is %d, delta = %d\n", */
    /*           (int) mtime, (int) fbuf.st_mtime, (int) fbuf.st_mtime - mtime); */

  closedir(dirpt);
  return 0;

}
