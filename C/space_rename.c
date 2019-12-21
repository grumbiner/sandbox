#include <stdio.h>

#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>

#define NMAX 900

int main(int argc, char *argv[]) {
  int fdes, i, j, len, count = 0;
  struct stat buf;
  char tmpnam[NMAX];
  
  printf("%s\n",argv[1]);
  fdes = open(argv[1], O_RDONLY);

  len = strlen(argv[1]);
  //printf("fdes = %d %d \n",fdes, len); fflush(stdout);

  //printf("trying strncpy\n"); fflush(stdout);
  strncpy(tmpnam, argv[1],(size_t) len);
  //printf("tmpnam %s\n",tmpnam); fflush(stdout);

  i = fstat(fdes, &buf);
  if (i != 0) {
    printf("%d inode %d\n",i, (int) buf.st_ino); fflush(stdout);
  }

  count = 0;
  for (j = 0; j < len; j++) {
    if (strncmp(&tmpnam[j]," ",1) == 0) {
      //printf(" space \n"); fflush(stdout);
      tmpnam[j] = '_';
      count++;
    }
  }
  if (count != 0) {
    //printf("have count %d spaces to replace\n",count);
    fflush(stdout);
    printf("new name = %s\n",tmpnam);
  } 

  i = link(argv[1], tmpnam);
  printf("link return code = %d\n",i);
  if (i == 0) {
    i = unlink(argv[1]);
    printf("unlink return code = %d\n",i);
  }

  return 0;
}
