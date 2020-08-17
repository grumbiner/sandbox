#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>

#define NMAX 900

int main(int argc, char *argv[]) {
  int fdes, i, j, len, count = 0;
  struct stat buf;
  char tmpnam[NMAX];
  char *newname;
  
  printf("%s\n",argv[1]);
  fdes = open(argv[1], O_RDONLY);

  len = strlen(argv[1]);
  //debug printf("fdes = %d %d \n",fdes, len); fflush(stdout);

  //debug printf("trying strncpy\n"); fflush(stdout);
  strncpy(tmpnam, argv[1],(size_t) len);
  //debug printf("tmpnam %s\n",tmpnam); fflush(stdout);

  i = fstat(fdes, &buf);
  if (i != 0) {
    printf("%d inode %d\n",i, (int) buf.st_ino); fflush(stdout);
  }

  count = 0;
  for (j = 0; j < len; j++) {
    if (strncmp(&tmpnam[j]," ",1) == 0) {
      //debug printf(" space \n"); fflush(stdout);
      tmpnam[j] = '_';
      count++;
    }
  }
  if (count != 0) {
    //debug printf("have count %d spaces to replace\n",count); fflush(stdout);
    printf("new name = %s\n",tmpnam);
  }
  else {
    return 0;
  }

  newname = (char *) malloc( (size_t) len);
  strncpy(newname, tmpnam, (size_t) len);
  printf("new name = %s\n",newname);

  i = link(argv[1], newname);
  printf("link return code = %d\n",i);
  if (i == 0) {
    i = unlink(argv[1]);
    printf("unlink return code = %d\n",i);
  }

  return 0;
}
