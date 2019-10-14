#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  struct stat buf;
  int nfield = 53;
  float bytes_per_point = 2.2;
  float guess;
  float sizes[4];
  int i;
  bool found;

  sizes[0] = 62.;
  sizes[1] = 126.;
  sizes[2] = 170.;
  sizes[3] = 256.;

  i = stat(argv[1], &buf);
  if (i != 0) {
    #ifdef VERBOSE
    printf("Error trying to stat %s\n",argv[1]);
    #endif
    printf("0\n");
    return 0;
  }

  guess = sqrt((float) buf.st_size/(float) nfield)/bytes_per_point;
//  printf("size %d  %d  %f\n", buf.st_size, buf.st_size/nfield, guess);

  found = false;
  for (i = 0; i < 4; i++) {
    if (fabs(guess/sizes[i] - 1.) < 0.1) {
      printf("%d\n",(int)sizes[i]);
      return 0;
      found = true;
      break;
    }
  }
  if (! found) {
    #ifdef VERBOSE
    printf("no match, size, guess = %d %f\n",buf.st_size, guess);
    #endif
    printf("0\n");
  }
  else {
    printf("%d i = %d\n",(int)sizes[i], i);
  }

  return 0;
}
