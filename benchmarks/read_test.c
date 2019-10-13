#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <sys/times.h>

#define totlen (1024 * 1024 * 1024)

// version to invoke read directly

//Do some timing tests
int main(void) {
   char datary[1024 * 1024];
   int sizewise, i, j, n;
   struct tms tstart, tend;
   //FILE *fin;
   int fin;

  fin = open("file1", O_RDONLY, 644);
  printf("fin = %d\n",fin);

  for (sizewise = BUFSIZ/8 ; sizewise <= BUFSIZ*1024; sizewise *= 2) {
     times(&tstart);
     lseek(fin, 0, SEEK_SET);
     for (i = 0; i < totlen / sizewise; i++) {
        n = read(fin, datary, sizewise);
     }
     times(&tend);
     printf("size %7d user time %5.3f sys time %5.3f total %6.3f\n", sizewise,
            (float) (tend.tms_utime - tstart.tms_utime) / (float) CLOCKS_PER_SEC*10000 ,
            (float) (tend.tms_stime - tstart.tms_stime) / (float) CLOCKS_PER_SEC*10000 ,
            (float) ( (tend.tms_utime - tstart.tms_utime) + (tend.tms_stime - tstart.tms_stime) ) / (float) CLOCKS_PER_SEC*10000 ) ;
     fflush(stdout);
  }

   close(fin);
     
   return 0;
}
