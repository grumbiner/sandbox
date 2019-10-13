#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <sys/times.h>

#define totlen (100* 1024 * 1024)

//Do some timing tests
int main(void) {
   char datary[1024 * 1024];
   int sizewise, i, j, n;
   struct tms tstart, tend;
   FILE *fin;

   sizewise = BUFSIZ;
   //for (j = 4; j < 16; j++) { 
   //  sizewise *= 2;
     fin = fopen("file1", "r");
     times(&tstart);
     for (i = 0; i < totlen / sizewise; i++) {
        n = fread(datary, sizeof(char), sizewise, fin);
     }
     times(&tend);
     printf("user time %f sys time %f \n", 
            (float) (tend.tms_utime - tstart.tms_utime) / (float) CLOCKS_PER_SEC , 
            (float) (tend.tms_stime - tstart.tms_stime) / (float) CLOCKS_PER_SEC ) ;
     fclose(fin);
   //} 
     
   return 0;
}
