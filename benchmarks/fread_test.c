#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>

#define totlen (1024 * 1024 * 1024)

//Do some timing tests
int main(void) {
   char datary[1024 * 1024];
   int sizewise, i, j, n;
   struct tms tstart, tend;
   FILE *fin;

   for (i = 0; i < 1024*1024; i++) { datary[i] = 0;}
   printf("buf size = %d\n",BUFSIZ); fflush(stdout);

   fin = fopen("file1", "r");
   if (fin == (FILE *) NULL) {
     fin = fopen("file1","w");
     for (j = 0; j < (totlen/1024/1024) ; j++) {
       fwrite(datary, 1, 1024*1024, fin);
     }
     rewind(fin);
   }

   for (sizewise = BUFSIZ/8 ; sizewise <= BUFSIZ*1024; sizewise *= 2) {
     times(&tstart);
     rewind(fin);
     for (i = 0; i < totlen / sizewise; i++) {
        n = fread(datary, sizeof(char), sizewise, fin); 
        if ( (i % 1024) == 0) printf("n = %d\n",n); fflush(stdout);
     }
     times(&tend);
     printf("size %7d user time %5.3f sys time %5.3f total %6.3f  ", sizewise,
            (float) (tend.tms_utime - tstart.tms_utime) / (float) CLOCKS_PER_SEC *10000 ,
            (float) (tend.tms_stime - tstart.tms_stime) / (float) CLOCKS_PER_SEC *10000 ,
            (float) ( (tend.tms_utime - tstart.tms_utime) + (tend.tms_stime - tstart.tms_stime) ) / (float) CLOCKS_PER_SEC *10000 ) ;
     printf("%4d %4d  %4d %4d\n",(int) tend.tms_utime, (int) tstart.tms_utime, (int) tend.tms_stime , (int) tstart.tms_stime );
     fflush(stdout);
   }

   fclose(fin);
     
   return 0;
}
