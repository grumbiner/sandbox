#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  unsigned char line[2048];
  char cmd[900];
  char fname[9000];
  struct stat sb;
  struct tm *timefields;
  time_t tmp;
  int k, i;

  fin = fopen(argv[1],"r");
  stat(argv[1],&sb);
  tmp = sb.st_mtimespec.tv_sec;
  /* timefields = gmtime(&tmp); */
  timefields = localtime(&tmp);
  /* printf("%d secs\n",tmp); */
  sprintf(fname, ".%s",argv[1]);
  fout = fopen(fname,"w");
  if (fout == (FILE *) NULL) {
    printf("failed to open %s\n",fname);
    return 1;
  }
  /* printf("opened %s\n",fname); */

  /* printf("%d %02d %02d %02d %02d\n", 
         timefields->tm_year+1900, 
         timefields->tm_mon+1,
         timefields->tm_mday, 
         timefields->tm_hour, 
         timefields->tm_min);
  */

  
  while ( ! feof(fin) ) {
    k = fread(line,sizeof(unsigned char), 2048, fin);
    for (i = 0; i < k; i++) {
      if (line[i] == 13) {
        fprintf(fout,"\n");
      }
      else {
        fprintf(fout,"%c",line[i]);
      }
    }
  }
  fclose(fout);

/* Now reset the time to match */
  sprintf(cmd,"touch -t %d%02d%02d%02d%02d %s\n", 
         timefields->tm_year+1900, 
         timefields->tm_mon+1,
         timefields->tm_mday, 
         timefields->tm_hour, 
         timefields->tm_min,
         fname);
  system(cmd);

/* Finally, rename to the original */
  sprintf(cmd,"mv %s %s\n",fname, argv[1]);
  system(cmd);

  return 0;
}
