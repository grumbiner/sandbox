#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Robert Grumbine  2007 Jan 10 */

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  unsigned char line[2048];
  char cmd[900];
  char fname[2045];
  struct stat sb;
  struct tm *timefields;
  time_t tmp;
  int k, i;

  stat(argv[1],&sb);
  tmp = sb.st_mtime;
  timefields = localtime(&tmp);
  
/* Now set the file name to include date */
  sprintf(fname,"%s.%4d%02d%02d", 
         argv[1],
         timefields->tm_year+1900, 
         timefields->tm_mon+1,
         timefields->tm_mday 
         );
  printf("%s\n",fname);

/* Finally, rename to the original */
  sprintf(cmd,"mv %s %s\n",argv[1],fname);
  system(cmd);

  return 0;
}
