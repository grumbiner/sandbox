#include <stdio.h>
#include <stdlib.h>

#define MAXLEN 2000

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  float t1, t2;
  double t3, t4;
  float j;
  char tab, line[MAXLEN*2], c1, c2, *num;

  tab = (char) 9;
  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");
  while ( !feof(fin1) ) {
    fgets(line, MAXLEN, fin1);
    sscanf(line, "%f %f %f\n",&j,&t1, &t2);
    //printf("%g %g %g\n",j, t1, t2);

    fgets(line, MAXLEN, fin2);
    //num = strtok(line, tab);
    //j = atof(num);
    //num = strtok(line, tab);
    //t3 = atof(num);
    //num = strtok(line, tab);
    //t4 = atof(num);
    //printf(" k %g %g %g\n",j,t3, t4); fflush(stdout);

    sscanf(line, "%f %f %f\n",&j,&t3, &t4);
    printf(" k %g %g %g\n",j,t3, t4); fflush(stdout);
    //printf("%g%c%g%c%g%c%g%c%g\n",j,tab,t1,tab,t2,tab,t3,tab,t4);
  }

  return 0;
}
