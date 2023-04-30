#include <stdio.h>

#define MAXLEN 2000

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  float t1, t2, t3;
  char tab, line[MAXLEN*2];

  tab = (char) 9;
  fin = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  while ( !feof(fin) ) {
    fgets(line, MAXLEN, fin);
    sscanf(line, "%f %f %f\n",&t1, &t2, &t3);
    printf("%f%c%f%c%f\n",t1,tab,t2,tab,t3);
  }

  return 0;
}
