#include <stdio.h>

#define MAXLEN 2000

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  float t1, t2, t3, t4, t5, t6;
  float j;
  char tab, line[MAXLEN*2];

  tab = (char) 9;
  fin = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  while ( !feof(fin) ) {
    fgets(line, MAXLEN, fin);
    sscanf(line, "%f %f %f %f %f %f %f\n",&j,&t1, &t2, &t3, &t4, &t5, &t6);
    printf("%e%c%e%c%e%c%e%c%e%c%e%c%e\n",j,tab,t1,tab,t2,tab,t3,tab,t4, tab,t5, tab,t6);
  }

  return 0;
}
