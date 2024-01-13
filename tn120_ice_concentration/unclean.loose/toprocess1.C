#include <stdio.h>

#include "icessmi.h"

int main(void) {
  bufr_line line;
  process_1 x;
  int i;
  FILE *fin, *fout;

  printf("Size of a process1 %d\n",sizeof(process_1) );
  printf("Size of a process2 %d\n",sizeof(process_2) );
  fin = fopen("fort.51", "r");
  fout = fopen("process1.out", "w");

  while (!feof(fin) ) {
    fread(&line, sizeof(bufr_line), 1, fin);
    for (i = 0; i < NSCANS; i++) {
      toprocess_1(x, line.full[i]);
      fwrite(&x, sizeof(process_1), 1, fout);
    }
  }

  return 0;
}
