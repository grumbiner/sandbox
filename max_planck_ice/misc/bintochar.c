#include <stdio.h>

int main(void)
{
  FILE *fin, *fout;
  int i, j;
  unsigned char map[93][77];

  fin = fopen("fort.10","r");
  fout = fopen("cout","w");

  fread(map, sizeof(unsigned char), 93*77, fin);
  for (j = 0; j < 93; j++) {
    for (i = 0; i < 77; i++) {
       if (map[j][i] == 157 ) {
         fprintf(fout, "%1d", 1);
       }
       else {
         fprintf(fout, "%1d", 0);
       }
    }
    fprintf(fout,"\n");
  }

  return 0;

}

