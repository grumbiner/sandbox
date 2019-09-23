#include <stdio.h>

#define NX 385
#define NY 465

int main(int argc, char *argv[])
{
  FILE *fin1, *fin2, *fin3, *fout;
  unsigned char in1[NY][NX], in2[NY][NX], in3[NY][NX];
  int i, j;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fin3 = fopen(argv[3], "r");
  fout = fopen(argv[4], "w");

  i = fread(in1, sizeof(char), NX*NY, fin1);
  if (i != NX*NY) { printf("failed to read in file 1 properly\n"); return -1;}
  i = fread(in2, sizeof(char), NX*NY, fin2);
  if (i != NX*NY) { printf("failed to read in file 1 properly\n"); return -1;}
  i = fread(in3, sizeof(char), NX*NY, fin3);
  if (i != NX*NY) { printf("failed to read in file 1 properly\n"); return -1;}
  
  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
       fprintf(fout, "%5.1f %5.1f %5.1f\n", (float) in1[j][i], (float) in2[j][i], (float) in3[j][i]);
    }
  }

  return 0;
}
