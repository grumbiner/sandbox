#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<float> ages[20];
  global_12th<unsigned char> input;
  FILE *fin;
  int i;
  ijpt loc;
  
  for (i = 0; i < 20; i++) {
    ages[i].set((float) 0);
  }

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    input.binin(fin);
    fclose(fin);
    for (loc.j = 0; loc.j < input.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < input.xpoints(); loc.i++) {
      ages[input[loc] ][loc] += 1;
    }
    }
  }

  fin = fopen("ageout","w");
  for (i = 0; i < 20; i++) {
    ages[i].binout(fin);
  }
  fclose(fin);

  return 0;
}
