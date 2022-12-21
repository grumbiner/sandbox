#include "ncepgrids.h"

int main(void) {
  global_12th<float> x;
  FILE *fout;

  fout = fopen("fat","w");
  for (int i = 1; i < 1024 / 9; i++) {
    x.binout(fout);
  }

  return 0;
}
