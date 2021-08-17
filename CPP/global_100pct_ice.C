#include "ncepgrids.h"

int main() {
  global_12th<unsigned char> icec;

  icec.set(100);
  FILE *fin = fopen("ice100","w");
  icec.binout(fin);
  fclose(fin);
  return 0;
}
