#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_15th<float> field;
  FILE *fin, *fout;
  fin = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");
  field.reader(fin);
  field.ftnout(fout);
  printf("Size of grid is %d by %d\n",field.xpoints(), field.ypoints() );
  return 0;
}
