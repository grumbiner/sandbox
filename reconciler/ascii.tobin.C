#include "grid_math.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  grid2<float> lats(421,403), lons(421,403);

  fin = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");
  lats.reader(fin);
  lons.reader(fin);

  lats.binout(fout);
  lons.binout(fout);

  return 0;
}
