#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_sst<unsigned char> out, count;
  global_ice<unsigned char> in;
  ijpt loc, loc2;
  latpt ll;
  FILE *fin, *fout;
  
  fin = fopen(argv[1],"r");
  in.binin(fin);
  fclose(fin);
  count.set(0);
  out.set(0);

  for (loc.j = 0; loc.j < in.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < in.xpoints(); loc.i++) {
    ll = in.locate(loc);
    loc2 = out.locate(ll);
    if (in[loc] > 0) count[loc2] += 1;
  }
  }
  printf ("count max, min, avg %d %d %d\n",count.gridmax(), count.gridmin(), count.average() );

  for (loc.j = 0; loc.j < out.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < out.xpoints(); loc.i++) {
    if (count[loc] == 4) out[loc] = 157;
  }
  }
  fout = fopen(argv[2],"w");
  out.binout(fout);
  fclose(fout);
  printf ("out max, min, avg %d %d %d\n",out.gridmax(), out.gridmin(), out.average() );

  return 0;
} 
