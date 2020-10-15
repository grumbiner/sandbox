#include "ncepgrids.h"

int main(int argc, char *argv[] ) {
  FILE *fin, *fout;
  northgrid<float> x, av;
  northgrid<int> count;
  ijpt loc;
  int i;
 
  fin = fopen(argv[1],"r");

  av.set((float)0);
  count.set(0);

  for (i = 0; i < 362; i++) {
     x.binin(fin);
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
     if (x[loc] < 1.28) {
       av[loc] += x[loc];
       count[loc] += 1;
     }
  }
  }
  }

  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    if (count[loc] > 0) {
      av[loc] /= count[loc];
    }
    else {
      av[loc] = 2.24;
    }
  }
  }

  fout = fopen(argv[2],"w");
  av.binout(fout);

  return 0;
}
