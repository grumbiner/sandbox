#include "ncepgrids.h"

int main(int argc, char *argv[] ) {
  FILE *fin, *fout;
  northgrid<float> x;
  northgrid<float> av[12], condav[12], squared[12], condsquare[12], prob[12];
  northgrid<int> count[12], icecount[12];
  ijpt loc;
  float flag = 2.24;
  int i;
 
  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open input file %s\n",argv[1]);
    return 1;
  }

  for (i = 0; i < 12; i++) {
    av[i].set((float) 0.0);
    condav[i].set((float) 0.0);
    squared[i].set((float) 0.0);
    condsquare[i].set((float) 0.0);
    prob[i].set((float) 0.0);

    count[i].set((int) 0.0);
    icecount[i].set((int) 0.0);
  }
    
  for (i = 0; i < 12*78; i++) {
    x.binin(fin);
    for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
       if (x[loc] < 1.28) {
         count[i%12][loc] += 1;
         av[i%12][loc] += x[loc];
         squared[i%12][loc] += x[loc]*x[loc];
       }
       if (x[loc] < 1.28 && x[loc] >= 0.15) {
         icecount[i%12][loc] += 1;
         condav[i%12][loc] += x[loc];
         condsquare[i%12][loc] += x[loc]*x[loc];
       }
    }
    }
  }

  fout = fopen(argv[2],"w");

  for (i = 0; i < 12; i++) {
    for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
      if (count[i][loc] > 0) {
        av[i][loc] /= count[i][loc];
        squared[i][loc] -= count[i][loc]*av[i][loc]*av[i][loc];
        prob[i][loc] = ((float) icecount[i][loc])/((float)count[i][loc]);
      }
      else {
        av[i][loc] = flag;
        squared[i][loc] = flag;
        prob[i][loc] = 0.0;
      }

      if (icecount[i][loc] > 0) {
        condav[i][loc] /= icecount[i][loc];
        condsquare[i][loc] -= icecount[i][loc]*condav[i][loc]*condav[i][loc];
      }
      else {
        condav[i][loc] = flag;
        condsquare[i][loc] = flag;
      }
    }
    }

    av[i].binout(fout);
    squared[i].binout(fout);
    condav[i].binout(fout);
    condsquare[i].binout(fout);
    prob[i].binout(fout);
  }


  return 0;
}
