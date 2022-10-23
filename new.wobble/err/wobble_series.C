#include "ncepgrids.h"

// Reanalysis is T62

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  gaussian<float> press(62), tmp(62);
  int k, count = 0;
  palette<unsigned char> gg(19, 65);
  mvector<float> line(press.xpoints());
  grid3<float> series(press.xpoints(), press.ypoints()/10, 73048); 
         //73048 is number of 6 hourly observations in the 50 years
  ijpt loc, tloc;

  printf("line.xpoints = %d\n",line.xpoints());
  press.set((float) 0.);
  fin = fopen("averaged_field","r");
  press.binin(fin);
  fclose(press);
  
  for (int i = 0; i < argc; i++) {
    fin = fopen(argv[i],"r");
    if (fin == (FILE *) NULL) {
      return 1;
    }
    while ( !feof(fin) ) {
      k = tmp.binin(fin);
      if (k == tmp.xpoints()*tmp.ypoints()) {
        tloc.k = count;
        for (tloc.i = 0; tloc.i < series.xpoints(); tloc.i++) {
          loc.i = tloc.i;
          for (tloc.j = 0; tloc.j < series.ypoints(); tloc.j++) { 
            loc.j = tloc.j; // will have this advance later
            series[tloc] = tmp[loc] - press[loc] ;  // get the deviation from the long term average
          }
        }
        count += 1;
      }
    }
    fclose(fin);
  }
  printf("count = %d\n",count);

// Now have the deviations, can do something:
  for (tloc.j = 0; tloc.j < series.ypoints(); tloc.j++) { 
  for (tloc.i = 0; tloc.i < series.xpoints(); tloc.i++) {
    sprintf(fname, "%03d%02d",tloc.i, tloc.j);
    fout = fopen(fname, "w");
    for (tloc.k = 0; tloc.k < series.kpoints(); tloc.k++) {
...   want to reorder such that k is the fastest varying index for our write out purposes
    }
  }
  } 


  return 0;
 
}
