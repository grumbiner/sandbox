#include <stdlib.h>

#include "ncepgrids.h"

template <class T>
float trials(mvector<T> &series, bool &mask);

int main(int argc, char *argv[]) {
  FILE *fin;
  char fname[80];
  int i, mm, k, j, ngood, count = 0;
  northhigh<unsigned char> conc[31];
  northhigh<unsigned char> floppy, avger;
  northhigh<bool> mask;
  ijpt loc;
  latpt ll;
  mvector<int> series(31);
  palette<unsigned char> gg(19, 65);

  mm = atoi(argv[1]);
  j = -1;
  for (i = 1; i <= 31; i++) {
    sprintf(fname,"parm13.north.2000%02d%02d",mm,i);
    fin = fopen(fname, "r");
    if (fin != (FILE*) NULL) {
      j++;
      conc[j].binin(fin);
      fclose(fin);
    }
  }
  ngood = j;
  printf("Found %d good days\n",ngood);

  mask.set(false);
  if (ngood != 31) series.resize(ngood);
  for (loc.j = 0; loc.j < conc[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc[0].xpoints(); loc.i++) {
    for (i = 0; i < ngood; i++) {
      series[i] = conc[i][loc];
    }
    if (trials(series, mask[loc]) > 0.0) {
      floppy[loc] = (unsigned char) (0.5 + trials(series, mask[loc]) ) ;
    }
    avger[loc] = series.average();
  }
  }
  floppy.xpm("flop.xpm", 14, gg);

  for (loc.j = 0; loc.j < avger.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < avger.xpoints(); loc.i++) {
//OLD    if (floppy[loc] > (unsigned char) 50 && avger[loc] > (unsigned char) 15) {
//OLD       ll = avger.locate(loc);
//OLD       printf("%3d %3d  %7.2f %6.2f  %3d %3d\n",loc.i, loc.j, ll.lon, ll.lat, 
//OLD                                          floppy[loc], avger[loc]);
//OLD       avger[loc] = 0;
//OLD    }
//OLD    if (floppy[loc] > (unsigned char) 50 ) {
//OLD      mask[loc] = true;
//OLD    }
//OLD    else {
//OLD      mask[loc] = false;
//OLD    }
     if (mask[loc]) { 
         count += 1;
         avger[loc] = 0;
     }
  }
  } 
  printf("max of avger %d %d\n",(int) avger.gridmax(), (int) avger.gridmin() );
  avger.xpm("avg.xpm", 7, gg);
  printf("%d points were masked out\n", count);

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    for (k = 0; k < ngood; k++) {
      if (mask[loc]) conc[k][loc] = 0;
    }
  }
  }

  for (k = 0; k < ngood; k++) {
    sprintf(fname,"nhfilt%02d.xpm",k);
    conc[k].xpm(fname, 9, gg);
  }
 
  return 0;
}

template <class T>
float trials(mvector<T> &series, bool &mask) {
  int i, nonzero = 0, flip = 0, follow = 0;

  nonzero = 0;
  flip = 0;
  for (i = 0; i < series.xpoints(); i++) {
    if (series[i] != 0) {
      nonzero += 1;
    }
    if (i != 0) {
      if ( ( (series[i] != 0) && (series[i-1] == 0) ) || 
           ( (series[i] == 0) && (series[i-1] != 0) )  ) {
        flip += 1;
      }
      if ( (series[i] != 0) && (series[i-1] != 0) ) {
        follow += 1;
      }
    }
  } //for

  //if (flip > nonzero + sqrt(nonzero*(1.-nonzero/series.xpoints()) ) ) {
  if (flip < nonzero - sqrt(nonzero*(1.-nonzero/series.xpoints()) ) ) {
    mask = false;
  }
  else {
    mask = true;
  } 
  return 100.*(float) flip / (float) nonzero;
}
