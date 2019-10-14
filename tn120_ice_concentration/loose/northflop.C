#include <stdlib.h>

#include "ncepgrids.h"

template <class T>
float trials(mvector<T> &series) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  char fname[80];
  int i, mm;
  northhigh<unsigned char> conc[31];
  northhigh<unsigned char> floppy, avger;
  ijpt loc;
  latpt ll;
  mvector<int> series(31);
  palette<unsigned char> gg(19, 65);

  mm = atoi(argv[1]);
  for (i = 1; i <= 31; i++) {
    //CDprintf("%02d\n",i);
    sprintf(fname,"parm13.north.2000%02d%02d",mm,i);
    fin = fopen(fname, "r");
    if (fin != (FILE*) NULL) {
      conc[i-1].binin(fin);
      fclose(fin);
    }
  }

  for (loc.j = 0; loc.j < conc[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc[0].xpoints(); loc.i++) {
    for (i = 0; i < 31; i++) {
      series[i] = conc[i][loc];
    }
    if (trials(series) > 0.0) {
      floppy[loc] = (unsigned char) (0.5 + trials(series) ) ;
      //CDprintf("  %3d %3d\n",loc.i, loc.j);
    }
    avger[loc] = series.average();
  }
  }
  floppy.xpm("flop.xpm", 14, gg);

  for (loc.j = 0; loc.j < avger.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < avger.xpoints(); loc.i++) {
    //CD printf("%3d %3d  %3d %3d\n",loc.i, loc.j, floppy[loc], avger[loc]);
    if (floppy[loc] > (unsigned char) 50 && avger[loc] > (unsigned char) 15) {
       ll = avger.locate(loc);
       printf("%3d %3d  %7.2f %6.2f  %3d %3d\n",loc.i, loc.j, ll.lon, ll.lat, 
                                          floppy[loc], avger[loc]);
    }
  }
  } 
 
  return 0;
}

template <class T>
float trials(mvector<T> &series) {
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

  if (nonzero > 1 ) {
   printf("%f  %f  %f\n",(float)follow / (float)(nonzero - 1) ,
                     (float)flip/(float)series.xpoints() ,
                     (float)nonzero/(float)series.xpoints() );
    //printf("%2d %2d ",nonzero, flip); 
  }

  return 100.*(float) flip / (float) nonzero;
}
