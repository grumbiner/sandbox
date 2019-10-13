#include <stdio.h>

#include "ncepgrids.h"
// The first argument is the reference file, and the rest are estimators

int main(int argc, char *argv[]) {
  mvector<northhigh<unsigned char> > estimates;
  mvector<float> column;
  northhigh<unsigned char> ref;
  northhigh<float> var;
  FILE *fin;
  ijpt loc;
  int i;

  //printf("argc = %d\n",argc);
  estimates.resize(argc-2);
  column.resize(argc-2);
  fin = fopen(argv[1], "r");
  ref.binin(fin);
  fclose(fin);
  var.set( (float) 0.0);

  for (i = 0; i < argc - 2; i++) {
    //printf("opening %s\n",argv[i+2]);
    fin = fopen(argv[i+2], "r");
    estimates[i].binin(fin);
    fclose(fin);
  }
  for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
     if (ref[loc] > 128) continue;
     printf("%3d %3d  %3d ",loc.i, loc.j, (int) ref[loc]);
     for (i = 0; i < estimates.xpoints() ; i++) {
        column[i] = estimates[i][loc];
        printf("%3d ",(int) column[i]);
        column[i] -= ref[loc];
     }
     printf(" %5.1f %6.1f %7.2f %7.2f ",column.maximum(), column.minimum(), column.average(), column.rms() );
     column -= column.average();
     printf(" %7.2f\n", column.rms() ); // having removed the mean, this is now a sdev
     var[loc] = column.rms() ;
     if (var[loc] > 7.0) ref[loc] = 0;
  }
  }

  fin = fopen("filt","w");
  ref.binout(fin);
  fclose(fin);

  return 0;
}
