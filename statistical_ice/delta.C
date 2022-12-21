#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> average, today, delta;
  ijpt loc;
  FILE *fin1, *fin2, *fout;
  palette<unsigned char> gg(19,65);

  fin1 = fopen(argv[1],"r");
  average.binin(fin1);
  fclose(fin1);

  fin2 = fopen(argv[2],"r");
  today.binin(fin2);
  fclose(fin2);

  for (loc.j = 0; loc.j < average.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < average.xpoints(); loc.i++) {
    if (average[loc] > 1.0 || today[loc] > 1.0 || average[loc] == 0.0 ) {
      delta[loc] = 2.24;
    }
    else {
      delta[loc] = today[loc] - average[loc];
    }
  }
  }

  fout = fopen(argv[3], "w");
  delta.binout(fout);
  fclose(fout);
 
  return 0;
  printf("gridmax, min, average %f %f %f\n",delta.gridmax(), delta.gridmin(), delta.average() );
  for (loc.j = 0; loc.j < average.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < average.xpoints(); loc.i++) {
    if (delta[loc] == 2.24) {
      delta[loc] = 0;
    }
  }
  }
  delta.scale();
  printf("gridmax, min, average %f %f %f\n",delta.gridmax(), delta.gridmin(), delta.average() );
  delta.xpm("d.xpm",7,gg);


  return 0;
}
