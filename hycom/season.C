#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  stlawrence<unsigned char> input;
  stlawrence<float> output;
  stlawrence<int> first, last, days;
  FILE *fin, *fout;
  int dayno;
  ijpt loc;
  latpt ll;
  palette<unsigned char> gg(19,65);
  char fname[90];

  first.set(0);
  last.set(0);
  days.set(0);

  for (dayno = 1; dayno < argc; dayno++) {
    fin = fopen(argv[dayno],"r");
    input.binin(fin);
    fclose(fin);
    for (loc.j = 0; loc.j < input.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < input.xpoints(); loc.i++) {
      output[loc] = (float) input[loc];
      if (output[loc] >= 15. && output[loc] < 128.) {
        if (first[loc] == 0) first[loc] = dayno;
        last[loc] = dayno;
        days[loc] += 1;
      }
    }
    } 
    sprintf(fname,"obsd.%d",dayno);
    fout = fopen(fname,"w");
    output /= 100.0;
    output.ftnout(fout);
    fclose(fout);
  }

  for (loc.j = 0; loc.j < input.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < input.xpoints(); loc.i++) {
    ll = first.locate(loc);
    if (days[loc] > 0 ) {
    printf("%3d %3d  %3d %3d %3d %f  %5.2f %6.2f\n",loc.i, loc.j, 
            first[loc], last[loc], days[loc], 
            (float) days[loc] / (float) (last[loc] - first[loc] + 1) ,
            ll.lat, ll.lon);
    }
    if (last[loc] >= dayno - 1) {
      printf("ice at end of record %3d %3d\n",loc.i, loc.j);
    }
  }
  }

  days.xpm("days.xpm", 11, gg);
  fout = fopen("days","w");
  days.ftnout(fout);
  fclose(fout);
  first.xpm("first.xpm", 11, gg);
  fout = fopen("first","w");
  first.ftnout(fout);
  fclose(fout);
  last.xpm("last.xpm", 13, gg);
  fout = fopen("last","w");
  last.ftnout(fout);
  fclose(fout);

  return 0;
}
