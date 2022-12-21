#include "gaussian.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  gaussian<float> u(62);
  ijpt loc1, loc2;
  fijpt floc;
  latpt north_sea,snsea;
  char tab;

  fout = fopen(argv[2], "w");
  tab = (char) 9;
  north_sea.lat = 50.0;
  north_sea.lon = 10.0;
  snsea.lat = -50.0;
  snsea.lon = 10.0;
  floc = u.locate(north_sea);
  loc1.i = (int)(0.5 + floc.i);
  loc1.j = (int)(0.5 + floc.j);
  floc = u.locate(snsea);
  loc2.i = (int)(0.5 + floc.i);
  loc2.j = (int)(0.5 + floc.j);

  fin = fopen(argv[1], "r");
  for (int i = 0; i < 75972; i++) {
    u.binin(fin);
    fprintf(fout, "%f%c%f%c%f\n",i*0.25, tab, u[loc1], tab, u[loc2]);
  }

  return 0;
} 
