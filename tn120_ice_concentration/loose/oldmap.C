#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<unsigned char> data, land;
  global_12th<float> counts;
  int i, j;
  palette<unsigned char> gg(19, 65);
  ijpt loc;
  latpt ll;

  counts.set( (float) 0.);

  fin = fopen(argv[1], "r");
  land.binin(fin);
  fclose(fin);

  for (i = 2; i < argc; i++) {
    fin = fopen(argv[i], "r");
    data.binin(fin);
    fclose(fin);
  
    for (j = 0; j < data.xpoints()*data.ypoints(); j++) {
      if (data[j] > 10) counts[j] += 1;
    }
  }

  fin = fopen("counts","w");
  counts.binout(fin);
  fclose(fin);

  for (j = 0; j < data.xpoints()*data.ypoints(); j++) {
    if (counts[j] != 0) {
      loc.i = j % data.xpoints();
      loc.j = j / data.xpoints();
      ll = counts.locate(loc);
      printf("land mask %3d  counts %.0f lat-lon %f %f\n",land[j], counts[j],
            ll.lat, ll.lon);
    }
  }


  counts.scale();
  counts.xpm("counts.xpm",1,gg);

  return 0;
}
