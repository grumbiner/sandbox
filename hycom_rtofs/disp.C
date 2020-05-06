#include "ncepgrids.h"

#define nx 216
#define ny  97

int main(int argc, char *argv[]) {
  FILE *fin;
  grid2<float> rad(nx, ny), sense(nx, ny), hs(nx, ny), hi(nx, ny), smax(nx, ny), imax(nx, ny);
  grid2<int> count(nx, ny);
  float x;
  palette<unsigned char> gg(19,65);

  fin = fopen(argv[1],"r");
  fread(&x, sizeof(float), 1, fin);
  rad.binin(fin);
  sense.binin(fin);
  hs.binin(fin);
  hi.binin(fin);
  imax.binin(fin);
  smax.binin(fin);
  count.binin(fin);
  fread(&x, sizeof(float), 1, fin);
  fclose(fin);

  rad /= (4*365);
  sense /= (4*365);

  printf("rad grid max, min, average %f %f %f\n",rad.gridmax(), rad.gridmin(), rad.average() );
  printf("sense grid max, min, average %f %f %f\n",sense.gridmax(), sense.gridmin(), sense.average() );
  printf("hs grid max, min, average %f %f %f\n",hs.gridmax(), hs.gridmin(), hs.average() );
  printf("hi grid max, min, average %f %f %f\n",hi.gridmax(), hi.gridmin(), hi.average() );
  printf("imax grid max, min, average %f %f %f\n",imax.gridmax(), imax.gridmin(), imax.average() );
  printf("smax grid max, min, average %f %f %f\n",smax.gridmax(), smax.gridmin(), smax.average() );
  printf("count grid max, min, average %d %d %d\n",count.gridmax(), count.gridmin(), count.average() );

  rad.scale();
  sense.scale();
  hs.scale();
  hi.scale();
  count.scale();
  imax.scale();
  smax.scale();
  rad.xpm("rad.xpm",7,gg);
  sense.xpm("sense.xpm",7,gg);
  hs.xpm("hs.xpm",7,gg);
  hi.xpm("hi.xpm",7,gg);
  count.xpm("count.xpm",7,gg);
  imax.xpm("imax.xpm",7,gg);
  smax.xpm("smax.xpm",7,gg);


  return 0;
}
