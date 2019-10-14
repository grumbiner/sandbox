#include "ncepgrids.h"

// apply the 0.25 degree a posteriori file to the hemispheric analysis grids
// use the nearest neighbor ll point as the flag

float posteriori(psgrid<float> &x, global_quarter<unsigned char> &flags);

int main(int argc, char *argv[]) {
  FILE *inflag, *innh, *insh;
  global_quarter<unsigned char> flags;
  northhigh<float> nh;
  southhigh<float> sh;
  float area = 0.0;

  inflag = fopen(argv[1], "r");
  innh   = fopen(argv[2], "r");
  insh   = fopen(argv[3], "r");

  flags.binin(inflag); fclose(inflag);
  nh.binin(innh); fclose(innh);
  sh.binin(insh); fclose(insh);
   
  if (nh.gridmax() < 3) nh *= 100.;
  area = posteriori(nh, flags);
  //printf("%f\n",area / 1.e12 );

  if (sh.gridmax() < 3) sh *= 100.;
  area += posteriori(sh, flags);
  printf("%f\n",area / 1.e9 );

  return 0;
}
float posteriori(psgrid<float> &x, global_quarter<unsigned char> &flags) {
  float area = 0;
  ijpt iloc, floc;
  latpt ll;

  for (iloc.j = 0; iloc.j < x.ypoints(); iloc.j++) {
  for (iloc.i = 0; iloc.i < x.xpoints(); iloc.i++) {
    ll = x.locate(iloc);
    floc = flags.locate(ll);
    if (flags[floc] == 2 || flags[floc] == 224) {
      x[iloc] = 0;
    }
    else if (flags[floc] == 158 || flags[floc] == 159 || flags[floc] == 160 \
                           || flags[floc] == 161 || flags[floc] == 162 \
                           || flags[floc] == 163 || flags[floc] == 164 ) {
      if (x[iloc] >= 15 && x[iloc] <= 128) {
           area += x.cellarea(iloc); 
           x[iloc] = 1;
      }
      else {
        x[iloc] = 0;
      }
    }
    else if (flags[floc] == 170 || flags[floc] == 171 || flags[floc] == 172) {
      if (x[iloc] >= 15 && x[iloc] <= 128) {
           area += x.cellarea(iloc); 
           x[iloc] = 1;
      }
      else {
        x[iloc] = 0;
      }
    }
    else {
      x[iloc] = 0;
    }
  }
  }

  //char fname[90];
  //palette<unsigned char> gg(2);
  //gg.set_color(0, 255, 255, 255);
  //gg.set_color(1, 255,   0,   0);
  //x.xpm("x.xpm",1,gg);
  //exit(1);


  return area;
}
