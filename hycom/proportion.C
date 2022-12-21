#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  int ti, tj, count, obsd;
  stlawrence<float> graphic;
  palette<unsigned char> shade(10);
  ijpt loc;

  graphic.set((float) 0.0);
  shade.set_color(0,  0,  0,  0);
  shade.set_color(1,  0,  0,255);
  shade.set_color(2, 64, 64,255);
  shade.set_color(3,128,128,255);
  shade.set_color(4,255,  0,  0);
  shade.set_color(5,255, 64, 64);
  shade.set_color(6,255,128,128);
  shade.set_color(7,255,255,255);
  
  fin = fopen("alpha","r");
  while ( ! feof(fin) ) {
    fscanf(fin, "%d %d %d %d",&ti, &tj, &count, &obsd);
    //printf("%d %d %d %d\n",ti, tj, count, obsd);
    loc.i = ti;
    loc.j = tj;
    count /= 4;
    if ( count > 8.* obsd ) {
      graphic[loc] = 1;
    }
    else if ( count > 4.* obsd ) {
      graphic[loc] = 2;
    }
    else if ( count > 2.* obsd ) {
      graphic[loc] = 3;
    }
    else if (count < obsd / 8.) {
      graphic[loc] = 4;
    }
    else if (count < obsd / 4.) {
      graphic[loc] = 5;
    }
    else if (count < obsd / 2.) {
      graphic[loc] = 6;
    }
    else {
      graphic[loc] = 7;
    }
  }

  graphic.xpm("ratios.xpm",1,shade);

  return 0;
}
