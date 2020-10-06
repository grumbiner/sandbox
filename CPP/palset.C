#include "color.h"

void palset(palette<unsigned char> &x) {
  int i, ny, dcol;
 
  dcol = 256 / x.ncol;
  x.set_color(0, 0,0,0);
  for (i = 1; i < x.ncol; i++) {
    ny = i*dcol;
    x.set_color(i, ny, ny, ny);
  }
  
  return;
}
