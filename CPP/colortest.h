#include <malloc.h>
#include <math.h>

#ifndef POINTH
  #include "points.h"
#endif
//Feb 25  2000

class color {
  public :
    unsigned char red, green, blue;
    operator=(point3<unsigned char>);
    color operator+(color );
    color operator-(color );
    float magnitude();
    void brighten(float );
    void brightness(float );
    void darken(float );
    color negate();
};
color::operator=(point3<unsigned char> x) {
  red   = x.i;
  green = x.j;
  blue  = x.k;
}
color color::operator+(color x) {
  color y;
  y.red   = (unsigned char) min(255, (int)x.red + (int)red);
  y.green = (unsigned char) min(255, (int)x.green + (int)green);
  y.blue  = (unsigned char) min(255, (int)x.blue + (int)blue);
  return y;
}
color color::operator-(color x) {
  color y;
  y.red   = (unsigned char) min(0, (int)x.red - (int)red);
  y.green = (unsigned char) min(0, (int)x.green - (int)green);
  y.blue  = (unsigned char) min(0, (int)x.blue - (int)blue);
  return y;
}

void color::brighten(float x) {
  red   = min( (unsigned char) 255, 
               (unsigned char) ( 0.5 + ( (float)red *x)) );
  green = min( (unsigned char) 255, 
               (unsigned char) ( 0.5 + ( (float)green  * x)) );
  blue  = min( (unsigned char) 255, 
               (unsigned char) ( 0.5 + ( (float)blue  * x)) );
}
void color::darken(float x) {
  red = (unsigned char) ( 0.5 + ( (float)red/x));
  green = (unsigned char) ( 0.5 + ( (float)green / x));
  blue  = (unsigned char) ( 0.5 + ( (float)blue / x));
}
float color::magnitude() {
  point3<float> x;
  x.i = red;
  x.j = green;
  x.k = blue;
  return (x.magnitude());
}
void color::brightness(float x) {
  float mag, mag2;
  mag = this->magnitude();
  mag2 = x / mag ; 
  this->brighten(mag2);
}
color color::negate() {
  color x;
  x.red   = 255 - red;
  x.green = 255 - green;
  x.blue  = 255 - blue;
  return x;
}    

class palette {
  public:
    int ncol;
    color *table;
    palette(void);
    palette(int );
    void setcolor(int , color);
    color getcolor(int);
// Analogs to color operators, to be applied sequentially
    void brighten(float );
    void brightness(float );
    void darken(float );
    void negate();
};
palette::palette(void) {
  ncol = 0;
  table = (color*) malloc(ncol*sizeof(color));
}
palette::palette(int n) {
  ncol = n;
  table = (color*) malloc(ncol*sizeof(color));
}
void palette::setcolor(int n, color c) {
  if (n >= ncol) {printf("there aren't that many colors in the table\n");
                  return; }
  table[n].red = c.red;
  table[n].green = c.green;
  table[n].blue  = c.blue;
}
color palette::getcolor(int n) {
  color x;
  if (n >= ncol) {printf("there aren't that many colors in the table\n");
                  return x; }
  x.red   = table[n].red ;
  x.green = table[n].green;
  x.blue  = table[n].blue ;
  return x;
}
void palette::brighten(float x) {
  int i;
  for (i = 0; i < ncol; i++) {
    table[i].brighten(x);
  }
}
void palette::brightness(float x) {
  int i;
  for (i = 0; i < ncol; i++) {
    table[i].brightness(x);
  }
}
void palette::darken(float x) {
  int i;
  for (i = 0; i < ncol; i++) {
    table[i].darken(x);
  }
}
void palette::negate(void) {
  int i;
  for (i = 0; i < ncol; i++) {
    table[i].negate();
  }
}
