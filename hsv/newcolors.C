#include "color.h"
#include "ncepgrids.h"

//algorithm from http://www.rapidtables.com/convert/color/rgb-to-hsv.htm
void rgb_to_hsv(point3<unsigned char> &rgb, point3<float> &hsv) {
  int cmax, cmin, delta;
  unsigned char r = rgb.i, g = rgb.j, b = rgb.k;
  float h, s, v;

//* Scale rgb to [0..1] (e.g. from 0..255)
//Cmax = max(r,g,b)
//Cmin = min(r,g,b)
//delta = Cmax - Cmin
  cmax = max(r,g);  cmax = max(cmax,b);
  cmin = min(r,g);  cmin = min(cmin,b);
  delta = cmax - cmin;


//Saturation 
//= 0 if cmax = 0, else
//S = delta / cmax
//
//Value
//V = cmax
  v = cmax / 255.0;
  if (cmax == 0) {
    s = 0;
  }
  else {
    s = (float)delta / (float)cmax;
  }

//H = 0 if delta = 0
//if cmax = r, H = 60degrees * (g-b)/delta (mod6)
//if cmax = g, H = 60degrees * (b-r)/delta + 2)
//if cmax = b, H = 60degrees * (r-g)/delta + 4)
  if (delta == 0) {
    h = 0;
  }
  else if (cmax == r) {
    float tmp = ((float)(g-b)/(float)delta);
    tmp -= (6.* (int)(tmp/6.));
    h = 60. * tmp;
  }
  else if (cmax == g) {
    h = 60. * ((float)(b-r)/(float)delta + 2);
  }
  else if (cmax == b) {
    h = 60. * ((float)(r-g)/(float)delta + 4);
  }
  else {
    printf("cmax != any of r,g,b -- %d vs. %d %d %d\n",cmax, r, g, b);
    h = 0;
  }
  if (h < 0) h += 360.;

  hsv.i = h;
  hsv.j = s;
  hsv.k = v;
  return;
  
//
}


void hsv_to_rgb(point3<unsigned char> &rgb, point3<float> &hsv) {
  float c, m, x;
  float h, s, v;
  float r, g, b, tmp;

  h = hsv.i;
  s = hsv.j;
  v = hsv.k;

//* Converting hsv to rgb
//let 
//C = V*S
//m = V-C
//X = C*(1- | (H/60degrees)mod 2 - 1 |)

  c = v * s;
  m = v - c;

  tmp = h / 60.0;
  tmp -= (2.* (int)(tmp/2.));
  x = c * (1. - fabs(tmp - 1.) );
  //printf("c, m, x: %f %f %f\n",c,m,x);

  if (h < 0) {
    printf("error, h < 0\n");
    return;
  }
  else if (h < 60) {
    r = c;  g = x; b = 0;
  }
  else if (h < 120) {
    r = x; g = c; b = 0;
  }
  else if (h < 180) {
    r = 0; g = c; b = x;
  }
  else if (h < 240) {
    r = 0; g = x; b = c;
  }
  else if (h < 300) {
    r = x; g = 0; b = c;
  }
  else if (h < 360) {
    r = c; g = 0; b = x;
  }
  else {
    printf("h out of range, %f\n",h);
    r = 0; g = 0; b = 0;
  }
  r += m;
  g += m;
  b += m;
  rgb.i = rint(255*r);
  rgb.j = rint(255*g);
  rgb.k = rint(255*b);

//H in      -> color' 
//[0,60)    -> (C,X,0)
//[60,120)  -> (X,C,0)
//[120,180) -> (0,C,X)
//[180,240) -> (0,X,C)
//[240,300) -> (X,0,C)
//[300,360) -> (C,0,X)
//
//then (r,g,b) = (color' + m)*255 

}

int main(int argc, char *argv[]) {
  palette<unsigned char> reference(19,65);
  palette<unsigned char> half(19,65);
  palette<unsigned char> joint(38,65);
  point3<unsigned char> rgb1, rgb2;
  point3<float> hsv;
  global_12th<float> sst, grad;
  FILE *fin;

  printf("go through palette\n"); fflush(stdout);
  for (int i = 0; i < reference.ncol; i++) {
    reference.get_color(i, rgb1);
    printf("%2d %3d %3d %3d  ",i,rgb1.i, rgb1.j, rgb1.k);

    rgb_to_hsv(rgb1, hsv);
    printf("%6.2f %5.3f %5.3f  ",hsv.i, hsv.j, hsv.k);

    hsv.j /= 4.;
    hsv_to_rgb(rgb2, hsv);
    printf("%3d %3d %3d\n",rgb2.i, rgb2.j, rgb2.k);

    half.set_color(i, rgb2);
    joint.set_color(2*i,rgb1);
    joint.set_color(2*i+1,rgb2); // paler
  }

  fin = fopen(argv[1],"r");
  sst.binin(fin);
  fclose(fin);
  printf("stats %f %f %f\n",sst.gridmax(), sst.gridmin(), sst.average());

  float flag = -99.;
  gradsq(sst, grad, flag);
  printf("gradsq %e %e %e %e\n",grad.gridmax(), grad.gridmin(), grad.average(), grad.rms() );

  //sst.scale();
  if (sst.gridmin() > 200) { 
    sst -= 270.;
    sst /= (sst.gridmax() - sst.gridmin());
    sst *= reference.ncol;
  }
  else if (sst.gridmax() == 1.) {
    sst *= reference.ncol - 1;
  }
  
  for (int i = 0; i < sst.xpoints()*sst.ypoints() ; i++) {
    if (grad[i] > 1.e-9) {
      sst[i] = 2*(int)sst[i];
    }
    else {
      sst[i] = (int)sst[i]*2 + 1;
    }
  }

  //sst.xpm("orig.xpm",1,reference);
  //sst.xpm("half.xpm",1,half);
  sst.xpm("joint.xpm",1,joint);


  return 0;
}
