#include <stdio.h>
// Perform intersatellite regression on SSMI (13, 14, 15)
// Full code version 21 April 2004
// Robert Grumbine

#include "mvector.h"
#include "ncepgrids.h"
#include "ssmiclass.h"

#define POINTS 40000
#define WATER 0

void regress(mvector<ssmipt> &first, mvector<ssmipt> &second, int index) ;
void correl(mvector<float> &pred, mvector<float> &conc, float &r,
            float &a, float &b) ;
float dot(mvector<float> &x, mvector<float> &y) ;

int paramcount = 0;

int main(int argc, char *argv[]) {
  southgrid<ssmipt> f13, f14, f15;
  mvector<ssmipt> testf13(POINTS), testf14(POINTS), testf15(POINTS);
  mvector<ssmipt> x;
  southgrid<unsigned char> mask;
  southgrid<bool> usable;
  FILE *fin;
  ijpt loc;
  int count, range, i = 0;

  fin = fopen(argv[1],"r");
  mask.binin(fin);
  fclose(fin);
  fin = fopen(argv[2],"r");
  f13.binin(fin);
  fclose(fin);
  fin = fopen(argv[3],"r");
  f14.binin(fin);
  fclose(fin);
  fin = fopen(argv[4],"r");
  f15.binin(fin);
  fclose(fin);

// Set up the mask of regressable points -- this could be read in instead.
  range = atoi(argv[5]);
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
     if (mask.anyof(WATER, range, loc) > 0) {
       usable[loc] = false;
     }
     else {
       usable[loc] = true;
       i += 1;
     }
  }
  }
  printf("range = %d\n",range);
  printf("found %d points far enough from water\n", i);
  if (i == 0) {
    printf("there are no points that can be used for regression\n");
    return -1;
  }



// Now look for all 3 having valid information and being on usable point
  count = 0;
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    if (f13[loc].obs.count > 0 &&
        f14[loc].obs.count > 0 &&
        f15[loc].obs.count > 0 && usable[loc] ) {
      testf13[count] = f13[loc];
      testf14[count] = f14[loc];
      testf15[count] = f15[loc];
      count += 1;
    }
  }
  }
  printf("Found %d points to work with\n", count);
  if (count == 0) {
    printf("there are no points that can be used for regression\n");
    return -1;
  }
// Do this so that we don't falsely pretend that we've got POINTS points
  x.resize(count);

  x = testf13;
  testf13.resize(count);
  testf13 = x;
  x = testf14;
  testf14.resize(count);
  testf14 = x;
  x = testf15;
  testf15.resize(count);
  testf15 = x;
  printf("%d %d %d %d\n",x.xpoints(), testf13.xpoints(), 
                   testf14.xpoints(), testf15.xpoints() );

// 
  printf("13 vs 14\n");
  regress(testf13,testf14,T19V);
  regress(testf14,testf13,T19V);
  regress(testf13,testf14,T19H);
  regress(testf13,testf14,T22V);
  regress(testf13,testf14,T37V);
  regress(testf13,testf14,T37H);
  regress(testf13,testf14,T85V);
  regress(testf13,testf14,T85H);
  regress(testf13,testf14,T85H+1);
  regress(testf13,testf14,T85H+2);
//
  printf("\n\n13 vs 15\n");
  regress(testf13,testf15,T19V);
  regress(testf13,testf15,T19H);
  regress(testf13,testf15,T22V);
  regress(testf13,testf15,T37V);
  regress(testf13,testf15,T37H);
  regress(testf13,testf15,T85V);
  regress(testf13,testf15,T85H);
  regress(testf13,testf15,T85H+1);
  regress(testf13,testf15,T85H+2);
//
  printf("\n\n14 vs 15\n");
  regress(testf14,testf15,T19V);
  regress(testf14,testf15,T19H);
  regress(testf14,testf15,T22V);
  regress(testf14,testf15,T37V);
  regress(testf14,testf15,T37H);
  regress(testf14,testf15,T85V);
  regress(testf14,testf15,T85H);
  regress(testf14,testf15,T85H+1);
  regress(testf14,testf15,T85H+2);

  return 0;
}

void regress(mvector<ssmipt> &first, mvector<ssmipt> &second, int index) {
  mvector<float> x(first.xpoints() ), y(first.xpoints() );
  int i;
  float a, b, r;
  int count = first.xpoints();
  FILE *fout;
  char fname[90];
  

  switch (index) {
    case T19V:
      for (i = 0; i < count; i++) {
        x[i] = first[i].obs.t19v;
        y[i] = second[i].obs.t19v;
      }
      break;
    case T19H:
      for (i = 0; i < count; i++) {
        x[i] = first[i].obs.t19h;
        y[i] = second[i].obs.t19h;
      }
      break;
    case T22V:
      for (i = 0; i < count; i++) {
        x[i] = first[i].obs.t22v;
        y[i] = second[i].obs.t22v;
      }
      break;
    case T37V:
      for (i = 0; i < count; i++) {
        x[i] = first[i].obs.t37v;
        y[i] = second[i].obs.t37v;
      }
      break;
    case T37H:
      for (i = 0; i < count; i++) {
        x[i] = first[i].obs.t37h;
        y[i] = second[i].obs.t37h;
      }
      break;
    case T85V:
      for (i = 0; i < count; i++) {
        x[i] = first[i].obs.t85v;
        y[i] = second[i].obs.t85v;
      }
      break;
    case T85H:
      for (i = 0; i < count; i++) {
        x[i] = first[i].obs.t85h;
        y[i] = second[i].obs.t85h;
      }
      break;
    case T85H+1:
      for (i = 0; i < count; i++) {
        x[i] = ((float)first[i].obs.t85v *first[i].obs.t85v) - 
               ((float)first[i].obs.t85h *first[i].obs.t85h);
        y[i] = ((float)second[i].obs.t85v *second[i].obs.t85v) - 
               ((float)second[i].obs.t85h *second[i].obs.t85h);
      }
      break;
    case T85H+2:
      for (i = 0; i < count; i++) {
        x[i] = sqrt( (float) ((float)first[i].obs.t85v *first[i].obs.t85v) - 
                     ((float)first[i].obs.t85h *first[i].obs.t85h) );
        y[i] = sqrt( (float) ((float)second[i].obs.t85v *second[i].obs.t85v) - 
                     ((float)second[i].obs.t85h *second[i].obs.t85h) );
      }
      break;
    default:
      printf("illegal case in regress\n");
      return;
  }

  
  sprintf(fname,"matchup.%d",paramcount);
  paramcount += 1;
  fout = fopen(fname,"w");
  for (i = 0; i < count; i++) {
    fprintf(fout,"%f %f  %f  \n",x[i],y[i],y[i]-x[i]);
  }
  y -= x;
  printf("xbar %8.2f  ybar %8.2f  ",x.average(), y.average());
  fprintf(fout,"xbar %8.2f  ybar %8.2f  ",x.average(), y.average());
  //x -= x.average(); 
  //y -= y.average();
  correl(x, y, r, a, b);
  printf("a b r: %6.1f %8.6f %f\n",a, b, r);
  fprintf(fout,"a b r: %6.1f %8.6f %f\n",a, b, r);

  fclose(fout);
  return;
}

void correl(mvector<float> &pred, mvector<float> &conc, float &r,
            float &a, float &b) {
  mvector<float> x(pred.xpoints()), y(pred.xpoints());
  float sx, sy, sxy, sxx;
  float det;
  float n;

  n = (float) pred.xpoints();
  x = pred;
  y = conc;

  if (x.xpoints() != y.xpoints() ) {
    printf("data volume mismatch in correl: %d %d\n",x.xpoints(), y.xpoints() );
  }
  sx = x.average()*n;
  sy = y.average()*n;
  sxy = dot(x, y);
  sxx = dot(x, x);

  det = n*sxx - sx*sx;
  if (det == 0.) {
    printf("Singular, cannot work with\n");
    a = 0;  b = 0; r = 0;
    return ;
  }

  b = (n * sxy  - sx*sy) / det;
  a = y.average() - b*x.average();

  r = (n * sxy  - sx*sy) / sqrt(det) / sqrt(n*dot(y,y) - sy*sy);

  return;

}
float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0.0;
  //should test for xpoints being same on both
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i] * y[i] ;
  }
  return (float) sum;
}
