#include <stdio.h>
#include <math.h>

#include "grid_math.h"
#include "mvector.h"

#define NX 1285
#define NY (4683/1)
#define BANDS 8

template <class T, class U>
void retype(mvector<T> &x, mvector<U> &y);

float dot(mvector<float> &x, mvector<float> &y) ;

template <class T>
float shaninfo(mvector<T> &x) ;


int main(void) {
  mvector<float> lats(NX*NY), lons(NX*NY);
  mvector<short int> tmp(NX*NY);
  mvector<float> band1(NX* NY), band2(NX* NY), band3(NX*NY), band4(NX*NY);
  mvector<float> band5(NX* NY), band6(NX* NY), band7(NX*NY), band8(NX*NY);
  mvector< mvector<float> * > all(8);
  FILE *fin;
  int i, j;

  fin = fopen("S1999071174340_412nm_nav_1285x9366_4f.flat","r");
  if (fin == (FILE*) NULL) {
    printf("Failed to open navigation file\n");
    return 1;
  }
  lats.binin(fin);
  lons.binin(fin);
  fclose(fin);
  
  fin = fopen("S1999071174340_412nm_1285x4683_4f.flat","r");
  if (fin == (FILE*) NULL) {
    printf("Failed to open band1 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band1);
  fclose(fin);

  fin = fopen("S1999071174340_443nm_1285x4683_4f.flat","r");
  if (fin == (FILE*) NULL) {
    printf("Failed to open band2 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band2);
  fclose(fin);

  fin = fopen("S1999071174340_490nm_1285x4683_2i.flat","r");
  if (fin == (FILE *) NULL) {
    printf("failed to open band3 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band3);
  fclose(fin);


  fin = fopen("S1999071174340_510nm_1285x4683_2i.flat","r");
  if (fin == (FILE *) NULL) {
    printf("failed to open band4 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band4);
  fclose(fin);
 

  fin = fopen("S1999071174340_555nm_1285x4683_2i.flat","r");
  if (fin == (FILE *) NULL) {
    printf("failed to open band5 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band5);
  fclose(fin);
 
  fin = fopen("S1999071174340_670nm_1285x4683_2i.flat","r");
  if (fin == (FILE *) NULL) {
    printf("failed to open band6 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band6);
  fclose(fin);
 
  fin = fopen("S1999071174340_765nm_1285x4683_2i.flat","r");
  if (fin == (FILE *) NULL) {
    printf("failed to open band7 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band7);
  fclose(fin);
 
  fin = fopen("S1999071174340_865nm_1285x4683_2i.flat","r");
  if (fin == (FILE *) NULL) {
    printf("failed to open band8 file\n");
    return 1;
  }
  tmp.binin(fin);
  retype(tmp, band8);
  fclose(fin);
 

  //printf("lats grid max, min, avg %f %f %f\n", lats.gridmax(), lats.gridmin(), lats.average() );
  //printf("lons grid max, min, avg %f %f %f\n", lons.gridmax(), lons.gridmin(), lons.average() );
  
  all[0] = &band1;
  all[1] = &band2;
  all[2] = &band3;
  all[3] = &band4;
  all[4] = &band5;
  all[5] = &band6;
  all[6] = &band7;
  all[7] = &band8;

  for (i = 0; i < BANDS; i++) {
    printf("band %d stats %f %f\n",i+1, all[i]->average(), shaninfo(*all[i]) );
    fflush(stdout);
    *all[i] -= all[i]->average();
  }
//Simple minded -- subtract i=0 from each of rest and recompute:
  i = 0;
  printf("band %d stats %f %f\n",i+1, all[i]->average(), shaninfo(*all[i]) );
  for (i = 1; i < BANDS; i++) {
    *all[i] -= *all[0];
    printf("band %d stats %f %f\n",i+1, all[i]->average(), shaninfo(*all[i]) );
    fflush(stdout); 
  }


  for (i = 0; i < BANDS-1; i++) {
  for (j = i; j < BANDS;   j++) { 
    printf("cors %d %d  %f\n",i,j, dot(*all[i], *all[j]) / 
           sqrt(dot(*all[i], *all[i])*dot(*all[j],*all[j]) )  );
    fflush(stdout);
  //printf("band1*2 / band1*band1 %f\n", dot(*all[0], band2) / 
   //      sqrt(dot(*all[0], *all[0])*dot(band2,band2) )  );
  }
  }


  return 0;
}
template <class T, class U>
void retype(mvector<T> &x, mvector<U> &y) {
  int i;
  for (i = 0; i < x.xpoints(); i++) {
    y[i] = (U) x[i];
  }
  return;
}
float dot(mvector<float> &x, mvector<float> &y) {
  int i; 
  double sum = 0.0;
  for (i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return (float) sum;
}
// Assume unit precision
template<class T>
float shaninfo(mvector<T> &x) {
  int i;
  mvector<long int> *count;
  mvector<int> delta(x.xpoints());
  double tempor, total=0.0;
  float xmax = x.maximum(), xmin = x.minimum();
   
  count = new mvector<long int>(xmax - xmin + 1);
  for (i = 0; i < count->xpoints(); i++) {
    count->operator[]( i )  = 0;
  }

  retype(x, delta);
  delta -= (int)xmin;
  for (i = 0; i < delta.xpoints(); i++) {
    count->operator[]( delta[i] ) += 1;
  }

  for (i = 0; i < count->xpoints(); i++) {
    if (count->operator[](i) != 0) {
      tempor = (float) count->operator[](i) / (float) x.xpoints();
      total += -tempor * log(tempor);
    }
    //printf("%5d %9d\n",i, count->operator[](i) );
  }
  total /= log(2.);
  return (float) total; 
}
