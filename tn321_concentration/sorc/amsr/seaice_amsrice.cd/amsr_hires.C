#include <cmath>
#include <cstdlib>
#include <cstdio>

#include "amsrice.h"

//old#ifndef IBM
//old  #define FALSE (1==0)
//old  #define TRUE  (1==1)
//old#endif

/* Elements specific to the high resolution AMSR algorithm and processing */
/* Note that almost the entire program is identical to the low-res version */
/* This is necessarily the case as the goal is to produce something which */
/*   is maximally correspondant to the old processing, just higher resolution */
/* ************************************************ */
int imax(int x, int y);
int imax(int x, int y) {
  if (x > y) {
    return x;
  }
  return y;
}
int imin(int x, int y);
int imin(int x, int y) {
  if (x < y) {
    return x;
  }
  return y;
}
/* ************************************************ */
void landuse(bool *use, unsigned char *mask, amsr *obs, 
             const int nx, const int ny,
             const int polei, const int polej, const int latrange) ;
void regress(float *test, float *conc, int count, float *a, float *b, 
                                                float *r) ;

float hires(amsr *map, int nx, int ny, int polei, int polej, 
                      unsigned char *mask) {
  float *conc, *test, a, b, r;
  bool *use;
  float fconc;
  int i, count = 0, tconc, tconc2;
  int latrange = 8*30;  
  float th, tv;

/* find the points which are usable -- good tb's, not too close to land */
  //olduse  = (bool *)   malloc(nx*ny*sizeof(int));
  //oldconc = (float *) malloc(nx*ny*sizeof(float));
  //oldtest = (float *) malloc(nx*ny*sizeof(float));
  use  = new bool[nx*ny];
  conc = new float[nx*ny];
  test = new float[nx*ny];
  if (use == (bool*) NULL) {
    printf("failed to new use in hires\n");
    return -1;
  }
  if (conc == (float*) NULL) {
    printf("failed to new conc in hires\n");
    return -1;
  }
  if (test == (float*) NULL) {
    printf("failed to new test in hires\n");
    return -1;
  }

  landuse(use, mask, map, nx, ny, polei, polej, latrange);


  for (i = 0; i < nx*ny; i++) {
    if (use[i]) {
      th = (float) map[i].t89h;
      tv = (float) map[i].t89v;

      if (tv > th) {
        conc[count] = map[i].conc_bar;
        test[count] = sqrt( fabs(tv*tv - th*th) );
        count += 1;
      }
    }
  }
  printf("hires count = %d\n", count); fflush(stdout);

/* Perform the linear regression */
  regress(test, conc, count, &a, &b, &r);
  printf("regression, %f %f  %f\n",a,b,r); fflush(stdout);


  for (i = 0; i < nx*ny; i++) {
    tconc = (int) map[i].conc_bar;
    tconc2 = (int) map[i].bar_conc;
    if (tconc != WEATHER && tconc != BAD_DATA && tconc != NO_DATA &&
        tconc2 != WEATHER && tconc2 != BAD_DATA && tconc2 != NO_DATA) {
      th = (float) map[i].t89h;
      tv = (float) map[i].t89v;
      fconc = a + b * sqrt(fabs(tv*tv - th*th) ); 
      if (fconc < MIN_CONC) fconc = 0;
      if (fconc > MAX_CONC) fconc = BAD_DATA;
      map[i].hires_conc = (unsigned char) (0.5 + fconc);
    }
    else {
      if (tconc2 == WEATHER || tconc == WEATHER) {
        map[i].hires_conc = WEATHER;
      }
      else {
        map[i].hires_conc = (unsigned char) tconc;
      }
      if (map[i].hires_conc != map[i].conc_bar) {
         printf("wild failure in hires %d vs. %d \n",(int) map[i].hires_conc, 
              (int) map[i].conc_bar );
      }
    }
  }

  free(use);
  free(conc);
  free(test);
  /* Return the correlation as a quality parameter */
  return r;
}

void landuse(bool *use, unsigned char *mask, amsr *obs, 
             const int nx, const int ny,
             const int polei, const int polej, const int latrange) {
  int i, j;
  int k, l, index, tindex, tconc;
  int range = 5;

  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
    index = i + j*nx;
    use[index] = true;
    tconc = (int) obs[index].conc_bar;
    if (tconc == WEATHER || tconc == BAD_DATA || tconc == NO_DATA ||
        mask[index] == LAND || mask[index] == COAST || tconc > 99 ) {
      use[index] = false;
    }
    else {
      for (l = imax(0,j - range); l <= imin(ny-1,j + range); l++) {
      for (k = imax(0,i - range); k <= imin(nx-1,i + range); k++) {
        tindex = k + l*nx;
        if (mask[tindex ] == LAND || mask[tindex ] == COAST) {
          use[index] = false;
          continue;
        }
      }
      }
    }
/* Now ensure that only high latitude values are used -- exclude Gulf
   Stream storms, etc. */
    if ( use[index] &&
        ((i-polei)*(i-polei)+(j-polej)*(j-polej)) > latrange*latrange) {
      use[index] = false;
    }

  }
  }    

  return;
}
void regress(float *test, float *conc, int count, float *a, float *b, 
             float *r) {
   double sx = 0., sy = 0., sxy = 0., sxx = 0., syy = 0., det;
   float tconc;
   int i;
 
   for (i = 0; i < count; i++) {
      sx += test[i];
      sy += conc[i];
      sxy += test[i]*conc[i];
      sxx += test[i]*test[i];
      syy += conc[i]*conc[i];
   }
   det = count * sxx - sx*sx;
   if (det == 0.) {
     printf("Singular in regress, cannot construct regression.\n");
     *a = 0.0;
     *b = 0.0;
     *r = 0.0;
     return;
   }

   *b = (count *sxy - sx*sy)/det;
   *a = (sy - *b*sx)/count;
   *r = (count *sxy - sx*sy) / sqrt(det) / sqrt(count*syy - sy*sy) ;
   #ifdef DEBUG
   printf("In regress, a, b, r = %f %f %f\n",*a, *b, *r); fflush(stdout);
   #endif

   sx = 0.;
   sxx = 0.;
   for (i = 0; i < count; i++) {
     sx  +=  *a + (*b)*test[i] - conc[i];
     sxx += (*a + (*b)*test[i] - conc[i])*(*a + (*b)*test[i]-conc[i]);
   }
   printf("sample bias, rms %f %f\n", sx/count, sqrt(sxx/count) ); 
   fflush(stdout); 

   for (i = 0; i < count; i++) {
     tconc = *a + (*b)*test[i];
     if (fabs(tconc - conc[i]) > 3.*sqrt(sxx/count) ) {
        printf("%f %f %f  %5.1f %5.1f %6.1f big delta\n",*a, *b, 
             test[i], conc[i], tconc, tconc - conc[i]);
        fflush(stdout);
     }
   }

   return;
}

int min(int x, int y) {
  if (x < y) { return x; }
  else { return y;}
}
