#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "icessmi.h"

#define FALSE (1==0)
#define TRUE (1==1)
#define MAX_CONC 128

/* Elements specific to the high resolution SSMI algorithm and processing */
/* Note that almost the entire program is identical to the low-res version */
/* This is necessarily the case as the goal is to produce something which */
/*   is maximally correspondant to the old processing, just higher resolution */

void landuse(int *use, unsigned char *mask, oldssmi *obs, int nx, int ny,
             int polei, int polej, int latrange) ;
void regress(float *test, float *conc, int count, float *a, float *b, 
                                                float *r) ;
int min(int x, int y) ;
int max(int x, int y) ;

float hires(oldssmi *map, int nx, int ny, int polei, int polej, 
            unsigned char *mask, unsigned char *hires_conc) {
  float *conc, *test, a, b, r;
  float c1, c2, c3, *q;
  int *use;
  float fconc;
  int i, count = 0, tconc;
  int latrange = 4*30;  
  float th, tv;

/* find the points which are usable -- good tb's, not too close to land */
  use  = malloc(nx*ny*sizeof(int));
  conc = malloc(nx*ny*sizeof(float));
  test = malloc(nx*ny*sizeof(float));

  landuse(use, mask, map, nx, ny, polei, polej, latrange);

  #ifdef VERBOSE
  printf("Entered hires and completed malloc and landuse\n"); fflush(stdout);
  #endif

  count = 0;
  for (i = 0; i < nx*ny; i++) {
    if (use[i]) {
      th = (float) map[i].t37h;
      tv = (float) map[i].t37v;
      th /= 100.;
      tv /= 100.;
      printf("%f %f\n",th, tv);

      conc[count] = map[i].conc_bar;
      test[count] = sqrt( fabs(tv*tv - th*th) );
      count += 1;
    }
  }
  printf("hires count = %d\n", count); fflush(stdout);

/* Perform the linear regression */
  regress(test, conc, count, &a, &b, &r);
  printf("regression, %f %e  %f\n",a,b,r); fflush(stdout);


  for (i = 0; i < nx*ny; i++) {
    tconc = (int) map[i].conc_bar;
    if (tconc != WEATHER && tconc != BAD_DATA && tconc != NO_DATA &&
        tconc != LAND && tconc != COAST ) {
      th = (float) map[i].t37h;
      tv = (float) map[i].t37v;
      th /= 100.;
      tv /= 100.;
      fconc = a + b * sqrt(fabs(tv*tv - th*th) ); 
      if (fconc < MIN_CONC) fconc = 0;
      if (fconc > MAX_CONC) fconc = MAX_CONC;
      hires_conc[i] = (unsigned char) (0.5 + fconc);
    }
    else {
      hires_conc[i] = (unsigned char) tconc;
      if (hires_conc[i] != map[i].conc_bar) {
         printf("wild failure in hires %d vs. %d \n",(int) hires_conc[i], 
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

void landuse(int *use, unsigned char *mask, oldssmi *obs, int nx, int ny,
             int polei, int polej, int latrange) {
  int i, j;
  int k, l, index, tindex, tconc;
  int range = 5;

  #ifdef VERBOSE
  printf("Entered landuse\n"); fflush(stdout);
  #endif

  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
    #ifdef VERBOSE2
    printf("landuse %d %d\n",i,j); fflush(stdout);
    #endif
    index = i + j*nx;
    use[index] = TRUE;
    tconc = (int) obs[index].conc_bar;
    if (tconc == WEATHER || tconc == BAD_DATA || tconc == NO_DATA ||
        tconc == LAND || tconc == COAST ||
        mask[index] == LAND || mask[index] == COAST ) {
      use[index] = FALSE;
    }
    else {
      for (l = max(0,j - range); l < min(ny-1,j + range); l++) {
      for (k = max(0,i - range); k < min(nx-1,i + range); k++) {
        tindex = k + l*nx;
        if (mask[tindex] == LAND || mask[tindex] == COAST) {
          use[index] = FALSE;
          continue;
        }
      }
      }
    }
/* Now ensure that only high latitude values are used -- exclude Gulf
   Stream storms, etc. */
    if ( use[index] &&
        ((i-polei)*(i-polei)+(j-polej)*(j-polej)) < latrange*latrange) {
      use[index] = FALSE;
    }

  }
  }    

  #ifdef VERBOSE
  printf("leaving landuse\n"); fflush(stdout);
  #endif
  return;
}
void regress(float *test, float *conc, int count, float *a, float *b, 
             float *r) {
   float sx = 0., sy = 0., sxy = 0., sxx = 0., syy = 0., det;
   float tconc;
   int i;
 
   printf("in regress, working with %d points\n",count);
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
     a = 0;
     b = 0;
     r = 0;
     return;
   }

   *b = (count *sxy - sx*sy)/det;
   *a = (sy - *b*sx)/count;
   *r = (count *sxy - sx*sy) / sqrt(det) / sqrt(count*syy - sy*sy) ;
   printf("In regress, a, b, r = %f %e %f\n",*a, *b, *r); fflush(stdout);

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
        printf("%f %f %f  %5.1f %5.1f %6.1f\n",*a, *b, test[i], conc[i], tconc, tconc - conc[i]);
        fflush(stdout);
     }
   }

   return;
}


int min(int x, int y) {
  if (x < y) { return x; }
  else { return y;}
  return y;
}
int max(int x, int y) {
  if (x > y) { return x; }
  else { return y;}
  return y;
}
