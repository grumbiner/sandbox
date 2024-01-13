#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "icessmi.h"
#define NX_NORTH 304
#define NY_NORTH 448
#define polei_NORTH (3850/25)
#define polej_NORTH (5350/25)

#define FALSE (1==0)
#define TRUE (1==1)

/* Elements specific to the high resolution SSMI algorithm and processing */
/* Note that almost the entire program is identical to the low-res version */
/* This is necessarily the case as the goal is to produce something which */
/*   is maximally correspondant to the old processing, just higher resolution */

int max(int x, int y) ;
int min(int x, int y) ;
float hires(float *test, unsigned char *team, int nx, int ny, 
           int polei, int polej, unsigned char *mask, unsigned char *recomp,
           int *use) ;
void landuse(int *use, unsigned char *mask, unsigned char *team, 
             float *param, int nx, int ny, int polei, int polej, int latrange) ;
void regress(float *test, float *conc, int count, float *a, float *b, 
                                                float *r) ;

int main(int argc, char *argv[]) {
  FILE *fin_team, *fin_par, *fin_mask, *fout;
  unsigned char mask[NY_NORTH][NX_NORTH], team[NY_NORTH][NX_NORTH];
  unsigned char tmask[NY_NORTH][NX_NORTH], tteam[NY_NORTH][NX_NORTH];
  unsigned char recomp[NY_NORTH][NX_NORTH];
  int use[NY_NORTH][NX_NORTH];
  float param[NY_NORTH][NX_NORTH];
  int i, j;

  fin_team = fopen(argv[1], "r");
  fin_par  = fopen(argv[2], "r");
  fin_mask = fopen(argv[3], "r");
  fout     = fopen(argv[4], "w");
  fread(tmask, sizeof(unsigned char), NX_NORTH*NY_NORTH, fin_mask);
  fread(tteam, sizeof(unsigned char), NX_NORTH*NY_NORTH, fin_team);
  fread(param, sizeof(float), NX_NORTH*NY_NORTH, fin_par);


  /* Need to flip j in mask and team */
  for (j = 0; j < NY_NORTH; j++) {
  for (i = 0; i < NX_NORTH; i++) {
    mask[j][i] = tmask[NY_NORTH - 1 - j][i];
    team[j][i] = tteam[NY_NORTH - 1 - j][i]; 
  }
  }
    
  hires(&param[0][0], &team[0][0], NX_NORTH, NY_NORTH, polei_NORTH, polej_NORTH,
        &mask[0][0], &recomp[0][0], &use[0][0]);

  #ifdef VERBOSE
  for (j = 0; j < NY_NORTH; j++) {
  for (i = 0; i < NX_NORTH; i++) {
    if (mask[j][i] != 157 && mask[j][i] != 195 && 
        team[j][i] != 157 && team[j][i] != 168 && team[j][i] != 0) {
    printf("res %3d %3d %7.3f %3d %3d\n",(int) team[j][i], (int) mask[j][i], param[j][i], (int) recomp[j][i], use[j][i]);
    }
  }
  }
  #endif

  fwrite(&recomp[0][0], sizeof(unsigned char), NX_NORTH*NY_NORTH, fout);

  return 0;
}

float hires(float *param, unsigned char *team, int nx, int ny, int polei, int polej, unsigned char *mask, unsigned char *recomp, int *use) {
  float *conc, *test, a, b, r;
  float fconc;
  int i, count = 0, tconc;
  int latrange = 4*30;  

/* find the points which are usable -- good tb's, not too close to land */
  conc = (float *) malloc(nx*ny*sizeof(float));
  test = (float *) malloc(nx*ny*sizeof(float));

  landuse(use, mask, team, param, nx, ny, polei, polej, latrange);

  #ifdef VERBOSE
  printf("Entered hires and completed malloc and landuse\n"); fflush(stdout);
  #endif

  for (i = 0; i < nx*ny; i++) {
    if (use[i]) {
      conc[count] = team[i];
      test[count] = param[i];
      count += 1;
    }
  }
  printf("hires count = %d\n", count); fflush(stdout);

/* Perform the linear regression */
  regress(test, conc, count, &a, &b, &r);
  printf("regression, %f %f  %f\n",a,b,r); fflush(stdout);

  for (i = 0; i < nx*ny; i++) {
    tconc = (int) team[i];
    if (tconc < MAX_CONC && param[i] != -1.) {
      fconc = a + b * param[i];
      if (fconc < MIN_CONC) fconc = 0.;
      if (fconc > MAX_CONC) {
        printf("too high %d  %f %f\n",i, fconc, param[i]);
        fconc = (float) NO_DATA;
      }
      if (fconc > 100.) fconc = 100.;
      recomp[i] = (unsigned char) (0.5 + fconc);
    }
    else {
      recomp[i] = NO_DATA;
    }
  }

  free(conc);
  free(test);
  /* Return the correlation as a quality parameter */
  return r;
}

void landuse(int *use, unsigned char *mask, unsigned char *team, 
             float *param, int nx, int ny,
             int polei, int polej, int latrange) {
  int i, j;
  int k, l, index, tindex, tconc;
  int range = 3;

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
    tconc = (int) team[index];
    if (tconc > 99 || tconc < MIN_CONC || 
        mask[index] == LAND || mask[index] == COAST ||
        param[index] <= 0. ) {
      use[index] = FALSE;
    #ifdef VERBOSE2
      printf("unused %3d %3d %f\n",(int) tconc, (int) mask[index], param[index]);
    #endif
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
        ((i-polei)*(i-polei)+(j-polej)*(j-polej)) > latrange*latrange) {
      use[index] = FALSE;
    }
    #ifdef VERBOSE2
    if (use[index]) {
      printf("%3d %3d  %3d %3d\n",i,j,team[index], mask[index]);
    }
    #endif

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
     printf("sx, sy, sxx, sxy %f %f %f %f  count %d\n",sx, sy, sxx, sxy, count);
     a = 0;
     b = 0;
     r = 0;
     return;
   }

   *b = (count *sxy - sx*sy)/det;
   *a = (sy - *b*sx)/count;
   *r = (count *sxy - sx*sy) / sqrt(det) / sqrt(count*syy - sy*sy) ;
   printf("In regress, a, b, r = %f %f %f\n",*a, *b, *r); fflush(stdout);

   sx = 0.;
   sxx = 0.;
   for (i = 0; i < count; i++) {
     sx  +=  *a + (*b)*test[i] - conc[i];
     sxx += (*a + (*b)*test[i] - conc[i])*(*a + (*b)*test[i]-conc[i]);
   }
   printf("regress sample bias, rms %f %f\n", sx/count, sqrt(sxx/count) ); 
   fflush(stdout); 

   for (i = 0; i < count; i++) {
     tconc = *a + (*b)*test[i];
     if (fabs(tconc - conc[i]) > 3.*sqrt(sxx/count) ) {
        printf("delta %f %f %f  %5.1f %5.1f %6.1f\n",*a, *b, test[i], conc[i], tconc, tconc - conc[i]);
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
