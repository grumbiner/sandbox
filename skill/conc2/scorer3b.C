// Program to score sea ice forecasts

#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "ncepgrids.h"

void scorer(grid2<float> &x, grid2<float> &y, grid2<float> &ref, mvector<float> &scores);
float murphy(float *f, float *obs, float *ref, int n) ;

extern "C" float iagree_(float *r, float *x, int &n);
extern "C" float correl_(float *r, float *x, int &n, float &r2, float &xmean,
                        float &ymean, float &sig2x, float &sig2y);
extern "C" float sumx_(float *r, int &n);
extern "C" float sumx2_(float *r, int &n);


int main(int argc, char *argv[]) {
  northgrid<float> concs[2], average;
  northgrid<float> land;
  mvector<float> scores(10);
  int finyear;
  int fin, fin2;
  int i, j, k;

  fin = open(argv[1], O_RDONLY);
  if (fin == -1 ) {
    printf("Failed to open the climatology file\n");
    return 1;
  }
  fin2 = open(argv[2], O_RDONLY);
  if (fin2 == -1 ) {
    printf("Failed to open the climatology file\n");
    return 1;
  }
  finyear = open(argv[3], O_RDONLY);
  if (finyear == -1) {
    printf("Failed to open the reference forecaster\n");
    return 2;
  }

  for (i = 0; i < 365; i++) {
    lseek(fin,  i*sizeof(float)*land.xpoints()*land.ypoints(), SEEK_SET);
    read(fin, concs[0].grid, sizeof(float)*concs[0].xpoints()*concs[0].ypoints() );
    lseek(fin2, i*sizeof(float)*land.xpoints()*land.ypoints(), SEEK_SET);
    read(fin2, concs[1].grid, sizeof(float)*concs[1].xpoints()*concs[1].ypoints() );

    for (j = i; j < i+183; j++) {
      lseek(finyear, (j%365)*sizeof(float)*land.xpoints()*land.ypoints(), SEEK_SET);
      read(finyear, average.grid, sizeof(float)*average.xpoints()*average.ypoints() );
      
      scorer(concs[0], concs[1], average, scores);
      printf("%3d %3d ", i, j);
      for (k = 0; k < scores.xpoints(); k++) {
        printf("%f ",scores[k]);
      }
      printf("\n");

    }
  } 

  return 0;
}

void scorer(grid2<float> &x, grid2<float> &y, grid2<float> &ref, mvector<float> &score) {
  float x1[x.xpoints()*x.ypoints() ];
  float x2[x.xpoints()*x.ypoints() ];
  float r[x.xpoints()*x.xpoints() ];
  float delta[x.xpoints()*x.ypoints() ];
  float delta1[x.xpoints()*x.ypoints() ];
  float delta2[x.xpoints()*x.ypoints() ];
  int count=0;
  int i; 
  int a11=0, a12=0, a21=0, a22=0;
  float level = 50./100.;
  float r2, xmean, ymean, sig2x, sig2y;

  for (i = 0; i < x.xpoints()*x.ypoints() ; i++) {
     if (( x[i] <= 128/100.) && 
         ( y[i] <= 128/100.) &&
         ( ref[i] <= 128/100.) ) {
       x1[count] = min(1.,x[i]);
       x2[count] = min(1.,y[i]);
       r[count]  = min(1.,ref[i]);
       delta[count] = x[i] - y[i];
       delta1[count] = x[i] - ref[i];
       delta2[count] = y[i] - ref[i];
       count += 1;
     }
  }

// Score 0-3 are threat type scores.
  for (i = 0 ; i < count; i++) {
     if (x1[i] > level) {
       if (x2[i] > level) {
         a11 += 1;
       }
       else {
         a12 += 1;
       }
     }
     else {
       if (x2[i] > level) {
         a21 += 1;
       }
       else {
         a22 += 1;
       }
     }
  }
     score[0] = (float) a11 / (float) (a11 + a21 + a12);
     score[1] = (float) (a11*a22 - a21*a12)/ (float) (a11*a22 + 1);
     score[2] = (float) (a11+a22) - sqrt((a11+a22)*(a11+a22)-
                                             4.*(a11*a22-a12*a22)) /
                (float) (a11+a22) + sqrt((a11+a22)*(a11+a22)-
                                             4.*(a11*a22-a12*a22));
     score[3] = (float) (a11+a22-a12-a21)/(float) (a11+a12+a21+a22);

//Score 4 is the index of agreement:
     score[4] = iagree_(delta1, delta2, count);

// Score 5, 6, 7 are delta statistics:
   score[5] = sumx_(delta, count);
   score[6] = sumx2_(delta, count);
   score[7] = (count*score[6] - score[5]*score[5]) / (count*count - count);
   score[5] /= (float) count;
   score[6] = sqrt(score[6]/(float) count);
   score[7] = sqrt(score[7]); 
     
// score 8 is correlation (not squared):
   correl_(delta1, delta2, count, r2, xmean, ymean, sig2x, sig2y);
   score[8] = r2;

// score 9 is murphy skill score:
   score[9] = murphy(x1, x2, r, count);

  return;
}
float murphy(float *f, float *obs, float *ref, int n) {
  int i;
  float s1 = 0, s2 = 0;
  for (i = 0; i < n; i++) {
     s1 += (f[i] - obs[i])*(f[i]-obs[i]);
     s2 += (ref[i] - obs[i])*(ref[i]-obs[i]);
  }
  return 1 - s1 / s2;
}
