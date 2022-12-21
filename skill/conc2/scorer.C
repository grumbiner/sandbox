// Program to score sea ice forecasts

#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "ncepgrids.h"

void scorer(grid2<float> &x, grid2<float> &y, mvector<float> &scores);

extern "C" float iagree_(float *r, float *x, int &n);
extern "C" float correl_(float *r, float *x, int &n, float &r2, float &xmean,
                        float &ymean, float &sig2x, float &sig2y);
extern "C" float sumx_(float *r, int &n);
extern "C" float sumx2_(float *r, int &n);

int main(int argc, char *argv[]) {
  northgrid<float> concs[2];
  northgrid<float> land;
  mvector<float> scores(9);
  int fin;
  int i, j, k;

  fin = open(argv[1], O_RDONLY);
  if (fin == -1 ) {
    printf("Failed to open the climatology file\n");
    return 1;
  }

  for (i = 0; i < 365; i++) {
    lseek(fin, i*sizeof(float)*land.xpoints()*land.ypoints(), SEEK_SET);
    //concs[0].binin(fin);
    read(fin, concs[0].grid, sizeof(float)*concs[0].xpoints()*concs[0].ypoints() );
    //printf("%3d average %f\n",i, concs[0].average() );
    for (j = i+1; j < i+183; j++) {
      lseek(fin, (j%365)*sizeof(float)*land.xpoints()*land.ypoints(), SEEK_SET);
      //concs[1].binin(fin);
    read(fin, concs[1].grid, sizeof(float)*concs[1].xpoints()*concs[1].ypoints() );
      
      scorer(concs[0], concs[1], scores);
      printf("%3d %3d ", i, j);
      for (k = 0; k < scores.xpoints(); k++) {
        printf("%f ",scores[k]);
      }
      printf("\n");

    }
  } 

  return 0;
}

void scorer(grid2<float> &x, grid2<float> &y, mvector<float> &score) {
  float x1[x.xpoints()*x.ypoints() ];
  float x2[x.xpoints()*x.ypoints() ];
  float delta[x.xpoints()*x.ypoints() ];
  int count=0;
  int i; 
  int a11=0, a12=0, a21=0, a22=0;
  float level = 50./100.;
  float r2, xmean, ymean, sig2x, sig2y;

  for (i = 0; i < x.xpoints()*x.ypoints() ; i++) {
     if (x[i] <= 128/100. && y[i] <= 128/100. ){
       x1[count] = x[i];
       x2[count] = y[i];
       delta[count] = x[i] - y[i];
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
//   score[0] = (float) a11;
//   score[1] = (float) a12;
//   score[2] = (float) a21;
//   score[3] = (float) a22;

//Score 4 is the index of agreement:
     score[4] = iagree_(x1, x2, count);

// Score 5, 6, 7 are delta statistics:
   score[5] = sumx_(delta, count);
   score[6] = sumx2_(delta, count);
   score[7] = (count*score[6] - score[5]*score[5]) / (count*count - count);
   score[5] /= (float) count;
   score[6] = sqrt(score[6]/(float) count);
   score[7] = sqrt(score[7]); 
     
// score 8 is correlation (not squared):
   correl_(x1, x2, count, r2, xmean, ymean, sig2x, sig2y);
   score[8] = r2;

  return;
}

