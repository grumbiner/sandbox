#include "ncepgrids.h"

// Test pairs of input fiels for spinup issues, i.e., they're
//   different, but we expect pretty similar
// 23 March 2007

// These might be suitable for inclusion in gaussian class?

#define T382 663552
#define T254 294912
#define T190 165888
#define T170 131072
#define T126  72960
#define T62   18048

#define MAXAVG 5

void compare(gaussian<float> &x, gaussian<float> &y) ;

int main(int argc, char *argv[]) {
  float *ftmp;
  int npts, maxpts;
  FILE *fin;
  gaussian<float> *t382;
  gaussian<float> *t254;
  int i, k;
  mvector<int> sizes(54);


  t382 = new gaussian<float>[MAXAVG](382);
  t254 = new gaussian<float>[MAXAVG](254);

  maxpts = T382;
  ftmp = new float[maxpts];

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    npts = fread(ftmp, sizeof(float), maxpts, fin);
    fclose(fin);
    switch(npts) {
       case T382 :
         sizes[i-1] = 382;
         for (k = 0; k < T382; k++) {
           t382[i-1][k] = ftmp[k];
         }
         break;
       case T254 :
         sizes[i-1] = 254;
         for (k = 0; k < T254; k++) {
           t254[i-1][k] = ftmp[k];
         }
         break;
       default :
         printf("Encountered an unknown size grid! %d points\n",npts);
         fflush(stdout);
    } // end of switch

  } // end of looping over arguments

  bool same = true;
  for (i = 1; i < argc - 1; i++) {
    same = same && (sizes[i] == sizes[0]); 
  }
  if (!same) {
    printf("Different sizes present ! \n");
    for (i = 0; i < argc - 1; i++) {
      printf("%d  size %d  \n",i,sizes[i]);
    }
    return 1;
  }

  for (i = 1; i < argc - 1; i++) {
    if (sizes[0] == 382) {
      compare(t382[0], t382[1]);
    }
    else if (sizes[0] == 254) {
      compare(t254[0], t254[1]);
    }
    else {
       printf("somehow ran out of range in size, size 0 = %d\n",sizes[0]);
    }
  }


  return 0;
}
void compare(gaussian<float> &x, gaussian<float> &y) {
  gaussian<float> delta(x.xpoints(), x.ypoints() );
  ijpt loc;

  delta = y;
  delta -= x;
  printf("delta max, min, avg, rms %f %f %f %f\n",delta.gridmax(), delta.gridmin(), delta.average(), delta.rms() );

  return;
}
