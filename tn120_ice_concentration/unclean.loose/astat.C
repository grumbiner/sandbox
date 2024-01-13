#include <stdio.h>
#include <math.h>

#include "mvector.h"
#include "ncepgrids.h"

// Program to begin working on statistical regressions between derived
//   85 GHz parameters and ice concentrations.
// Robert Grumbine 16 October 1998

typedef struct {
  unsigned int t19v : 16;
  unsigned int t19h : 16;
  unsigned int t22v : 16;
  unsigned int t37v : 16;
  unsigned int t37h : 16;
  unsigned int t85v : 16;
  unsigned int t85h : 16;
  unsigned int conc_bar :  8;
  unsigned int bar_conc :  8; 
} ssmi_struct;

class ssmipt {
  public:
    ssmi_struct ssmi ;
    ssmipt();
    ssmipt & operator=(int );
    point3<float> polarization();
    point3<float> gradient();
    point3<float> differences();
};
ssmipt::ssmipt() {
  ssmi.t19v = 0;
  ssmi.t19h = 0;
  ssmi.t22v = 0;
  ssmi.t37v = 0;
  ssmi.t37h = 0;
  ssmi.t85v = 0;
  ssmi.t85h = 0;
  ssmi.conc_bar = 0;
  ssmi.bar_conc = 0;
}
ssmipt & ssmipt::operator=(int x) {
  ssmi.t19v = x;
  ssmi.t19h = x;
  ssmi.t22v = x;
  ssmi.t37v = x;
  ssmi.t37h = x;
  ssmi.t85v = x;
  ssmi.t85h = x;
  ssmi.conc_bar = x;
  ssmi.bar_conc = x;
  return *this;
}
point3<float> ssmipt::gradient() {
  point3<float> x;
  x.i = (float) ssmi.t22v / (float) ssmi.t19v;
  x.j = (float) ssmi.t37v / (float) ssmi.t19v;
  x.k = (float) ssmi.t85v / (float) ssmi.t19v;
  return x;
}
point3<float> ssmipt::polarization() {
  point3<float> x;
  x.i = (float) ssmi.t19v / (float) ssmi.t19h;
  x.j = (float) ssmi.t37v / (float) ssmi.t37h;
  x.k = (float) ssmi.t85v / (float) ssmi.t85h;
  return x;
}
point3<float> ssmipt::differences() {
  point3<float> x;
  x.i = (float) ssmi.t19v - (float) ssmi.t19h;
  x.j = (float) ssmi.t37v - (float) ssmi.t37h;
  x.k = (float) ssmi.t85v - (float) ssmi.t85h;
  return x;
}
  

void  cormat(mvector<float> &x1, mvector<float> &x2, mvector<float> &x3, 
             mvector<float> &x4, mvector<float> &x5, mvector<float> &x6, 
             mvector<float> &x7, mvector<float> &x8, int totpts) ;

extern "C" correl_(float *x, float *y, int *n, float *r2, \
                  float *xbar, float *ybar, float *sig2x, float *sig2y);

float dot(mvector<float> &x, mvector<float> &y);
void gsorthog(mvector<float> &x1, mvector<float> &x2);
void swap(mvector<float> &x1, mvector<float> &x2) ;


int main(int argc, char *argv[]) {
  FILE *fin, *f85;
  northgrid<ssmipt> basedata;
  northgrid<unsigned char> land;
  mvector<float> x1(basedata.xpoints()*basedata.ypoints() );
  mvector<float> x2(basedata.xpoints()*basedata.ypoints() );
  mvector<float> x3(basedata.xpoints()*basedata.ypoints() );
  mvector<float> x4(basedata.xpoints()*basedata.ypoints() );
  mvector<float> x5(basedata.xpoints()*basedata.ypoints() );
  mvector<float> x6(basedata.xpoints()*basedata.ypoints() );
  mvector<float> x7(basedata.xpoints()*basedata.ypoints() );
  mvector<float> x8(basedata.xpoints()*basedata.ypoints() );
  int totpts=0;

  ssmipt tmp1, tmp2;
  point3<float> grad, polar, diff;
  ijpt x;

  printf("Size of an ssmi: %d\n",sizeof(ssmi_struct) );
  printf("Size of an ssmipt: %d\n",sizeof(ssmipt) );

  fin = fopen("nland.map","r");
  if (fin == (FILE *) NULL ){
    printf("Failed to open the land file\n");
    return -1;
  }
  land.binin(fin);
  printf("land average %d %d %d\n",
    (int) land.average(), 
    (int) land.gridmax(), 
    (int) land.gridmin() );
  fclose(fin);


  f85 = fopen("f85","w");
  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file!\n");
    return 1;
  }

  printf("basedata nx, ny %d %d\n", basedata.xpoints(), basedata.ypoints() );
  basedata.binin(fin);

  for (x.j = 0; x.j < basedata.ypoints() ; x.j++) {
  for (x.i = 0; x.i < basedata.xpoints() ; x.i++) {
    //printf("land %3d %3d %3d\n",x.i,x.j, (int) land[x] );

    if ( (int) basedata[x].ssmi.conc_bar < 100 && 
         (int) basedata[x].ssmi.bar_conc < 100 &&
         (int) basedata[x].ssmi.conc_bar >  15 && 
         (int) basedata[x].ssmi.bar_conc >  15 &&
         (int) basedata[x].ssmi.t85h > 100*100 &&
         (int) basedata[x].ssmi.t85h < 300*100 &&
         (int) basedata[x].ssmi.t85v > 100*100 &&
         (int) basedata[x].ssmi.t85v < 300*100 
                                                   ) {
      tmp1 = basedata[x];
      polar = tmp1.polarization();
      diff = tmp1.differences();
      grad = tmp1.gradient();


      if ( (int)land[x] <  100) {
      //printf("%5d %5d %5d %5d %5d %5d %5d  %6.3f %6.3f %6.3f  %6.3f %6.3f %6.3f  %5.0f %5.0f %5.0f  %3d %3d\n",
      //  basedata[x].ssmi.t19v,
      //  basedata[x].ssmi.t19h,
      //  basedata[x].ssmi.t22v,
      //  basedata[x].ssmi.t37v,
      //  basedata[x].ssmi.t37h,
      //  basedata[x].ssmi.t85v,
      //  basedata[x].ssmi.t85h,
      //  polar.i, polar.j, polar.k,
      //  grad.i, grad.j, grad.k,
      //  diff.i, diff.j, diff.k,
      //  basedata[x].ssmi.conc_bar,
      //  basedata[x].ssmi.bar_conc );

      fprintf(f85,"%5d %5d  %5d %5d  %5d %5d  %6.3f %f\n", 
        basedata[x].ssmi.conc_bar,
        basedata[x].ssmi.bar_conc,
        basedata[x].ssmi.t85v,
        basedata[x].ssmi.t85h,
        basedata[x].ssmi.t85v - basedata[x].ssmi.t85h,
        basedata[x].ssmi.t85v + basedata[x].ssmi.t85h,
        (float) basedata[x].ssmi.t85v / (float) basedata[x].ssmi.t85h,
        (float) basedata[x].ssmi.t85v * (float) basedata[x].ssmi.t85h /1.e8);

        x1[totpts] = basedata[x].ssmi.conc_bar;
        x2[totpts] = basedata[x].ssmi.bar_conc;
        x3[totpts] = basedata[x].ssmi.t85v;
        x4[totpts] = basedata[x].ssmi.t85h;
        x5[totpts] = basedata[x].ssmi.t85v - basedata[x].ssmi.t85h;
        x6[totpts] = basedata[x].ssmi.t85v + basedata[x].ssmi.t85h;
        x7[totpts] = (float) basedata[x].ssmi.t85v / (float) basedata[x].ssmi.t85h;
        x8[totpts] = (float) basedata[x].ssmi.t85v * (float) basedata[x].ssmi.t85h /1.e8;
        totpts += 1;

     } // end of 'not land' portion
     else {
       printf("else land %3d %3d %3d\n",x.i,x.j, land[x] );
     }
       
    }

  }
  } 

  cormat(x1, x2, x3, x4, x5, x6, x7, x8, totpts);

  return 0;
}
void  cormat(mvector<float> &x1, mvector<float> &x2, mvector<float> &x3, 
             mvector<float> &x4, mvector<float> &x5, mvector<float> &x6, 
             mvector<float> &x7, mvector<float> &x8, int totpts) {
//Have extracted mvector of points, now see about simple correlation matric
  float cors[8][8];
  mvector<float> *vecs[8], *tvec;
  float r2, xbar, ybar, sig2x, sig2y;
  int i, j, nmaxcur;
  
  tvec = new mvector<float> (totpts);
  vecs[0] = new mvector<float> (totpts);
  vecs[1] = new mvector<float> (totpts);
  vecs[2] = new mvector<float> (totpts);
  vecs[3] = new mvector<float> (totpts);
  vecs[4] = new mvector<float> (totpts);
  vecs[5] = new mvector<float> (totpts);
  vecs[6] = new mvector<float> (totpts);
  vecs[7] = new mvector<float> (totpts);
  *vecs[0] = x1;
  *vecs[1] = x2;
  *vecs[2] = x3;
  *vecs[3] = x4;
  *vecs[4] = x5;
  *vecs[5] = x6;
  *vecs[6] = x7;
  *vecs[7] = *vecs[6];
  *vecs[7] *= *vecs[6];

  nmaxcur = 8;
//normalizing and removing averages leaves unchanged
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  //CDprintf("%d norm %f average %f\n",j,vecs[j]->norm(), vecs[j]->average() );
  }

  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j]->vec, vecs[i]->vec, &totpts, &r2, &xbar, &ybar, 
                                                      &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }

// Now see about orthogonalizing w.r.t. 7 (PR)
  swap(*vecs[6], *vecs[nmaxcur-1]);
  for (j = 0; j < nmaxcur - 1; j++) {
    gsorthog(*vecs[nmaxcur-1], *vecs[j]);
  }  
  //vecs[0]->printer(stdout);
  nmaxcur -= 1;

//Repeated code.  Note, however, that this is now all variables.
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  }
  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j]->vec, vecs[i]->vec, &totpts, &r2, &xbar, &ybar, 
                                                      &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }

// Repeat, now extract 7th param (PR^2)
  swap(*vecs[6], *vecs[nmaxcur-1]);
  for (j = 0; j < nmaxcur - 1; j++) {
    gsorthog(*vecs[nmaxcur-1], *vecs[j]);
  }  
  //vecs[0]->printer(stdout);
  nmaxcur -= 1;
//Repeated code.  Note, however, that this is now all variables.
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  }
  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j]->vec, vecs[i]->vec, &totpts, &r2, &xbar, &ybar, 
                                                      &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }
  
// Repeat, now extract 3rd param (T85V)
  swap(*vecs[2], *vecs[nmaxcur-1]);
  for (j = 0; j < nmaxcur - 1; j++) {
    gsorthog(*vecs[nmaxcur-1], *vecs[j]);
  }  
  //vecs[0]->printer(stdout);
  nmaxcur -= 1;
//Repeated code.  Note, however, that this is now all variables.
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  }
  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j]->vec, vecs[i]->vec, &totpts, &r2, &xbar, &ybar, 
                                                      &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }
  
  return;
}

float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0.0;
  //should text for xpoints being same on both
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i] * y[i] ;
  }
  return (float) sum;
}

//Perform Gram-Schmidt orthogonalization, assuming that mvectors are already
//  normalized and zero mean x2 = x2 - dot(x1,x2)*x1
void gsorthog(mvector<float> &x1, mvector<float> &x2) {
  float tdot ;
  tdot = dot(x1,x2) / dot(x1,x1);
  //CDprintf("tdot = %f, mvector norms are %f %f\n",tdot, x1.norm(), x2.norm() );
  x2 -= x1 * tdot;
  return;
}
void swap(mvector<float> &x1, mvector<float> &x2) {
  mvector<float> y(x1.xpoints() );
  y = x2;
  x2 = x1;
  x1 = y;
  return;
}
//  mvector<float> y(x1.xpoints() );
//  float tdot;
//
//  tdot = dot(x1, x2);
//  y = 
//
