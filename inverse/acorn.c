#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NX 385
#define NY 465

#define NLAG 8

void xtract(unsigned char *xtot, unsigned char *tser, int nx, int ny, 
            int ipt, int jpt, int nd);
int someunflag(unsigned char *tser, int nd);
float autcor(unsigned char *tser, const int nx, const int lag);

int main(int argc, char *argv[])
{
  FILE *fin, *fout;
  int i, j, yy, mo, dy, nd, tmp, lag;

  char fname[90];
  float aut[NLAG][NY][NX]; 

  float xtmp[NY][NX];
  int n1, n2;
  unsigned char xout[NY][NX];
  unsigned char xtot[31][NY][NX];
  unsigned char tser[31];


  printf("started program \n");
  tmp = atoi(argv[1]);
  yy  = tmp / 100;
  mo = tmp % 100;
  printf("argv1, yy, mo %s, %d %d\n", argv[1], yy, mo);



  if ( mo == 1 || mo == 3 || mo == 5 || mo == 7 || mo == 8 || mo == 10 || mo == 12 ) {
    nd = 31;
  }
  else if (mo == 4 || mo == 6 || mo == 9 || mo == 11) {
    nd = 30;
  }
  else if (yy % 4 == 0) {
    nd = 29;
  }
  else {
    nd = 28;
  }
  printf("nday  = %d\n",nd);
 

  for (dy = 1; dy <= nd; dy++) {
    sprintf(fname, "north.%2d%02d%02d", yy, mo, dy);
    fin = fopen(fname, "r");
    if (fin == NULL ) {
      printf("failed to open %s\n",fname);
    }
    else {
/*      fread(&n1, sizeof(int), 1, fin);
      fread(&n2, sizeof(int), 1, fin); */
      fread(xtmp, sizeof(float), NX*NY, fin);
      for (j = 0; j < NY; j++) {
        for (i = 0; i < NX; i++) {
           xtot[dy-1][j][i] = (unsigned char) (xtmp[j][i]*100. + 0.5);
        }
      }
    }

  } /* end of reading in data */

  printf("Finished reading in data\n");
  fflush(stdout);

/* Compute autocorrelations */
  for (j = 0 ; j < NY; j++) {
    for (i = 0; i < NX; i++) {
       xtract(&xtot[0][0][0], &tser[0], NX, NY, i, j, nd);
       if (someunflag(tser, nd) ) {
         for (lag = 0; lag < NLAG; lag++) {
            aut[lag][j][i] = autcor(tser, nd, lag);
/*            printf("lag i j auto %3d %3d %3d %f\n", lag, i, j, aut[lag][j][i]); */ 
/*            fflush(stdout); */
         }
       }
       else {
         for (lag = 0; lag < NLAG; lag++) {
            aut[lag][j][i] = 1.0;
         }
       }
    }
  }

  printf("Finished computing autocorrelations\n");
  fflush(stdout);

/* Transfer to write out */
  for (lag = 0; lag < NLAG; lag++) {
    sprintf(fname, "auto.%s.%s.%01d", argv[1], argv[2], lag);
    fout = fopen(fname, "w");
    for (j = 0; j < NY; j++) {
      for (i = 0; i < NX; i++) {
        if (aut[0][j][i] != 0) {
          xout[j][i] = (unsigned char) (aut[lag][j][i]/aut[0][j][i] *100 + 100) ; 
        }
        else {
         xout[j][i] = 255;
        }
      }
    }
    fwrite(xout, sizeof(unsigned char), NX*NY, fout);
    fclose(fout);
  }

  return 0;

}

void xtract(unsigned char *xtot, unsigned char *tser, int nx, int ny, 
            int ipt, int jpt, int nd)
{
  int i, index;

  for (i = 0; i < nd; i++) {
    tser[i] = xtot[ipt + jpt*nx + i*nx*ny];
  }

  return;
}

int someunflag(unsigned char *tser, int nd)
{
  int i, ret;
  ret = (1 == 0);

  for (i = 0; i < nd; i++) {
    if ( tser[i] < 128 )  { ret = (1==1) ; }
  }

  return ret;
}


float autcor(unsigned char *tser, const int nx, const int lag)
{
  int i, j;
  float *lx, *tx;
  float atmp, xbar;

  if (nx == 0) return -1.0;

  lx = malloc( sizeof(float)*nx );
  tx = malloc( sizeof(float)*nx );

  j = 0;
  xbar = 0.;

  if (lag >= 0 ) {
    for (i = 0; i < nx - lag; i++) {
      if ( tser[i] < 128 && tser[i+lag] < 128 ) {
        xbar += tser[i];
        tx[j] = (float) tser[i];
        lx[j] = (float) tser[i+lag];
        j+= 1;
      }
    }
    if ( j == 0) {
      free(lx);
      free(tx);
      return -1.;
    }
    else {
/*      printf("%d %f\n",j, xbar); */
      xbar = xbar / (float)j;
      atmp = 0.0;
      for (i = 0; i < j; i++) {
          atmp += (tx[i]-xbar)*(lx[i]-xbar);
      }
    }

  }
  else {
    printf("negative lags not handled yet \n");
    free(lx);
    free(tx);
    return -1.0;
  }

  free(lx);
  free(tx);

/* Note that this is the unscaled autocorrelation, need to divide
   outside with autcor(0 lag) to get scaled */
  if ( j > 5 ) {
    return ( atmp ) ;
  }
  else {
    return -1.0;
  }

}
