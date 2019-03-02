#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NX 385
#define NY 465

#define NLAG 3

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

  unsigned char xtmp[NY][NX];
  unsigned char xtot[31][NY][NX];
  unsigned char tser[31];


  printf("started program \n");
  tmp = atoi(argv[1]);
  yy  = tmp / 100;
  mo = tmp % 100;
  printf("argv1, yy, mo %s, %d %d\n", argv[1], yy, mo);

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
