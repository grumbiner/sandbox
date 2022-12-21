#include "grid_math.h"
#include "mvector.h"

#define NDAYS (28*365+7)

void predictor(grid2<float> &a, mvector<float> &b, mvector<float> &sun, mvector<float> &c, mvector<mvector<float> > &oscs) ;


int main(int argc, char *argv[]) {
// inputs: obs'd nao, ao, pna, aao, tsi, day by day from start
  mvector<float> nao(NDAYS), ao(NDAYS), pna(NDAYS), aao(NDAYS), tsi(NDAYS);
  float tmp[5];

// Derived -- summed oscillation values, Odot is the observed values, O is the
//   time integral
  mvector<mvector<float> > summed(NDAYS);
  mvector<mvector<float> > predicted(NDAYS);

  int i, k;

// Read in data
  FILE *fin[5];
  for (i = 1; i <=5; i++) {
    fin[i-1] = fopen(argv[i], "r");
  }
  for (i = 0; i < NDAYS; i++) {
    for (k = 0; k < 5; k++) {
      fscanf(fin[k], "%f",&tmp[k]);
    }
    nao[i] = tmp[0];
     ao[i] = tmp[1];
    aao[i] = tmp[2];
    pna[i] = tmp[3];
    tsi[i] = tmp[4];
  }
  for (i = 1; i <=5; i++) {
    fclose(fin[i-1]);
  }


// Compute summed values:
  for (i = 0; i < NDAYS; i++) {
    summed[i].resize(4);
    summed[i] = (float) 0.0;
    predicted[i].resize(4);
    predicted[i] = (float) 0.0;
  }
  for (i = 1; i < NDAYS; i++) {
    summed[i][0] += nao[i-1];
    summed[i][1] +=  ao[i-1];
    summed[i][2] += aao[i-1];
    summed[i][3] += pna[i-1];
  }
 
// rescale tsi to have zero mean.
  tmp[0] = tsi.average();
  tsi -= tmp[0];
  printf("mean tsi %f\n",tmp[0]);
  printf("final sums %f %f %f %f\n",summed[NDAYS-1][0], summed[NDAYS-1][1], summed[NDAYS-1][2], summed[NDAYS-1][3]);
  
// csv output of values:
  FILE *fout;
  fout = fopen(argv[6],"w");
  for (i = 0; i < NDAYS; i++) {
    fprintf(fout,"%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n",i, summed[i][0], summed[i][1], summed[i][2], summed[i][3], tsi[i],  nao[i], ao[i], aao[i], pna[i]);
  }





  return 0;
}
void scorer() {

  return;
}


// routine to take the matrix coefficients, solar flux coefficients, constants 
//   and an assumed initial condition of zero to produce a time series of the
//   values of each of the oscillations -- nao, ao, pna, aao.
void predictor(grid2<float> &a, mvector<float> &b, mvector<float> &sun, mvector<float> &c, mvector<mvector<float> > &oscs) {
  int i, j, k;
  ijpt loc;

  oscs[0] = (float) 0.;
  for (i = 1; i < sun.xpoints(); i++) {
    for (j = 0; j < b.xpoints(); j++) {
      oscs[i][j] += c[j] + b[j]*sun[i];
      for (k = 0; k < b.xpoints(); k++) {
        loc.i = j;
        loc.j = k;
        oscs[i][j] += a[loc]*oscs[i-i][k];
      }
    }
  }

  return;
}
