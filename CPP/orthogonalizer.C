#include "mvector.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

float absdot(mvector<float> &x, mvector<float> &y) ;
float real_dot(mvector<float> &x, mvector<float> &y) ;
float orthog(mvector<float> &x, mvector<float> &y) ;

int main(void) {
  FILE *fin, *fout;
  char line[900];
  int i,j;
  float x, r;
  char *sep = "\t", *null;
  char fname[90];
  mvector<mvector<float> > states(51);

// get the state name index:
  char names[51][900];
  fin = fopen("states", "r");
  for (i = 0; i < 51; i++) {
    fscanf(fin, "%s", names[i]);
//    printf("state %d is %s\n",i,names[i]);
  }
  fclose(fin);

  fin = fopen("elections7.csv","r");
  null = NULL;

// Read in the data:
  for (i = 0; i < 51; i++) {
    states[i].resize(14);

    fgets(&line[0], 900, fin);
    //printf("%s",line);

    j = 0;
    x = atof(strtok(&line[0], sep) ) ;
    //printf("j %d %f\n",j,x);
    states[i][j] = x;
    
    for (j = 1; j < 14; j++) {
      x = atof(strtok(null, sep));
      //printf("j %d %f\n",j,x);
      states[i][j] = x;
    }
  }
  fclose(fin);

  float sum, smax = 0;
  int nmax = -1, round = 0;
  for (round = 0; round < 10; round++) {
    sum = 0;
    smax = 0;
    for (i = 0; i < states.xpoints(); i++) {
      sum = 0;
  
      for (j = 0; j < states.xpoints(); j++) {
        sum += absdot(states[i],states[j]);
      }
  
      if (fabs(sum) > fabs(smax)) {
        smax = fabs(sum);
        nmax = i;
      }
    }
    printf("round %1d state = %2d %s score = %6.2f  ",round, nmax, names[nmax], smax);
    for (j = 0; j < 13; j++) {
      printf(" %6.2f",states[nmax][j]);
    }
    printf("\n");


    // orthog:
    int lm;
    sprintf(fname,"round%d.csv",round);
    fout = fopen(fname, "w");
    for (j = 0; j < states.xpoints(); j++) {
      if (j != nmax) {
        r = orthog(states[j],states[nmax]);
        fprintf(fout,"%s\t%1d\t%f\t",names[j], round, r);
        for (lm = 0; lm < 12; lm++) {
          fprintf(fout,"%5.2f\t",states[j][lm]);
        } 
        fprintf(fout,"\n");
      }
    }
      states[nmax] = 1.e-7; // use this to avoid division by zero
      r = 1.0;
        fprintf(fout,"%s\t%1d\t%f\t ",names[nmax], round, r);
        for (lm = 0; lm < 12; lm++) {
          fprintf(fout,"%5.2f\t",states[nmax][lm]);
        } 
        fprintf(fout,"\n");
    
    fclose(fout);
  }

  return 0;
}
float absdot(mvector<float> &x, mvector<float> &y) {
  float sum = 0;

  for (int i = 0; i < 12; i++) {
    sum += x[i]*y[i];
  }
  //return sqrt(fabs(sum));
  return (fabs(sum));
}
float real_dot(mvector<float> &x, mvector<float> &y) {
  float sum = 0;

  for (int i = 0; i < 12; i++) {
    sum += x[i]*y[i];
  }
  return (sum);
}
float orthog(mvector<float> &x, mvector<float> &y) {
  float mx, my, pxy, r;

  pxy = real_dot(x,y);
  mx  = sqrt(real_dot(x,x));
  my  = sqrt(real_dot(y,y));
  r   = pxy/mx/my;
  //printf("r = %f\n",r);
  for (int i = 0; i < 12; i++) {
    x[i] -= r*y[i];
  }

  return r;
}
