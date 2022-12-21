#include "mvector.h"
#include "genes.h"

#include <stdlib.h>

#define POPULATION 100
#define ninputs 20
#define nscores 20
void descend(int nparent, int nchild, mvector<float> *inputs, mvector<float> *sdevs) ;
void write(FILE *fout, int i, mvector<float> *scores, mvector<float> *inputs, mvector<float> *sdevs) ;
void swap(int i,int j, mvector<float> *inputs, mvector<float> *sdevs) ;
float randnorm(float sdev) ;
float lognorm(float sdev) ;
void newpop(mvector<float> *inputs, mvector<float> *sdevs) ;

int main(int argc, char* argv[]) {
  FILE *fin, *fout;

  mvector<float> scores[POPULATION], inputs[POPULATION], sdevs[POPULATION];
  mvector<float> overall(POPULATION);
  float sum = 0.0, smax = 0.;
  int count, count2 = 0, count4 = 0;
  float req;
  mvector<bool> parent(POPULATION);
  int nparent;
  bool newstart = false;
 
  int i, j;
  long int seed = 0;

  fin = fopen(argv[1], "r");
  if ( fin == (FILE*) NULL) {
    printf("could not open input file, defaulting to creating a new population\n");
    newstart = true;  
  }
  fout = fopen(argv[2], "w");
  if ( fout == (FILE*) NULL) {
    printf("could not open output file, serious error! %s\n",argv[2]);
    return 1;
  }

  for (i = 0; i < POPULATION; i++) {
    scores[i].resize(nscores);
    inputs[i].resize(ninputs);
    sdevs[i].resize(ninputs);
  }

  if (newstart) {
    for (i = 0; i < POPULATION; i++) {
      newpop(inputs, sdevs);
      scores[i] = 0.0;
      write(fout, i, scores, inputs, sdevs); 
    }
    return 2;
  }

  srand48(seed);

// Read in inputs
//  for (i = 0; i < POPULATION; i++) {
//    scores[i].binin(fin);
//    inputs[i].binin(fin);
//    sdevs[i].binin(fin);
//  }

// Select parents 
  for (i = 0; i < POPULATION; i++) {
    for (j = 0; j < nscores; j++) {
      sum += scores[i][j];
    }
    overall[i] = sum/(float) nscores; 
    if (overall[i] > smax) smax = overall[i];
  }

  // do some browsing towards selecting population, 
  //   noting that 0 will always be kept as it's the original MDL set
  for (i = 0; i < POPULATION; i++) {
    if (overall[i] > smax*(1.-1./2.) ) count2++;
    if (overall[i] > smax*(1.-1./4.) ) count4++;
  }
  printf("count2 %d count4 %d\n",count2, count4);
  if (count2 > POPULATION/2) {
    req = smax*(1.-1./4.);
    count = count4;
  }
  else {
    req = smax*(1.-1./2.);
    count = count2;
  }

  
// Now scan through and if not a parent, evolve a new parameter set:
  parent[0] = true;
  for (i = 1; i < POPULATION; i++) {
    if (overall[i] > req) { 
      parent[i] = true; 
    }
    else {
      parent[i] = false;
    }
  }
  // sort up so that all parents are in 0-count(-1)
  for (i = 1; i < POPULATION-1; i++) {
    if (!parent[i]) {
      for (j = i+1; j < POPULATION; j++) {
        if (parent[j]) {
          swap(i,j, inputs, sdevs);
        }
      }
    }
  }
  // now run through and create descendents from parents
  for (i = count; i < POPULATION; i++) {
    nparent = (int) drand48()*count;
    descend(nparent, i, inputs, sdevs);
  }

// Write out descendents for evaluation:
  for (i = count; i < POPULATION; i++) {
    write(fout, i, scores, inputs, sdevs); 
  }

  fclose(fout);

  return 0;
}

void descend(int nparent, int nchild, mvector<float> *inputs, mvector<float> *sdevs) {
  // create a descendent in nchild from the parent in nparent
  int i;
  for (i = 0; i < inputs[nparent].xpoints(); i++) {
    inputs[nchild][i] = inputs[nparent][i] + randnorm(sdevs[nparent][i]);
    sdevs [nchild][i] = sdevs [nparent][i] + lognorm( sdevs[nparent][i]);
  }
  
  return;
}
void swap(int x,int y, mvector<float> *inputs, mvector<float> *sdevs) {
  float tmp1, tmp2;
  for (int i = 0; i < sdevs[0].xpoints(); i++) {
    tmp1         = inputs[x][i];
    tmp2         = sdevs[x][i];
    inputs[x][i] = inputs[y][i];
    sdevs[x][i]  = sdevs[y][i];
    inputs[y][i] = tmp1   ;
    sdevs[y][i]  = tmp2   ;
  } 

  return;
}
void write(FILE *fout, int i, mvector<float> *scores, mvector<float> *inputs, mvector<float> *sdevs) {
  int j;
  for (j = 0 ; j < scores[i].xpoints(); j++) {
    fprintf(fout,"%f ",scores[i][j]);
  }
  fprintf(fout,"\n");
  for (j = 0 ; j < inputs[i].xpoints(); j++) {
    fprintf(fout,"%f %f ",inputs[i][j], sdevs[i][j]);
    //fprintf(fout,"%f  ",inputs[i][j]);
  }
  fprintf(fout,"\n");
  
  return;
}
float randnorm(float sdev) {
  float v1, v2, rsq, fac;

  do {
    v1 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v2 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v1 = 2.*v1 - 1.;
    v2 = 2.*v2 - 1.;
    rsq = v1*v1 + v2*v2;
  }
  while (rsq >= 1.0 || rsq == 0.0) ;

  fac = sqrt(-2.*log(rsq)/rsq);

  return v1*fac;

}
float lognorm(float sdev) {
  return sdev;
}
void newpop(mvector<float> *inputs, mvector<float> *sdevs) {
  // create a descendent in nchild from the parent in nparent
  int i, j;

  for (j = 0; j < POPULATION; j++) {
    for (i = 0; i < inputs[0].xpoints(); i++) {
      inputs[j][i] = randnorm(1.0);
      sdevs [j][i] = lognorm(1.0);
    }
  }
  
  return;
}
