#include <iostream>
#include "mvector.h"
#include "program.h"

// Function to run through the population, select out the fit, erase the unfit,
//   and mutate the fit to fill up the population

void selector1(mvector<program> &population, float &best) {
  float average = 0.;
  mvector<program> temp(NPROGS);
  int nlethal = 0, nfavorable = 0, nsurvive = 0;

  int i, j;

  for (i = 0; i < population.xpoints(); i++) {
    average += population[i].value;
  }
  average /= population.xpoints();
 
  j = 1;
  for (i = 0; i < population.xpoints() ; i++) {
    if (population[i].value > average ) {
       nlethal += 1; 
    }
    else {
      temp[j].set(population[i]);
      j += 1;
    }
    if (population[i].value == best) {
      temp[0].set(population[i]);
    }
    if (population[i].value < population[0].value) {
      nfavorable += 1;
    }
  } 
  nsurvive = j - 1;

  // Finished copying over the successful variations to temporary
  cout << "Lethal variants: " << nlethal << " favorable " << nfavorable; 
  cout << endl; 

  for (i = 0; i < nsurvive+1; i++) {
    population[i].set(temp[i]);
  }
  // Mutate into the vacancies
  for (i = nsurvive+1; i < NPROGS; i++) { 
     j = (int) ((float)(nsurvive+1)*rand()/(RAND_MAX + 1.0) );
     population[j].mutate(population[i]);  // mutate _from_ parent _to_ i
  }
 
  return;
} 

/////////////////////////////////////////
// Routine to manage the selection of new programs
void selector2(program *x, int nprogs) {
  float avscore, minscore, maxscore, refscore;
  int nlethal, nunfav, nbetter, nequal, nneutral;
  int nbest, nparents, parent;
  program tprog(8);

  int i, j, count;

  avscore = 0.0;
  nbetter = 0;
  nequal  = 0;
  nlethal = 0;
  nunfav = 0;
  nneutral = 0;
  count    = 0;
  minscore = 1.e5;
  maxscore = -1.e5;
  refscore = x[0].value;
  nbest = 0;
  for (i = 0; i < nprogs; i++) {
    maxscore = fmax(maxscore, x[i].value);
    if (x[i].value < minscore ) {
      minscore = fmin(minscore, x[i].value);
      nbest = i;
    }
    if (x[i].value < 2.*113. ) {
      avscore += x[i].value;
      count += 1;
    }
    else {
      nlethal += 1;
    }
  }
  avscore /= (float) count;
  printf("Population average, min, max score is %f %f %f %d\n",
           avscore, minscore, maxscore, count);
  //printf("Best score is %f, from program %d\n",minscore, nbest);
  //x[nbest].list(stdout);

  for (i = 0; i < nprogs; i++) {
    if (x[i].value == refscore) {
      nequal += 1;
    }
    else if (x[i].value < refscore) {
      nbetter += 1;
    }
    else if (x[i].value > avscore) {
      nunfav += 1;
    }
    else { 
      nneutral += 1;
    }
  }
  printf("%d Lethal variants\n",nlethal);
  printf("%d Unfavorable variants\n",nunfav);
  printf("%d Neutral variants\n",nneutral);
  printf("%d Clones\n",nequal); 
  printf("%d Favorable mutants\n",nbetter);
    
// 1) Copy over the original plus all the ones that are better
  // swap the current best program in to the first slot
  if (nbest != 0) {
    //printf("Swapping in the best program\n"); fflush(stdout);
    tprog.set(x[0]);
    x[0].set(x[nbest]);
    x[nbest].set(tprog);
  }
  j = 1; // now leave x[0] alone
  for (i = 1; i < nprogs; i++) {
    if (x[i].value < refscore) {
      //printf("Copying in the %d th improved program\n", j); fflush(stdout);
      x[j].set(x[i]);
      j+= 1;
    }
  }
  nparents = j;
  printf("Copied over %d improved versions\n",j); fflush(stdout);

// Copy over the neutral ones, starting above the good ones already
//   copied
  count = 0;
  i = 1;
  while (count+nparents < 0.05*nprogs && i < nprogs) {
    if (x[i].value < avscore && x[i].value >= refscore ) {
      x[j].set(x[i]); // set j to be i
      j        += 1;
      nparents += 1;
      count    += 1;
    }
    i++;
  }
  printf("Copied over %d neutral or improved versions\n",j); fflush(stdout);

// 3) Now mutate the population of improvements in to the remainder of the
//    field
  printf("Constructing %d mutants\n",nprogs - j);
  for (i = j; i < nprogs; i++) {
     //printf("Constructing program %d\n",i); fflush(stdout);
     parent = (int) ((float)(nparents+1)*rand()/(RAND_MAX + 1.0) );
     x[parent].mutate(x[i]);  // mutate _from_ parent _to_ i
  }
  //printf("Leaving Selector\n"); fflush(stdout);

  return ;
}

// Function to run through the population, select out the fit, erase the unfit,
//   and mutate the fit to fill up the population
void selector3(program *population, float &best) {
  double average = 0., eps=1.e-6;
  //mvector<program> temp(NPROGS+1);
  program temp[NPROGS+1];
  int nlethal = 0, nfavorable = 0, nsurvive = 0;

  int i, j;

  for (i = 0; i < NPROGS; i++) {
    //printf("averaging, i = %d\n",i); fflush(stdout);
    average += population[i].value;
  }
  average /= (NPROGS);

  j = 1;
  for (i = 0; i < NPROGS ; i++) {
    if (population[i].value > average *(1.+eps) ) {
       printf("score, vs avg %f %f\n",population[i].value, average);
       nlethal += 1;
    }
    else {
      temp[j].set(population[i]);
      j += 1;
    }
    if (population[i].value == best) {
      temp[0].set(population[i]);
    }
    if (population[i].value < population[0].value) {
      nfavorable += 1;
    }
  }
  nsurvive = j - 1;

  // Finished copying over the successful variations to temporary
  if (nlethal > 0 || nfavorable > 0 ) {
    cout << "Lethal variants: " << nlethal << " favorable " << nfavorable;
    cout << endl;
  }

// Following is necessary to avoid simply duplicating for all time.
  if (nsurvive > 0.5*NPROGS ) nsurvive = NPROGS/8 ;

  for (i = 0; i < min(nsurvive+1,NPROGS) ; i++) {
    population[i].set(temp[i]);
  }
  // Mutate into the vacancies
  for (i = nsurvive+1; i < NPROGS; i++) {
     j = (int) ((float)(nsurvive+1)*rand()/(RAND_MAX + 1.0) );
     population[j].mutate(population[i]);  // mutate _from_ parent _to_ i
  }
  return;
}
