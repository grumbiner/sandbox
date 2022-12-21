// metagenomics 
  float toler = 1./1.8;
  int ntrain = 365;

// Genomics
  int population = 100;
  int genmax = 2200;
#define NPARMS 6
  mvector<float> regress[NPARMS], sdevs[NPARMS];
  mvector<float> best(NPARMS), sd_best(NPARMS);
  mvector<float> trial(NPARMS), trial_sd(NPARMS);
  mvector<float> scores(population);


// Initialize:
  for (i = 0; i < NPARMS; i++) {
    regress[i].resize(population);
    sdevs[i].resize(population);
  }
  best = 0.;
  sdbest = 1.0;

  float eval = evaluate(matchups, ntrain, toler, trial);
  float score_null = score(toler, matchups[8], 0, ntrain);
  printf("pre-trained function: %f\n", eval, score_null);

  float score_best = score_null;

  if (score_null != eval) {
    printf("%f v %f fails\n",score_null, eval);
    return 1;
  }








///////////////////////// subroutines/functions
// Construct the time series of values predicted from the evolutionary algorithm
float evaluate(mvector<float> *matchups, int ntrain, float toler, mvector<float> &tmp) {
  mvector<float> delta(ntrain);
  for (int i = 0; i < ntrain; i++) {
    delta[i] = matchups[8][i] + tmp[0];
    for (int j = 1; j < NPARMS; j++) {
      delta[i] += tmp[j]*matchups[j][i];
    }
  }
  return (score(toler, delta, 0, ntrain));
}
////////////////////////////
float score(float toler, mvector<float> &delta, int start, int end) {
// Score is to penalize the deltas:
//   if |delta| < toler, no penalty (obs are only to 1 F)
//1)   else sum squared deviation
//2) else sum 4th power
  int i;
  double sumsq = 0.0, power = 2.;
  for (i = start; i <  end; i++) {
    //if (fabs(delta[i]) > toler) {
      sumsq += pow(delta[i],power);
    //}
  }
  return (float) pow(sumsq/((double)(end-start)), 1./power);
}
