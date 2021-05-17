#include "genes.h"

#define TAU 0
#define T2 1
#define TD 2
#define TH 3 // 850-1000 mb T
#define RH 4
#define WD 5 // wind, m/s
#define TOB 6
#define TDOB 7
#define TERR 8
#define TDERR 9

#define NSTEP 579

float randnorm(float sdev) ;
float lognorm(float sdev) ;

void newpop(mvector<float> *regress, mvector<float> *sdevs, 
            mvector<float> &best, mvector<float> &sd_best);

float score(float toler, mvector<float> &delta, int start, int end) ;
void hill_climb(mvector<float> *matchups, int ntrain, float toler, mvector<float> &tmp, bool update) ;

float evaluate(mvector<float> *matchups, int ntrain, float toler, mvector<float> &tmp) ;
void make_vector(mvector<float> *matchups, int start, int end, float toler, mvector<float> &best, mvector<float> &delta) ;
void display(mvector<float> *matchups, int nobs, mvector<float> &best, mvector<float> &sd_best) ;

int main(int argc, char *argv[]) {
  mvector<float> matchups[10]; // 10 columns, trying to predict last 2, 
                               // being aware that 7,8 are the observations
                               // note that TAU is counter, indexing days since start
// Set up, get data:
  FILE *fin;
  int i, j, parm;
  float tmp[10];

// for the evolution:
  float toler = 1./1.8;
  int ntrain = 365;
  int population = 100;
  //int genmax = 50000;
  int genmax = 5000*60; // currently ~5k per second
#define NPARMS 6
  mvector<float> regress[NPARMS], sdevs[NPARMS];
  mvector<float> best(NPARMS), sd_best(NPARMS);
  mvector<float> trial_sd(NPARMS), trial(NPARMS);
  mvector<float> scores(population);

  long int seed = 0;
  srand48(seed);

// get the data:
  fin = fopen(argv[1], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  for (j = 0; j < 10; j++) {
    matchups[j].resize(NSTEP);
  }

  for (i = 0; i < NSTEP && !feof(fin); i++) {
    fscanf(fin, "%f %f %f %f %f %f %f %f %f %f\n", &tmp[0], &tmp[1],
        &tmp[2], &tmp[3], &tmp[4], &tmp[5], &tmp[6], &tmp[7], &tmp[8], &tmp[9]);
    for (j = 0; j < 10; j++) {
      matchups[j][i] = tmp[j]; 
    }
    if (feof(fin)) break;
  }
  int nobs = i;
  printf("nobs = %d\n",nobs); fflush(stdout);
 
  for (j = 1; j < 10; j++) {
    printf("%2d %6.2f %6.2f %5.2f\n",j, matchups[j].average(), 
            matchups[j].rms(),
            sqrt(-matchups[j].average()*matchups[j].average() + 
                  matchups[j].rms()*matchups[j].rms())          );
    fflush(stdout);
  }

  best = 0.;
  for (i = 0; i < NPARMS; i++) {
    regress[i].resize(population);
    sdevs[i].resize(population);
  }
  for (i = 0; i < trial.xpoints(); i++) {
    trial[i] = 0.;
  }

  float       eval = evaluate(matchups, ntrain, toler, trial);
  float score_null = score(toler, matchups[8], 0, ntrain);
  float score_best = score_null, score_raw = score_null;
  if (score_null != eval) {
    printf("%f v %f fails\n",score_null, eval);
    return 1;
  }

//////////////////////////////////////////////////////////////////////
// use indices 1-5 (gfs vars) to predict index 8 (difference between model and obs t2m) 
// --> 5 floats (slopes of dependencies) (?range [+-1], precision/sigma ? )
// --> 1 float (mean) (+- 30)(precision?)
// Evolutionary strategy, let the slopes be gaussian (0,1) to start with, 
//   can evolve towards 0 for parms to ignore, can grow as needed. Setting sdev allows
//   for parms to constrain themselves

  int gen_best = 0;

// Develop the initial population:
// note the negative sign
  j = 8;
  best[0]    = -matchups[j].average();
  sd_best    = 0.5;
  sd_best[0] = sqrt(-matchups[j].average()*matchups[j].average() +
                     matchups[j].rms()*matchups[j].rms())   ;
  //printf("initial mean + sd_mean : %f %f\n",best[0], sd_best[0]);

  eval = evaluate(matchups, ntrain, toler, best);
  printf("score before %f and after bias correction %f, %f\n",score_best, eval, best[0]);
  score_null = eval;
  //display(matchups, nobs, best, sd_best);

  printf("preserve member 0 with the best so far. Elitist 1+N scheme\n");
  newpop(regress, sdevs, best, sd_best);
// Iterate over generations:
  int generations, last_update = 0;
  bool update = false;

  for (generations = 0; generations < genmax; generations++) {
    for (j = 1; j < population; j++) {
      for (parm = 0; parm < NPARMS; parm++) {
           trial[parm] = regress[parm][j];
        trial_sd[parm] =   sdevs[parm][j];
      }
      scores[j] = evaluate(matchups, ntrain, toler, trial);
      if (scores[j] <= score_null) {
        printf("good %4d %3d %f ",generations, j, scores[j]/score_null );
        printf("%7.3f  %6.3f %6.3f %6.3f %6.3f %6.3f  %7.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",
                  trial[0], trial[1], trial[2], trial[3], trial[4], trial[5],
                  trial_sd[0], trial_sd[1], trial_sd[2], trial_sd[3], trial_sd[4], trial_sd[5]);
      }
      // Improvement:
      if (scores[j] < score_best) {
        gen_best = generations;
        for (i = 0; i < NPARMS; i++) {
          best[i]    = trial[i];
          sd_best[i] = trial_sd[i];
        }
        //debug display(matchups, nobs, best, sd_best);

        last_update = generations;
        score_best = scores[j];
        if ((score_best / score_null) < 0.01) {
          printf("gen %4d best, %e of orig  ",generations, 
                        score_best / score_null);
        }
        else {
          printf("gen %4d best, %f of orig  ",generations, 
                        score_best / score_null);
        }
        printf("%7.3f  %6.3f %6.3f %6.3f %6.3f %6.3f  %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",best[0], 
                   best[1], best[2], best[3], best[4], best[5],
               sd_best[0], sd_best[1], sd_best[2], sd_best[3], sd_best[4], sd_best[5]
             );
      }

    } // population

    if (sd_best[0] > 100) sd_best[0] = 10.;
    if (sd_best[0] < 0.1) sd_best[0] = 1.0;

    newpop(regress, sdevs, best, sd_best);

  //debug-verbose: display(matchups, nobs, best, sd_best);
  } // end looping over generations


  // print final score and genes that get there
  display(matchups, nobs, best, sd_best);

  mvector<float> delta(nobs);
  make_vector(matchups, 0, nobs, toler, best, delta);
  printf("raw, debiased %5.2f %5.2f training score %5.2f untrained-period score %5.2f  relative %5.3f\n",
            score_raw,
            score_null,          
            score(toler, delta,   0, 365),
            score(toler, delta, 365, nobs),
            score(toler, delta,   0, 365)/score_null  );
  if (gen_best == 0) {
    printf("never did improve on first guess\n");
  }

  return 0;
}
////////////////////////////////////////////////////////
void make_vector(mvector<float> *matchups, int start, int end, float toler, mvector<float> &best, mvector<float> &delta) {
  for (int i = 0; i < delta.xpoints(); i++) {
    delta[i] = matchups[8][i+start] + best[0];
    for (int parm = 1; parm < NPARMS; parm++) { 
      delta[i] += best[parm]*matchups[parm][i+start];
    }
  }
    //for (int parm = 3; parm < NPARMS; parm++) { 
    //  delta[i] += best[1]*(matchups[1][i+start]-matchups[2][i+start]);
    //  delta[i] += best[2]*pow((matchups[1][i+start]-matchups[2][i+start]),2.)/10.0;
  return;
}
void display(mvector<float> *matchups, int nobs, mvector<float> &best, mvector<float> &sd_best) {
  int i, k, parm;
  float toler = 1./1.8;
  mvector<float> delta(nobs);

  printf("best genome: ");
    for (parm = 0; parm < best.xpoints(); parm++) {
      printf("%6.2f %7.3f  ",best[parm],sd_best[parm]);
    }
  printf("\n"); fflush(stdout);

  make_vector(matchups, 0, nobs, toler, best, delta);
  for (i = 0; i < nobs; i++) {
    printf("display %3d ",i);
    for (k = 0; k < 10; k++) {
      printf("%.2f ",matchups[k][i]);
    }

    printf(" delta  %6.2f \n",delta[i]);
  } 

  return;
}
///////////////////////////////////////////////////////////
// start-end, not just ntrain, return score
float evaluate(mvector<float> *matchups, int ntrain, float toler, mvector<float> &tmp) {
  int end = ntrain, start = 0;
  mvector<float> delta(end - start + 1);

  make_vector(matchups, 0, ntrain, toler, tmp, delta);
  return (score(toler, delta, 0, ntrain));
}
////////
void hill_climb(mvector<float> *matchups, int ntrain, float toler, mvector<float> &best, bool update) {
  mvector<float> delta(ntrain), tmp2(best.xpoints());
  float stmp[best.xpoints()+1], ds[best.xpoints()], dsbest = 0.0;
  int i, j, jmax = -1;
  float perturb = 0.1;


  make_vector(matchups, 0, ntrain, toler, best, delta);
  stmp[0] = score(toler, delta, 0, ntrain);

  for (j = 0; j < best.xpoints(); j++) {
    tmp2 = best;
    tmp2[j] += perturb; // 'small' perturbation -- how to define/arrive at?
    make_vector(matchups, 0, ntrain, toler, tmp2, delta);
    
    stmp[j+1] = score(toler, delta, 0, ntrain);
    ds[j] = (stmp[j+1]-stmp[0]);
    if (fabs(ds[j]) > dsbest) {
      jmax = j;
      dsbest = ds[j];
    }
  }
  printf("j dsbest update %d %f %d\n",jmax, dsbest, update);
  //if (update) best[jmax] += 0.1*dsbest/fabs(dsbest); // smaller score is better
  if (update) best[jmax] += perturb;

}
////////////////////////////
float score(float toler, mvector<float> &delta, int start, int end) {
// Score is to penalize the deltas:
//A)  if |delta| < toler, no penalty (obs are only to 1 F)
//1) else sum squared deviation
//2) else sum 4th power
  int i;
  double sumsq = 0.0, power = 2.;
  for (i = start; i <  end; i++) {
    if (fabs(delta[i]) > toler) {
      sumsq += pow(delta[i],power);
    }
  }
  return (float) pow(sumsq/((double)(end-start)), 1./power);
}
////////////////////////////
void newpop(mvector<float> *regress, mvector<float> *sdevs, 
            mvector<float> &best, mvector<float> &sd_best)  {
  int np, nparms = best.xpoints();
  int memno, nmem = regress[0].xpoints();

  memno = 0;
  for (np = 0; np < nparms; np++) {
    regress[np][memno] = best[np];
    sdevs[np][memno]   = sd_best[np];
  }
  for (memno = 1; memno < nmem; memno++) {
    for (np = 0; np < nparms; np++) {
      regress[np][memno] = best[np] + randnorm(sd_best[np]);
      sdevs[np][memno]   = lognorm(sd_best[np])*sd_best[np];
    }
  }
  return;
}
//////////////////////////////////////////////////////////
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
  return v1*fac*sdev;

}
// note the 0.25 is a kludge addition, make it 1 for original
float lognorm(float sdev) {
  return exp(0.25*randnorm(sdev));
}
