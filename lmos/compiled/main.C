//#include "genes.h"

#define NIN 10
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
#define NPARMS 6

#include "subs.C"

int main(int argc, char *argv[]) {
  mvector<float> matchups[NIN]; // NIN columns, trying to predict last 2, 
                               // being aware that 7,8 are the observations
                               // note that TAU is counter, indexing days since start
// Set up, get data:
  FILE *fin;
  int i, j, parm;
  float tmp[NIN];

// for the evolution:
  float toler = 1./1.8;
  int ntrain = 365;
  int population = 100;
  //int genmax = 50000;
  int genmax = 5000*60*10; // currently ~5k per second
  int nparms = NPARMS;
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
  for (j = 0; j < NIN; j++) {
    matchups[j].resize(NSTEP);
  }

  for (i = 0; i < NSTEP && !feof(fin); i++) {
    fscanf(fin, "%f %f %f %f %f %f %f %f %f %f\n", &tmp[0], &tmp[1],
        &tmp[2], &tmp[3], &tmp[4], &tmp[5], &tmp[6], &tmp[7], &tmp[8], &tmp[9]);
    for (j = 0; j < NIN; j++) {
      matchups[j][i] = tmp[j]; 
    }
    if (feof(fin)) break;
  }
  int nobs = i;
  printf("nobs = %d\n",nobs); fflush(stdout);
 
  printf("initial statistics\n");
  printf("parameter #, mean, rms, variance\n");
  for (j = 1; j < NIN; j++) {
    printf("%2d %6.2f %6.2f %5.2f\n",j, matchups[j].average(), 
            matchups[j].rms(),
            sqrt(-matchups[j].average()*matchups[j].average() + 
                  matchups[j].rms()*matchups[j].rms())          );
    fflush(stdout);
  }

  best = 0.;
  for (i = 0; i < nparms; i++) {
    regress[i].resize(population);
    sdevs[i].resize(population);
  }
  for (i = 0; i < trial.xpoints(); i++) {
    trial[i] = 0.;
  }

  float       eval = evaluate(matchups, nparms, ntrain, toler, trial);
  float score_null = score(toler, matchups[TERR], 0, ntrain);
  float score_best = score_null, score_raw = score_null;
  if (score_null != eval) {
    printf("%f v %f fails\n",score_null, eval);
    return 1;
  }

//////////////////////////////////////////////////////////////////////
// use indices 1-5 (gfs vars) to predict index 8 (TERR, difference between model and obs t2m) 
// --> 5 floats (slopes of dependencies) (?range [+-1], precision/sigma ? )
// --> 1 float (mean) (+- 30)(precision?)
// Evolutionary strategy, let the slopes be gaussian (0,1) to start with, 
//   can evolve towards 0 for parms to ignore, can grow as needed. Setting sdev allows
//   for parms to constrain themselves

  int gen_best = 0;

// Develop the initial population:
// note the negative sign
  j = TERR;
  best[0]    = -matchups[j].average();
  sd_best    = 0.5;
  sd_best[0] = sqrt(-matchups[j].average()*matchups[j].average() +
                     matchups[j].rms()*matchups[j].rms())   ;

  eval = evaluate(matchups, nparms, ntrain, toler, best);
  printf("score before %f and after bias correction %f, %f\n",score_best, eval, eval/score_best);
  score_null = eval;

  printf("preserve member 0 with the best so far. Elitist 1+N scheme\n");
  newpop(regress, sdevs, best, sd_best);
// Iterate over generations:
  int generations, last_update = 0;

  printf("good strategies are printed out with:\n");
  printf("generation pop_member_no fraction of original score (lower is better) then n pairs of value,sd_value for n parameters\n");
  printf("if a new best is found, it is printed out on a 'best' line\n");
  for (generations = 0; generations < genmax; generations++) {
    for (j = 1; j < population; j++) {
      for (parm = 0; parm < nparms; parm++) {
           trial[parm] = regress[parm][j];
        trial_sd[parm] =   sdevs[parm][j];
      }
      scores[j] = evaluate(matchups, nparms, ntrain, toler, trial);
      if (scores[j] <= score_null) {
        printf("good %4d %3d %f ",generations, j, scores[j]/score_null );
        for (int pindex = 0; pindex < nparms; pindex++) {
          printf("%6.3f %6.3f  ",trial[pindex], trial_sd[pindex]);
        }
        printf("\n");
        //printf("%7.3f  %6.3f %6.3f %6.3f %6.3f %6.3f  %7.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",
        //          trial[0], trial[1], trial[2], trial[3], trial[4], trial[5],
        //          trial_sd[0], trial_sd[1], trial_sd[2], trial_sd[3], trial_sd[4], trial_sd[5]);
      }
      // Improvement:
      if (scores[j] < score_best) {
        gen_best = generations;
        for (i = 0; i < nparms; i++) {
          best[i]    = trial[i];
          sd_best[i] = trial_sd[i];
        }

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
        for (int pindex = 0; pindex < nparms; pindex++) {
          printf("%6.3f %6.3f  ",best[pindex], sd_best[pindex]);
        }
        printf("\n");
        //printf("%7.3f  %6.3f %6.3f %6.3f %6.3f %6.3f  %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",best[0], 
        //           best[1], best[2], best[3], best[4], best[5],
        //       sd_best[0], sd_best[1], sd_best[2], sd_best[3], sd_best[4], sd_best[5]
        //     );
      }

    } // population

    if (sd_best[0] > 100) sd_best[0] = 10.;
    if (sd_best[0] < 0.05) sd_best[0] = 0.5;

    newpop(regress, sdevs, best, sd_best);

  } // end looping over generations


  // print final score and genes that get there
  display(matchups, nparms, nobs, best, sd_best);

  mvector<float> delta(nobs);
  make_vector(matchups, nparms, 0, nobs, toler, best, delta);
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
