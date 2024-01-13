#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>  
#include <fcntl.h>


#include "mvector.h"
#include "ncepgrids.h"

#define HIRES
#include "ssmi.h"
// Program to experiment with training a genetic algorithm to
//   flag weather points.
// Robert Grumbine 27 July 2001

#include "collect.C"
#include "specialized.C"

#define NLINES 3000
int main(int argc, char *argv[]) {
  mvector<mvector<int> > genome(POPULATION);
  mvector<mvector<int> > elite(POPULATION);
  mvector<float> weights(GENES);
  mvector<float> scores(POPULATION), elite_scores(POPULATION);
  mvector<ssmi_bufr_line> test_data(NLINES);
  global_ice<float> sst, ice;
  global_ice<unsigned char> land;
  int i, j;
  float best, mean;
  int generation = 0, genmax = 200, epoch, epochmax=1;
  int fin;
  ssmi_bufr_line line_tmp;
  FILE *sstin;

//Initialize:
  srand(1);

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(GENES*(BITS+1));
     elite[i].resize(GENES*(BITS+1));
     newgenes(genome[i]);
  }

//Obtain some data to work with (generic step with specialized implementation)
  sstin = fopen("sst", "r");
  if (sstin == (FILE *) NULL ){
    printf("failed to open sst file\n");
    return 2;
  }
  sst.binin(sstin);
  fclose(sstin);
  if (sst.average() > 30.) sst -= 273.15; // convert to C

  sstin = fopen("land", "r");
  if (sstin == (FILE *) NULL) {
    printf("failed to open land file\n");
    return 2;
  }
  land.binin(sstin);
  fclose(sstin);

  sstin = fopen("ice","r");
  if (sstin == (FILE *) NULL) {
    printf("failed to open ice filt\n");
    return 3;
  }
  ice.binin(sstin);
  fclose(sstin);
  if (ice.average() < 10.) ice *= 100.; // convert to percents

// Now start evaluating/evolving:
  scores = (float) 0.0;
  elite_scores = (float) 0.0;

// Loop for a number of epochs -- epoch = new pass through the data 
//   files:
for (epoch = 0; epoch < epochmax; epoch++) {
  generation = 0;
  fin = open(argv[1], O_RDONLY);
  if (fin <= 0) {
    printf("Failed to open %s\n",argv[1]);
    printf("Quitting now!\n");
    return 1;
  }
  if (epoch != 0) {
    test_data.resize(NLINES*pow(2, epoch) );
  }
  for (i = 0; i < test_data.xpoints(); i++) {
    j = 1 + (int) ((40./(pow(2,epoch)) * rand()) / (RAND_MAX+1.0));
    lseek(fin, j*sizeof(ssmi_bufr_line), SEEK_CUR);
    read(fin, &line_tmp, sizeof(ssmi_bufr_line));
    test_data[i] = line_tmp;
  }
  close(fin);     
 
  weights[0] = 21.;
  weights[1] = 1.;
  printf("Score of the team filter is %f in epoch %d\n",
        scorer(sst, land, ice, test_data, weights), epoch );
  do {
    generation += 1;
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights);
        scores[i] = scorer(sst, land, ice, test_data, weights);
      } 
    }
    best = scores.maximum();
    mean = scores.average();
    printf("epoch %d generation %d scores: %f %f %f %f\n", epoch, generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
    fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);

    printf("\ngeneration %3d top 15 list\n", generation);
    for (i = 0; i < 15; i++) { 
      printf("score %f ",scores[i]);
      showgenes(stdout, genome[i]);
      fflush(stdout);
    }
    printf("\n");

    grazer(genome, scores);
  } while (best > 1.125*mean && best < 0.95 && generation < genmax);

  printf("\nepoch %d\n", epoch);
  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i]);
  }

} // end of epochs

  return 0;

}
/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.


float scorer(global_ice<float> &sst, global_ice<unsigned char> &land, global_ice<float> &ice, mvector<ssmi_bufr_line> &test_data, mvector<float> &weights ) {
  float t1errs = 0., t2errs = 0.;
  float tmp_t1errs, tmp_t2errs, count = 0.;
  ijpt locij;
  latpt lloc;
  int i, j;
  float flag, t1count = 0., t2count = 0., realice = 0., realocean = 0., weight;
  float podice, podocean;

  for (i = 0; i < test_data.xpoints(); i++) {
  for (j = 0; j < NSCANS; j++) {
     lloc.lat = test_data[i].full[j].latitude; 
     lloc.lon = test_data[i].full[j].longitude; 
     locij = sst.locate(lloc);
     flag = flagdet(test_data[i].full[j], weights);
     tmp_t1errs = 0; tmp_t2errs = 0;
     calc(ice[locij], sst[locij], land[locij], flag, tmp_t1errs, tmp_t2errs);
     if (ice[locij] >= 15. && land[locij] == (unsigned char) 0) {
       realice += 1.;
     }
     if (land[locij] == (unsigned char) 0 && ice[locij] < 15.) {
       realocean += 1.;
     }
     t1errs += tmp_t1errs;
     t2errs += tmp_t2errs;
     count += 1.;
     if (flag == 1. && land[locij] == (unsigned char) 0) {
       t1count += 1; 
     }
     else if (flag == 0. && land[locij] == (unsigned char) 0) {
       t2count += 1;
     }
    
  }
  } 

  podice =   (t1count - t1errs) / realice;
  podocean = (t2count - t2errs) / realocean;
  weight = 16.*max(0.,podice-(1.-1./16.) ) * (2./1.)*max(0., podocean-0.5);
  printf("%3.0f %f  %f %f  %f\n",weights[0], weights[1], podice, podocean, 
         weight);

  return weight;

}
void calc(float ice, float sst, unsigned char land, float flag, float &type1, float &type2) {
// Note that flag = 1 for ice, 0 for not-ice.
  float tcrit = 2.0;
  type1 = 0.;
  type2 = 0.;

  // type 1:
  if (flag == 1 && ice >= 15. && land == (unsigned char) 0 ) {
    type1 = 0.;
  }
  else if (flag == 1 && ice < 15. && land == (unsigned char) 0 ) {
    type1 = 1.;
  }

  //type 2 error count
  else if (flag == 0 && ice < 15.  && land == (unsigned char) 0 ) {
    type2 = 0.;
  }
  else if (flag == 0 && ice >= 15. && land == (unsigned char) 0 ) {
    type2 = 1.; 
  }
  else if (land != (unsigned char) 0) {
    return;
  }
  else {
    printf("bad case, sst ice flag land = %f %f %f %d\n",
         sst, ice, flag, (int) land);
  }

  return;
}
