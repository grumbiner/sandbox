#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>  
#include <fcntl.h>


#include "genes.h"

#include "mvector.h"
#include "ncepgrids.h"

#define HIRES
#include "ssmi.h"
// Program to experiment with training a genetic algorithm to
//   flag weather points.
// Robert Grumbine 27 July 2001

#define POPULATION 250

#define FLOAT_TYPE 1
#define INT_TYPE   0

#include "genes.h"
  
// Functions which are somewhat or very specialized towards the
// problem at hand.
void transcriber(mvector<int> &genome, mvector<united> &weights, genetic_code &x) ;
void showgenes(FILE *fout, mvector<int> &genome, genetic_code &x) ;

float scorer(global_ice<float> &sst, global_ice<unsigned char> &land, 
             global_ice<float> &ice, mvector<ssmi_bufr_line> &test_data, 
             mvector<united> &weights ) ;

void data_scale(ssmi_bufr_line &tmp);
float scale(float x, float lower, float upper) ;

////////////////// Main program ///////////////
int main(int argc, char *argv[]) {
//Gene-related:
  genetic_code x;
  mvector<united> weights(x.code_length);
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);
  float best, mean;
  int generation = 0, genmax = 40, epoch, epochmax=2;

// Problem-related:
  mvector<ssmi_bufr_line> test_data(1000);
  global_ice<float> sst, ice;
  global_ice<unsigned char> land;
  int i, j;
  int fin;
  ssmi_bufr_line line_tmp;
  FILE *sstin;

//Initialize:
  srand(1);

  printf("code length = %d\n",x.code_length);
  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(x.code_length);
     newgenes(genome[i]);
        #ifdef VERBOSE2
        transcriber(genome[i], weights, x);
        showgenes(stdout, genome[i], x); fflush(stdout);
        #endif
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
    test_data.resize(1000*pow(2, epoch) );
  }
  for (i = 0; i < test_data.xpoints(); i++) {
    j = 1 + (int) ((40./(pow(2,epoch)) * rand()) / (RAND_MAX+1.0));
    lseek(fin, j*sizeof(ssmi_bufr_line), SEEK_CUR);
    read(fin, &line_tmp, sizeof(ssmi_bufr_line));
    data_scale(line_tmp);
    test_data[i] = line_tmp;
  }
  close(fin);     
  printf("finished reading in data\n"); fflush(stdout);
 
  do {
    #ifdef SYMMETRY_TEST
    int alpha, beta, gamma;
    if (generation == 0) {
      // Test for scores being symmetric
      weights[3].fval = 0.5;
      weights[2].ival = 2;
      for (alpha = 0; alpha < 7; alpha++) {
      for (beta = 0; beta < 7; beta++) {
         weights[0].ival = alpha;
         weights[1].ival = beta;
         printf("%1d %1d %f ",weights[0].ival, weights[1].ival, scorer(sst, land, ice, test_data, weights) ) ;
         weights[0].ival = beta;
         weights[1].ival = alpha;
         printf("%1d %1d %f\n",weights[0].ival, weights[1].ival, scorer(sst, land, ice, test_data, weights) ) ; 
          
      }
      }
      return 3;
    }
    #endif
         
    generation += 1;
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights, x);
        scores[i] = scorer(sst, land, ice, test_data, weights);
        printf("%d %f ",i, scores[i]); fflush(stdout);
        showgenes(stdout, genome[i], x); fflush(stdout);
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
      showgenes(stdout, genome[i], x);
      fflush(stdout);
    }
    printf("\n");

    grazer(genome, scores);
  } while (best > 1.125*mean && best < 0.95 && generation < genmax);

  printf("\nepoch %d\n", epoch);
  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i], x);
  }

} // end of epochs

  return 0;

}

/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.

// utility function declaration 
float flagdet(ssmi_bufr_point &obs, mvector<united> &weights) ;
void calc(float ice, float sst, unsigned char land, float flag, float &tmp_t1errs, float &tmp_t2errs) ;

float scorer(global_ice<float> &sst, global_ice<unsigned char> &land, global_ice<float> &ice, mvector<ssmi_bufr_line> &test_data, mvector<united> &weights ) {
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
     calc(ice[locij], sst[locij], land[locij], flag, tmp_t1errs, tmp_t2errs);
     t1errs += tmp_t1errs;
     t2errs += tmp_t2errs;

     if (ice[locij] >= 15. && land[locij] == (unsigned char) 0) {
       realice += 1.;
     }
     if (land[locij] == (unsigned char) 0 && ice[locij] < 15.) {
       realocean += 1.;
     }

     count += 1.;
     if (flag == 1.0 && land[locij] == (unsigned char) 0) {
       t1count += 1; 
     }
     else if (flag == 0.0 && land[locij] == (unsigned char) 0) {
       t2count += 1;
     }
    
  }
  } 

  podice =   (t1count - t1errs) / realice;
  podocean = (t2count - t2errs) / realocean;
  weight = 4.*max(0.,podice-(1.-1./4.)) * (1./1.)*max(0.,podocean-0.);
  printf("%8.5f %8.5f  %8.5f   %1d %1d %1d %f\n", podice, podocean, weight, 
        weights[0].ival, weights[1].ival, weights[2].ival, weights[3].fval); 

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

  // auxiliary 
  else if (land != (unsigned char) 0) {
    return;
  }
  else {
    printf("bad case, sst ice flag land = %f %f %f %d\n",
         sst, ice, flag, (int) land);
  }

  return;
}
float flagdet(ssmi_bufr_point &obs, mvector<united> &weights) {
  float parmtmp, flagret = 0, cutoff;
  float add = 1.;
  int i, j, op;
  float temps[7], t1, t2;
  
  temps[0] = obs.t19v;
  temps[1] = obs.t19h;
  temps[2] = obs.t22v;
  temps[3] = obs.t37v;
  temps[4] = obs.t37h;
  temps[5] = obs.t85v;
  temps[6] = obs.t85h;

  t1     = temps[weights[0].ival];
  t2     = temps[weights[1].ival];
  op     =       weights[2].ival;
  cutoff =       weights[3].fval;

  switch (op) {
    case 0:
      parmtmp = (t1 + t2)/2.;
      break;
    case 1:
      parmtmp = (t1 - t2 + 1.)/2.;
      break;
    case 2:
      parmtmp = (t1*t2);
      break;
    case 3:
      parmtmp = ((t1+1.)/(t2+1.) - 0.5 ) / 1.5;
      break;
    case 4:
      parmtmp = t1;
      break;
  }
  //printf("%f %f  %f\n",t1, t2, parmtmp);

  if (parmtmp > cutoff) {
    flagret = 1.;
  }

  return flagret;
}


///////////////////////////////////////////

///////////// Ensure the data are in the 0-1 range:
void data_scale(ssmi_bufr_line &tmp) {
  int i;
  float lower = 50.0, upper = 300.0;
  for (i = 0; i < NSCANS; i++) {
    tmp.full[i].t19v = scale(tmp.full[i].t19v, lower, upper);
    tmp.full[i].t19h = scale(tmp.full[i].t19h, lower, upper);
    tmp.full[i].t22v = scale(tmp.full[i].t22v, lower, upper);
    tmp.full[i].t37v = scale(tmp.full[i].t37v, lower, upper);
    tmp.full[i].t37h = scale(tmp.full[i].t37h, lower, upper);
    tmp.full[i].t85v = scale(tmp.full[i].t85v, lower, upper);
    tmp.full[i].t85h = scale(tmp.full[i].t85h, lower, upper);
  }
  return;
}
float scale(float x, float lower, float upper) {
  if (x < lower) return 0;
  if (x > upper) return upper;
  return (x - lower) / (upper - lower);
}
   
