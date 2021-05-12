#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ncepgrids.h"
#include "time_series.h"

#include "genes.h"
#define POPULATION 500


void soiread(time_series<float> &soi, int years) ;

float scorer(mvector<united> &weights, time_series<float> &icec,
             mvector<float> &icevar, time_series<float> &soi, int lead) ;

void displayer(mvector<int> &genome, time_series<float> &icec, time_series<float> &soi, int lead, genetic_code &x) ;

#define NGENES 5
#define LEADER 31
#define NYEARS 90

int main(int argc, char *argv[]) {
  genetic_code x(NGENES);
  mvector<mvector<int> > genome(POPULATION);
  mvector<mvector<int> > elite(POPULATION);
  mvector<united> weights;
  mvector<float> scores(POPULATION);
  time_series<float> soi(NYEARS*12), icec(NYEARS*12);
  northgrid<float> fullgrid;
  mvector<float> avgs(12), soiavg(12), icevar(12);
  int i, j, lead;
  float best, mean;
  int generation = 0, genmax = 400;
  united f1, f2;
  FILE *fin;
  ijpt loc;
  fijpt floc;
  latpt ll;

//Initialize:
  srand(1);

  lead = atoi(argv[1]);
  soiread(soi, NYEARS);
  soi /= 256.; // rescale spots to be more in the line of 0-1

  // Get hold of the ice concentrations:
  fin = fopen("nhallmonths.bin","r");

  ll.lat =  atof(argv[2]); ll.lon = atof(argv[3]); 
  floc = fullgrid.locate(ll);
  loc = floc;
  printf("Location is %f %f  %d %d\n",ll.lat, ll.lon, loc.i, loc.j);

  for (i = 0; i < icec.xpoints(); i++) {
    fullgrid.binin(fin);
    icec[i] = fullgrid[loc];
  }
  fclose(fin);
  printf("finished reading in ice concentrations\n"); fflush(stdout);
  
  // Remove the monthly averaged values in icec so as to work with
  // ice concentration anomalies:
  avgs = (float) 0.0;
  soiavg = (float) 0.0;
  icevar = (float) 0.0;
  for (i = 0 ; i < icec.xpoints(); i++) {
    avgs[i%12] += icec[i];
    soiavg[i%12] += soi[i];
  }
  avgs /= NYEARS;
  soiavg /= NYEARS;
  for (i = 0; i < icec.xpoints(); i++) {
    icec[i] -= avgs[i%12];
    soi[i]  -= soiavg[i%12];
    icevar[i%12] += icec[i]*icec[i];
  }
  for (i = 0; i < 12; i++) {
    icevar[i] = sqrt(icevar[i]/(NYEARS-1) );
    printf("%f %f %f\n",avgs[i], soiavg[i], icevar[i]);
  }

// Set up the genome
  f1.fval = -1.; f2.fval = 1.;
  x.newgene(0, 7, FLOAT_TYPE, f1, f2);
  f1.fval = -1.; f2.fval = 1.;
  x.newgene(1, 7, FLOAT_TYPE, f1, f2);

  f1.ival = 0; f2.ival = 31;
  x.newgene(2, 5, INT_TYPE, f1, f2);
  f1.ival = 0; f2.ival = 31;
  x.newgene(3, 5, INT_TYPE, f1, f2);
  x.newgene(4, 5, INT_TYPE, f1, f2);

  weights.resize(x.ncodes);
  printf("ncodes, code length = %d %d\n",x.ncodes, x.code_length);

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(x.code_length);
     elite[i].resize(x.code_length);
     newgenes(genome[i]);
  }


// Now start evaluating/evolving:
  scores = (float) 0.0;
  //printf("before loop, score max = %f\n",scores.maximum() );
  do {
    generation += 1;
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights, x);
        scores[i] = scorer(weights, icec, icevar, soi, lead);
      } 
    }
    best = scores.maximum();
    mean = scores.average();
    printf("generation %d scores: %f %f %f %f\n", generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
    fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);
    grazer(genome, scores);
  } while (best > 1.125*mean && generation < genmax);

  printf("\n");
  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i], x);
  }
  displayer(genome[0], icec, soi, lead, x);

  return 0;

}
/////////////////////////////////
void displayer(mvector<int> &genome, time_series<float> &icec, 
               time_series<float> &soi, int lead, genetic_code &x) {
  float est, tmp_w;
  int i;
  mvector<united> weights(x.code_length);

  transcriber(genome, weights, x);
  for (i = LEADER; i < soi.xpoints() - lead ; i++) {
    est = weights[0].fval* icec [i-weights[2].ival] + 
          weights[1].fval* icec [i-weights[3].ival];
    printf("%4d  %5.2f %5.2f %6.3f\n",i, est, icec[i+lead], est - icec[i+lead]);
  }
  return;
}
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.
//  weights run back N time steps from current
float scorer(mvector<united> &weights, time_series<float> &icec, 
             mvector<float> &icevar,  time_series<float> &soi, int lead) {
  float est, tmp_w = 0., tmp_avg = 0., score;
  float avg = 0;
  int i;

  for (i = LEADER; i < icec.xpoints()-lead; i++) {
    avg += icec[i+lead];
  }
  avg /= icec.xpoints()-lead-LEADER;

  for (i = LEADER; i < icec.xpoints()-lead; i++) {
    est = weights[0].fval* icec [i-weights[2].ival] + 
          weights[1].fval* icec [i-weights[3].ival];
          //weights[1].fval* icec [i-weights[3].ival]*icec[i-weights[4].ival];
    tmp_w   += (icec[i+lead] - est) * (icec[i+lead] - est) / icevar[i%12];
    tmp_avg += (icec[i+lead] - avg) * (icec[i+lead] - avg) / icevar[i%12];
  }
  score =  100.*(1. - tmp_w / tmp_avg ); // percent variance explained
  if (score > 100.) {
    printf("tmp_w = %f tmp_avg = %f\n",tmp_w, tmp_avg);
  }
  return score;
 
}
//Read in sunspot numbers:
void soiread(time_series<float> &spots, int years) {
   float d1, d2, d3, d4;
   int i;
   FILE *fin;

   fin = fopen("spot_num.txt","r");

   // d1 is year
   // d2 is month
   // d3 is spot number
   // d4 is variance of spot number
   i = 0;
   while(!feof(fin) && i < spots.xpoints() ) {
     fscanf(fin, "%f %f %f %f\n",&d1, &d2, &d3, &d4);
     if (d1 < 1991 - years) { 
       continue;
     }
     else {
       spots[i] = d3;
       i += 1;
     }
   } 

  printf("finished reading in sunspots\n"); fflush(stdout);
  return;
}

