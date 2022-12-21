#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ncepgrids.h"
#include "time_series.h"
#include "genes.h"
#define POPULATION 500


void soiread(time_series<float> &soi, int years) ;

float scorer(mvector<united> &weights, time_series<float> &icec,
                      time_series<float> &soi, int lead) ;

void displayer(mvector<int> &genome, time_series<float> &icec, time_series<float> &soi, int lead, genetic_code &x) ;

#define NGENES 4
#define LEADER 31
#define NYEARS 90

int globalcount = 0;

int main(int argc, char *argv[]) {
  genetic_code x(NGENES);
  mvector<mvector<int> > genome(POPULATION);
  mvector<mvector<int> > elite(POPULATION);
  mvector<united> weights;
  mvector<float> scores(POPULATION);
  time_series<float> soi(NYEARS*12), icec(NYEARS*12);
  northgrid<float> fullgrid;
  mvector<float> avgs(12), soiavg(12);
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
  for (i = 0 ; i < icec.xpoints(); i++) {
    avgs[i%12] += icec[i];
    soiavg[i%12] += soi[i];
  }
  avgs /= NYEARS;
  soiavg /= NYEARS;
  for (i = 0; i < icec.xpoints(); i++) {
    icec[i] -= avgs[i%12];
    soi[i]  -= soiavg[i%12];
  }

// Set up the genome
  f1.fval = -4.; f2.fval = 4.;
  x.newgene(0, 7, FLOAT_TYPE, f1, f2);
  f1.fval = -4.; f2.fval = 4.;
  x.newgene(1, 7, FLOAT_TYPE, f1, f2);

  f1.ival = 0; f2.ival = 31;
  x.newgene(2, 5, INT_TYPE, f1, f2);
  f1.ival = 0; f2.ival = 31;
  x.newgene(3, 5, INT_TYPE, f1, f2);

//  x.newgene(4, 5, INT_TYPE, f1, f2);

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
      // Variant for use of globalcount -- evaluate score every time through.
        transcriber(genome[i], weights, x);
        scores[i] = scorer(weights, icec, soi, lead);
      } 
    }
    best = scores.maximum();
    mean = scores.average();
    printf("generation %d scores: %f %f %f %f\n", generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
    fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);
    showgenes(stdout, genome[0], x);
    grazer(genome, scores);
  } while (best > 1.125*mean && generation < genmax);

  printf("\n");
  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i], x);
  }
  displayer(genome[0], icec, soi, lead, x);

  printf("globalcount = %d\n",globalcount);
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
          weights[1].fval* icec  [i-weights[3].ival];
    printf("%4d  %5.2f %5.2f %6.3f\n",i, est, icec[i+lead], est-icec[i+lead]);
  }
  return;
}
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.
//  weights run back N time steps from current
// Variant -- score is number of times correctly over 0.2 in magnitude,
//            minus number of times truth is over but pred isn't, 
//            minus number of times pred is over but truth isn't.
float scorer(mvector<united> &weights, time_series<float> &icec, 
                            time_series<float> &soi, int lead) {
  float est, tmp_w = 0., tmp_avg = 0., score;
  float cut = 0.20;
  int i;
  int pod = 0, far = 0, poss = 0, imposs = 0;

  for (i = LEADER; i < icec.xpoints()-lead; i++) {
    est = weights[0].fval* icec [i-weights[2].ival] + 
          weights[1].fval* icec [i-weights[3].ival];
    if (fabs(icec[i+lead]) >= cut && fabs(est) >= cut) {
      poss += 1;
    }
    else if (fabs(icec[i+lead]) >= cut && fabs(est) < cut) {
      poss -= 2;
    }
    else if (fabs(icec[i+lead]) < cut && fabs(est) < cut) {
      poss += 1;
    }
    else {
      poss -= 10;
    }
  }
//    if (fabs(icec[i+lead]) >= cut) {
//      poss += 1;
//      if (fabs(est) >= cut) {
//        pod += 1;
//      }
//    }
//    else {
//      imposs += 1;
//      if (fabs(est) >= cut) {
//        far += 1;
//      }
//    }
//  }
//
//  score =  (float) pod / (float) poss  -  (float) far / (float) imposs
//      *sqrt(globalcount/POPULATION/10); 
//  globalcount += 1;
  score = (float) poss;
  return score;

// Novel idea (?) -- alternate between scoring POD and FAR.  To survive,
//   genome must satisfy both ... but would need to re-evaluate genomes,
//    which current version doesn't.  Change above.
//  globalcount += 1;
//  if (globalcount % 2 == 1) {
//    return 100.*(float) pod / (float) poss;
//  }  
//  else {
//    return 100.* ( 1. - (float) far / (float) imposs );
//  }
 
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

