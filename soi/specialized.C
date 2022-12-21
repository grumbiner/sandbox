#include "mvector.h"
#include "ncepgrids.h"
#include "ssmi.h"

#include "collect.C"

// Functions which are somewhat or very specialized towards the
// problem at hand.
void transcriber(mvector<int> &genome, mvector<float> &weights) ;
void showgenes(FILE *fout, mvector<int> &genome) ;
void newgenes(mvector<int> &genome) ;
void reproducer(mvector<mvector<int> > &genomes, mvector<float> &scores) ;
float scorer(global_ice<float> &sst, global_ice<float> &ice, mvector<ssmi_bufr_line> &test_data, mvector<float> &weights ) ;
void displayer(mvector<int> &genome, mvector<float> &soi) ;
float flagdet(ssmi_bufr_point &obs, mvector<float> &weights) ;
void calc(float ice, float sst, float flag, float &tgood, float &tbad) ;

///////////////////////////////////////////
// Transcribe a hierarchal genome into a mvector of floating point weights
//   determine number of bits per gene from fact that number of weights
//   will be given
void transcriber(mvector<int> &genome, mvector<float> &weights) {
  int ngenes = weights.xpoints();
  int bits   = genome.xpoints()/weights.xpoints() - 1;

  int i, j, mul, base, tmp;
  float val;

  // hardwire the genome structure here, for weather filter.
  bits = 9;
  weights[0] = genome[0] + 2*genome[1] + 4*genome[2] + 8*genome[3]
                 + 16*genome[4];
  weights[0] = (float) ((int) weights[0] % 24);
//  for (i = 0; i < ngenes; i++) {
    i = 1;
    base = 4;
    mul = 1.;
      tmp = 0;
      for (j = base+1; j < base + bits; j++) {
        tmp += mul*genome[j];
        mul *= 2;
      }
      // +-1 weights[i] = (float) tmp / (pow(2., (float)bits-1) - 1.);
      // 0 - +2
      weights[i] = (float) tmp / (pow(2., (float)bits-1) );
      // +-4 weights[i] = (float) tmp / (pow(2., (float)bits-3) );
//  }

  return ;
}
/////////////////////////////////
void newgenes(mvector<int> &genome) {
  int i;
  for (i = 0; i < genome.xpoints(); i++) {
    genome[i] = (int) (0.5 + (1.0*rand()) /(RAND_MAX+1.0) );
  }
  return;
}
void showgenes(FILE *fout, mvector<int> &genome) {
  int i;
  mvector<float> weights(GENES);
  transcriber(genome, weights);
  for (i = 0; i < weights.xpoints(); i++) {
    fprintf(fout, "%8.5f  ",weights[i]);
  }
  fprintf(fout, "\n");
  return;
}
/////////////////////////////////
void reproducer(mvector<mvector<int> > &genomes, mvector<float> &scores) {
  float average = scores.average(), reqt;
  int i, j, nbetter;
  float total, running, trand;
  int nparents = 0, parent1, parent2;
  int iter = 0, tries;
  mvector<float> pcross(scores.xpoints() );

  nbetter = 0;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > average) nbetter += 1;
  }
  printf("%d genomes of %d above average  ",nbetter, genomes.xpoints() );

  if (nbetter > genomes.xpoints()/2) {
    reqt = average + 0.25 * (scores.maximum() - average) ;
  }
  else {
    reqt = average;
  }
  reqt = max(-100.0, reqt);
// Weight roulette wheel by score, high score is better
  total = 0.0;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > reqt) { total += scores[i]; }
    else {
      scores[i] = 0;
    }
  }
  running = 0.;
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] > 0.0) {
      nparents += 1;
      running += scores[i] / total;
      pcross[i] = running;
    }
  }

  if (nparents != 1) {
    for (i = 0; i < genomes.xpoints(); i++) {
      if (scores[i] == 0. ) {
        if ((1.0*rand()) /(RAND_MAX+1.0) < PCROSS ) {
          trand = (1.0*rand()) /(RAND_MAX+1.0);
          parent1 = find_parent(pcross, trand);
          tries = 0;
          do {
            tries += 1;
            trand = (1.0*rand()) /(RAND_MAX+1.0);
            parent2 = find_parent(pcross, trand);
            //printf("parent2 parent1 %d %d %d\n", parent2, parent1, nparents);
          } while (parent2 == parent1 && tries < 10);
          crossover(genomes, parent1, parent2, i);
        }
        else {
          newgenes(genomes[i]);
        }
      }
    }
  }
  // end of horizontal transfer/crossover.  Skip if only 1 parent

  // apply mutation:
  for (i = 0; i < genomes.xpoints(); i++) {
    if (scores[i] <= reqt) {
      mutate(genomes[i]);
      // Note that we don't have to worry about lethal mutations
    }
  }

  return;

}
/////////////////////////////////
float scorer(global_ice<float> &sst, global_ice<float> &ice, mvector<ssmi_bufr_line> &test_data, mvector<float> &weights ) {
  float good = 0., bad = 0.;
  float tgood, tbad;
  ijpt locij;
  latpt lloc;
  int i, j;
  float flag;

  for (i = 0; i < test_data.xpoints(); i++) {
  for (j = 0; j < NSCANS; j++) {
     lloc.lat = test_data[i].full[j].latitude;
     lloc.lon = test_data[i].full[j].longitude;
     locij = sst.locate(lloc);
     flag = flagdet(test_data[i].full[j], weights);
     tgood = 0; tbad = 0;
     calc(ice[locij], sst[locij], flag, tgood, tbad);
     good += tgood;
     bad += tbad;
  }
  }
  //printf("good bad %f %f\n",good, bad);
  return exp(10.*good/(bad+good)) ;
}
/////////////////////////////////
void calc(float ice, float sst, float flag, float &tgood, float &tbad) {
// Note that flag = 1 for ice, 0 for not-ice.
  float tcrit = 2.0;
  tgood = 0.; tbad = 0.;
  if (flag == 1 && sst <= tcrit) {
    //tgood = 8*(tcrit - sst);
    tgood = 1*(8*(tcrit - sst));
  }
  else if (sst > tcrit) {
    //tbad = sst;
    tbad = 1*(sst);
  }
  else if (flag == 0 && sst < tcrit) {
    //tbad = sst;
    tbad = 1*(8.*(tcrit - sst));
  }
  else if (sst >= tcrit) {
    //tgood = 8*(tcrit-sst);
    tgood = 1*(sst);
  }
  else {
    printf("error in calc %f %f\n",flag, sst);
  }
  return;
}
/////////////////////////////////
float flagdet(ssmi_bufr_point &obs, mvector<float> &weights) {
  float parmtmp, flagret = 0;
  switch((int) weights[0] ) {
    case 0:
       parmtmp = obs.t19v/obs.t19h;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 1:
       parmtmp = obs.t19v/obs.t19h;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 2:
       parmtmp = obs.t37v/obs.t37h;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 3:
       parmtmp = obs.t37v/obs.t37h;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 4:
       parmtmp = obs.t85v/obs.t85h;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 5:
       parmtmp = obs.t85v/obs.t85h;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 6:
       parmtmp = obs.t22v/obs.t19v;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 7:
       parmtmp = obs.t22v/obs.t19v;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 8:
       parmtmp = obs.t37v/obs.t19v;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 9:
       parmtmp = obs.t37v/obs.t19v;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 10:
       parmtmp = obs.t85v/obs.t19v;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 11:
       parmtmp = obs.t85v/obs.t19v;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 12:
       parmtmp = obs.t37v/obs.t22v;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 13:
       parmtmp = obs.t37v/obs.t22v;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 14:
       parmtmp = obs.t85v/obs.t22v;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 15:
       parmtmp = obs.t85v/obs.t22v;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 16:
       parmtmp = obs.t85v/obs.t37v;
       if (parmtmp > weights[1]) flagret = 1.; break;
    case 17:
       parmtmp = obs.t85v/obs.t37v;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 18:
       parmtmp = (obs.t85v - obs.t19v) / (obs.t85v + obs.t19v);
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 19:
       parmtmp = (obs.t85v - obs.t22v) / (obs.t85v + obs.t22v);
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 20:
       parmtmp = (obs.t85v - obs.t37v) / (obs.t85v + obs.t37v);
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 21:
       parmtmp = (obs.t37v - obs.t19v) / (obs.t37v + obs.t19v);
       if (parmtmp < weights[1]) flagret = 1.; break;
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 22:
       parmtmp = (obs.t37v - obs.t22v) / (obs.t37v + obs.t22v);
       if (parmtmp < weights[1]) flagret = 1.; break;
    case 23:
       parmtmp = (obs.t22v - obs.t19v) / (obs.t22v + obs.t19v);
       if (parmtmp < weights[1]) flagret = 1.; break;
    default:
      printf("algorithm type out of range\n");
  }

  return flagret;
}
/////////////////////////////////
void displayer(mvector<int> &genome, mvector<float> &soi) {
  float est, tmp_w;
  int i, j, lead = 0;
  mvector<float> weights(GENES);
  transcriber(genome, weights);
  for (i = weights.xpoints(); i < soi.xpoints() - lead -1; i++) {
     est = 0.0;
     for (j = 0; j < weights.xpoints(); j++) {
        est += soi[i-j]*weights[j];
     }
     printf("%d  %f %f\n",i, est, soi[i+lead]);
  }
  return;
}
/////////////////////////////////

