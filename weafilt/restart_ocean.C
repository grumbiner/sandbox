#include "ncepgrids.h"

// arguments are: icegrid, landgrid, Tb ascii file
#define NOBS 5520128

void scorer(metricgrid<float> &icec, metricgrid<unsigned char> &land, 
            mvector<ijpt> &locs, mvector<float> &obs, mvector<float> &scores, 
            float &critval) ;
void ranger(mvector<float> &vals, float &top, float &bottom, float &step) ;

int main(int argc, char *argv[]) {
  FILE *fin, fout;
  global_12th<float> gice;
  global_12th<unsigned char> gland;
  mvector< mvector<float> > obs(NOBS); // lat, lon, 7 Tb
  mvector<float> scores(2*4), trial;
  mvector<ijpt> ilocs(NOBS);

  float l1, l2, t1, t2, t3, t4, t5, t6, t7;
  int i, nobs;
  float top, bottom, step, critval;
  latpt tloc;


  fin = fopen(argv[1],"r");
  gice.binin(fin);
  fclose(fin);
  if (gice.gridmax() < 3) gice *= 100;
  printf("ice max min avg %f %f %f\n",
          gice.gridmax(), gice.gridmin(), gice.average() );

  fin = fopen(argv[2], "r");
  gland.binin(fin);
  fclose(fin);

  fin = fopen(argv[3], "r");

  i = 0;
  while (!feof(fin) ) {
    if ( (i % 1000000) == 0 ) {
      printf("i = %d\n",i); fflush(stdout);
    }

    obs[i].resize(9);

    fscanf(fin, "%f %f %f %f %f %f %f %f %f\n",
      &l1, &l2, &t1, &t2, &t3, &t4, &t5, &t6, &t7);
      obs[i][0] = l1;  obs[i][1] = l2;
      obs[i][2] = t1; obs[i][3] = t2; obs[i][4] = t3;
      obs[i][5] = t4; obs[i][6] = t5; obs[i][7] = t6; obs[i][8] = t7;
     
      tloc.lat = l1;
      tloc.lon = l2;
      ilocs[i] = gice.locate(tloc);
    i += 1;
  }
  fclose(fin);
  nobs = i - 1;

// work on the critical point tests:
  printf("nobs = %d\n",nobs);
  trial.resize(nobs);

  // t19v
  for (i = 0; i < nobs; i++) {
    trial[i] = obs[i][2]; 
  }
  //////////////// -- universal section, to make function -- //////////////
  ranger(trial, top, bottom, step);
  printf("for trial %d top bottom step = %f %f %f\n",2, top, bottom, step);
  for (critval = bottom; critval < top; critval += step ) {
    scorer(gice, gland, ilocs, trial, scores, critval);
    printf("trial %d gt critval %f  scores %f %f %f %f\n",2, critval, 
                 scores[0], scores[1], scores[2], scores[3]);
    printf("trial %d lt critval %f  scores %f %f %f %f\n",2, critval, 
                 scores[4], scores[5], scores[6], scores[7]);
  }
  // t19h
  for (i = 0; i < nobs; i++) {
    trial[i] = obs[i][3]; 
  }
  ranger(trial, top, bottom, step);
  printf("for trial %d top bottom step = %f %f %f\n",3, top, bottom, step);
  for (critval = bottom; critval < top; critval += step ) {
    scorer(gice, gland, ilocs, trial, scores, critval);
    printf("trial %d gt critval %f  scores %f %f %f %f\n",3, critval, 
                 scores[0], scores[1], scores[2], scores[3]);
    printf("trial %d lt critval %f  scores %f %f %f %f\n",3, critval, 
                 scores[4], scores[5], scores[6], scores[7]);
  }
  // t37v
  for (i = 0; i < nobs; i++) {
    trial[i] = obs[i][5]; 
  }
  ranger(trial, top, bottom, step);
  printf("for trial %d top bottom step = %f %f %f\n",5, top, bottom, step);
  for (critval = bottom; critval < top; critval += step ) {
    scorer(gice, gland, ilocs, trial, scores, critval);
    printf("trial %d gt critval %f  scores %f %f %f %f\n",5, critval, 
                 scores[0], scores[1], scores[2], scores[3]);
    printf("trial %d lt critval %f  scores %f %f %f %f\n",5, critval, 
                 scores[4], scores[5], scores[6], scores[7]);
  }
  // t37h
  for (i = 0; i < nobs; i++) {
    trial[i] = obs[i][6]; 
  }
  ranger(trial, top, bottom, step);
  printf("for trial %d top bottom step = %f %f %f\n",6, top, bottom, step);
  for (critval = bottom; critval < top; critval += step ) {
    scorer(gice, gland, ilocs, trial, scores, critval);
    printf("trial %d gt critval %f  scores %f %f %f %f\n",6, critval, 
                 scores[0], scores[1], scores[2], scores[3]);
    printf("trial %d lt critval %f  scores %f %f %f %f\n",6, critval, 
                 scores[4], scores[5], scores[6], scores[7]);
  }
  // t85v
  for (i = 0; i < nobs; i++) {
    trial[i] = obs[i][7];
  }
  ranger(trial, top, bottom, step);
  printf("for trial %d top bottom step = %f %f %f\n",7, top, bottom, step);
  for (critval = bottom; critval < top; critval += step ) {
    scorer(gice, gland, ilocs, trial, scores, critval);
    printf("trial %d gt critval %f  scores %f %f %f %f\n",7, critval, 
                 scores[0], scores[1], scores[2], scores[3]);
    printf("trial %d lt critval %f  scores %f %f %f %f\n",7, critval, 
                 scores[4], scores[5], scores[6], scores[7]);
  }
  // t85h
  for (i = 0; i < nobs; i++) {
    trial[i] = obs[i][8]; 
  }
  ranger(trial, top, bottom, step);
  printf("for trial %d top bottom step = %f %f %f\n",8, top, bottom, step);
  for (critval = bottom; critval < top; critval += step ) {
    scorer(gice, gland, ilocs, trial, scores, critval);
    printf("trial %d gt critval %f  scores %f %f %f %f\n",8, critval, 
                 scores[0], scores[1], scores[2], scores[3]);
    printf("trial %d lt critval %f  scores %f %f %f %f\n",8, critval, 
                 scores[4], scores[5], scores[6], scores[7]);
  }

// Now try binary combinations in the delta i-j / sum(i+j) sense
//  skip T22V channel (n = 4)
  int j, k, l;
  k = 9;
  for (i = 2; i <= 7; i++) {
    //if (i == 4) i++;
  for (j = i+1; j <= 8; j++) {
    //if (j == 4) j++;
    for (l = 0; l < nobs; l++) {
      trial[l] = (obs[l][i] - obs[l][j]) / (obs[l][i] + obs[l][j]);  
    }
    ranger(trial, top, bottom, step);
    printf("for trial %d i, j %d %d top bottom step = %f %f %f\n",
                k, i, j, top, bottom, step);
    for (critval = bottom; critval < top; critval += step ) {
      scorer(gice, gland, ilocs, trial, scores, critval);
      printf("trial %d gt critval %f  scores %f %f %f %f\n",k, critval,
                   scores[0], scores[1], scores[2], scores[3]);
      printf("trial %d lt critval %f  scores %f %f %f %f\n",k, critval,
                   scores[4], scores[5], scores[6], scores[7]);
    }
    k++;
  }
  }
  
  


  return 0;
}

void scorer(metricgrid<float> &icec, metricgrid<unsigned char> &land, 
            mvector<ijpt> &locs, mvector<float> &obs, mvector<float> &scores, 
            float &critval) {
  int totice = 0, totocean = 0;
  int prop_gtocean = 0, fals_gtocean = 0, prop_gtice = 0, fals_gtice = 0;
  int prop_ltocean = 0, fals_ltocean = 0, prop_ltice = 0, fals_ltice = 0;
  int i;
  float icecon;

  for (i = 0; i < obs.xpoints(); i++) {
    icecon = icec[locs[i] ];
    if (icecon == 0) {
      totocean += 1;
    }
    else if (icecon > 0 && icecon < 128  ) {
      totice += 1;
    }
    else {
      printf("impossible ice value %f\n",icecon );
    }

// greater than
    if (icecon == 0 && obs[i] > critval) {
      prop_gtocean += 1;
    }
    else if (icecon != 0 && obs[i] > critval)  {
      fals_gtocean += 1;
    } 

    if (icecon > 0 && obs[i] > critval) {
      prop_gtice += 1;
    }
    else if (icecon == 0 && obs[i] > critval) {
      fals_gtice += 1;
    }

// less than
    if (icecon == 0 && obs[i] < critval) {
      prop_ltocean += 1;
    }
    else if (icecon != 0 && obs[i] < critval) {
      fals_ltocean += 1;
    } 
    if (icecon > 0 && obs[i] < critval) {
      prop_ltice += 1;
    }
    else if (icecon == 0 && obs[i] < critval) {
      fals_ltice += 1;
    }

  }

//scores:
//  0 is pod for ocean > critval
//  1 is far for ocean > critval
//  2 is pod for ice > critval
//  3 is far for ice > critval
//  4 is pod for ocean < critval
//  5 is far for ocean < critval
//  6 is pod for ice < critval
//  7 is far for ice < critval
   scores[0] = (float) prop_gtocean / (float) totocean;
   scores[1] = (float) fals_gtocean / (float) (prop_gtocean+fals_gtocean);
   // ice pod/far
   //scores[2] = (float) prop_gtice   / (float) totice;
   //scores[3] = (float) fals_gtice   / (float) (prop_gtice+fals_gtice);
   scores[2] = (float) prop_gtocean   / (float) (fals_gtocean+1); // add 1 to avoid /0
   scores[3] = (float) fals_gtice   / (float) (prop_gtice+fals_gtice);
   scores[4] = (float) prop_ltocean / (float) totocean;
   scores[5] = (float) fals_ltocean / (float) (prop_ltocean+fals_ltocean);
   // ice pod/far
   //scores[6] = (float) prop_ltice   / (float) totice;
   //scores[7] = (float) fals_ltice   / (float) (prop_ltice+fals_ltice);
   scores[6] = (float) prop_ltocean   / (float) (fals_ltocean+1); // add 1 to avoid /0
   scores[7] = (float) fals_ltice   / (float) (prop_ltice+fals_ltice);
   //printf("%d %d %d  %d %d %d  %d %d %d  %d %d %d\n",
   //        prop_gtocean, fals_gtocean, totocean,
   //        prop_gtice, fals_gtice, totice, 
   //        prop_ltocean, fals_ltocean, totocean, 
   //        prop_ltice, fals_ltice, totice);


  return;
}
void ranger(mvector<float> &vals, float &top, float &bottom, float &step) {
  top = vals.maximum();
  bottom = vals.minimum();
  step = (top - bottom) / 256;

  return;
}
