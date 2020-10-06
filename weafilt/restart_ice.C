#include "ncepgrids.h"

// arguments are: icegrid, landgrid, Tb ascii file

// Look for an ice filter (yes if ice), will start from pre-filtered
//   for ocean

#define NOBS 5520128
  typedef struct {
    float observed[9], icec;
    unsigned char land;
  } obsout;

void scorer(metricgrid<float> &icec, metricgrid<unsigned char> &land, 
            mvector<ijpt> &locs, mvector<float> &obs, mvector<float> &scores, 
            float &critval) ;
void ranger(mvector<float> &vals, float &top, float &bottom, float &step) ;

int main(int argc, char *argv[]) {
  FILE *fin, fout;
  mvector<obsout> obs(NOBS); // lat, lon, 7 Tb, ice, land
  mvector<float> scores(2*4), trial;
  mvector<ijpt> ilocs(NOBS);
  global_12th<float> gice;
  global_12th<unsigned char> gland;


  int i, nobs;
  obsout x;
  float top, bottom, step, critval;
  latpt tloc;


  fin = fopen(argv[1],"r");
  i = 0;
  while (!feof(fin) ) {
    if ( (i % 1000000) == 0 ) {
      printf("i = %d\n",i); fflush(stdout);
    }

    fread(&obs[i], sizeof(obsout), 1, fin);

    //if (i < 10) {
    //printf("%f %f %f %f %f %f %f %f %f  %f %3d\n",
    //  obs[i].observed[0], obs[i].observed[1],
    //  obs[i].observed[2], obs[i].observed[3], obs[i].observed[4],
    //  obs[i].observed[5], obs[i].observed[6], obs[i].observed[7], obs[i].observed[8],
    //  obs[i].icec, obs[i].land);
    //} 

    tloc.lat = obs[i].observed[0];
    tloc.lon = obs[i].observed[1];
    ilocs[i] = gice.locate(tloc);

    gice[ilocs[i]] = obs[i].icec;
    gland[ilocs[i]] = obs[i].land;

    i += 1;
  }
  fclose(fin);
  nobs = i - 1;

// work on the critical point tests:
  printf("nobs = %d\n",nobs);
  trial.resize(nobs);

  // t19v
  for (i = 0; i < nobs; i++) {
    trial[i] = obs[i].observed[2]; 
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
    trial[i] = obs[i].observed[3]; 
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
    trial[i] = obs[i].observed[5]; 
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
    trial[i] = obs[i].observed[6]; 
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
    trial[i] = obs[i].observed[7];
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
    trial[i] = obs[i].observed[8]; 
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
      trial[l] = (obs[l].observed[i] - obs[l].observed[j]) / (obs[l].observed[i] + obs[l].observed[j]);  
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

   scores[0] = (float) prop_gtocean / (float) totocean;
   scores[1] = (float) fals_gtocean / (float) (prop_gtocean+fals_gtocean);
   scores[2] = (float) prop_gtocean / (float) (fals_gtocean+1); // add 1 to avoid /0
   scores[3] = 0.0;
   scores[4] = (float) prop_ltocean / (float) totocean;
   scores[5] = (float) fals_ltocean / (float) (prop_ltocean+fals_ltocean);
   scores[6] = (float) prop_ltocean / (float) (fals_ltocean+1); // add 1 to avoid /0
   scores[7] = 0.0;

  return;
}
void ranger(mvector<float> &vals, float &top, float &bottom, float &step) {
  top = vals.maximum();
  bottom = vals.minimum();
  step = (top - bottom) / 256;

  return;
}
