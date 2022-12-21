#include "ncepgrids.h"

// arguments are: icegrid, landgrid, Tb ascii file

// Restart the search for a good ocean/weather/ice filter

// This prog is utility to display values after a filter is
//   tripped, fill in the filter by hand

#define NOBS 5520128

void unary(int &chan, float &critval, float &test, mvector<mvector<float> > &obs, 
                 mvector<float> &trial, metricgrid<float> &icec, metricgrid<unsigned char> &land) ;
void binary(int &chan1, int &chan2, float &critval, float &test, 
            mvector<mvector<float> > &obs, mvector<float> &trial, 
            metricgrid<float> &icec, metricgrid<unsigned char> &land) ;

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

  int chan, chan1, chan2;
  float test;

// Get data ////////////////////////////////////////////////
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
////////////////////////////////////////////////

//Unary filters:
//2 < 179.319794
//3 > 272.704865
//5 < 187.976028
//6 > 277.056366
//7 > 289.068237
//8 > 266.875610

  chan = 2; critval = 179.319794; test = -1; // <1 -> lt test
  unary(chan, critval, test, obs, trial, gice, gland);
  chan = 3; critval =  272.704865; test = +1;
  unary(chan, critval, test, obs, trial, gice, gland);
  chan = 5; critval =  187.976028; test = -1;
  unary(chan, critval, test, obs, trial, gice, gland);
  chan = 6; critval =  277.056366; test = +1;
  unary(chan, critval, test, obs, trial, gice, gland);
  chan = 7; critval =  289.068237; test = +1;
  unary(chan, critval, test, obs, trial, gice, gland);
  chan = 8; critval =  266.875610; test = +1;
  unary(chan, critval, test, obs, trial, gice, gland);


// Binary (delta/mean) filters:
//2,3  9 < 0.005930
//2,4 10 < -0.050329
//2,5 11 < -0.069213
//2,7 13 < -0.135559
//2,8 14 < -0.084642
//3,4 15 > -0.010433 
//3,8 19 < -0.273411
//4,6 21 > 0.183716
//5,6 24 < 0.006615
//5,7 25 < -0.084970
//5,8 26 < -0.052001
//6,8 28 < -0.188299
//7,8 29 < 0.001320
  chan1 = 2; chan2 = 3; critval = 0.005930; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 2; chan2 = 4; critval = -0.050329; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 2; chan2 = 5; critval = -0.069213; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 2; chan2 = 7; critval = -0.135559; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 2; chan2 = 8; critval = -0.084642; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 3; chan2 = 4; critval = -0.010433; test = +1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 3; chan2 = 8; critval = -0.273411; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 4; chan2 = 6; critval =  0.183716; test = +1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 5; chan2 = 6; critval =  0.006615; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 5; chan2 = 7; critval = -0.084970; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 5; chan2 = 8; critval = -0.052001; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 6; chan2 = 8; critval = -0.188299; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);
  chan1 = 7; chan2 = 8; critval =  0.001320; test = -1;
  binary(chan1, chan2, critval, test, obs, trial, gice, gland);

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

void unary(int &chan, float &critval, float &test, 
           mvector<mvector<float> > &obs, mvector<float> &trial,
           metricgrid<float> &icec, metricgrid<unsigned char> &land) {
  int i, j, nobs = trial.xpoints();
  FILE *filt, *unfilt;
  ijpt loc;
  latpt ll;
  char fname[80];
  typedef struct {
    float observed[9], icec;
    unsigned char land;
  } obsout;
  obsout x;

  sprintf(fname,"unary_filt_%02d",chan);
  filt = fopen(fname, "w");

  sprintf(fname,"unary_unfilt_%02d",chan);
  unfilt = fopen(fname, "w");

  if (test < 0) {
    for (i = 0; i < nobs; i++) {
      trial[i] = obs[i][chan];
      for (j = 0; j < 9; j++) {
        x.observed[j] = obs[i][j];
      }
      ll.lat = obs[i][0];
      ll.lon = obs[i][1];
      loc = icec.locate(ll);
      x.icec = icec[loc];
      x.land = land[loc];
      if (trial[i] < critval) {
        // write out as filtered
        fwrite(&x, sizeof(x), 1, filt);
        printf("filtered %6.2f %7.2f chan %2d\n",ll.lat, ll.lon, chan);
      }
      else {
        // write out as unfiltered
        fwrite(&x, sizeof(x), 1, unfilt);
        printf("unfiltered %6.2f %7.2f chan %2d\n",ll.lat, ll.lon, chan);
      }
    }
  }
  else {
    for (i = 0; i < nobs; i++) {
      trial[i] = obs[i][chan];
      for (j = 0; j < 9; j++) {
        x.observed[j] = obs[i][j];
      }
      ll.lat = obs[i][0];
      ll.lon = obs[i][1];
      loc = icec.locate(ll);
      x.icec = icec[loc];
      x.land = land[loc];
      if (trial[i] > critval) {
        // write out as filtered
        fwrite(&x, sizeof(x), 1, filt);
        printf("filtered %6.2f %7.2f chan %2d\n",ll.lat, ll.lon, chan);
      }
      else {
        // write out as unfiltered
        fwrite(&x, sizeof(x), 1, unfilt);
        printf("unfiltered %6.2f %7.2f chan %2d\n",ll.lat, ll.lon, chan);
      }
    }
  }

  fclose(filt);
  fclose(unfilt);
  return;
}
void binary(int &chan1, int &chan2, float &critval, float &test, 
           mvector<mvector<float> > &obs, mvector<float> &trial,
           metricgrid<float> &icec, metricgrid<unsigned char> &land) {
  int i, j, nobs = trial.xpoints();
  FILE *filt, *unfilt;
  ijpt loc;
  latpt ll;
  char fname[80];
  typedef struct {
    float observed[9], icec;
    unsigned char land;
  } obsout;
  obsout x;

  sprintf(fname,"binary_filt_%02d%02d",chan1, chan2);
  filt = fopen(fname, "w");

  sprintf(fname,"binary_unfilt_%02d%02d",chan1, chan2);
  unfilt = fopen(fname, "w");

  if (test < 0) {
    for (i = 0; i < nobs; i++) {
      trial[i] = (obs[i][chan1] - obs[i][chan2])/(obs[i][chan1]+obs[i][chan2]);
      for (j = 0; j < 9; j++) {
        x.observed[j] = obs[i][j];
      }
      ll.lat = obs[i][0];
      ll.lon = obs[i][1];
      loc = icec.locate(ll);
      x.icec = icec[loc];
      x.land = land[loc];
      if (trial[i] < critval) {
        // write out as filtered
        fwrite(&x, sizeof(x), 1, filt);
        printf("filtered %6.2f %7.2f chans %2d %2d\n",ll.lat, ll.lon, chan1, chan2);
      }
      else {
        // write out as unfiltered
        fwrite(&x, sizeof(x), 1, unfilt);
        printf("unfiltered %6.2f %7.2f chans %2d %2d\n",ll.lat, ll.lon, chan1, chan2);
      }
    }
  }
  else {
    for (i = 0; i < nobs; i++) {
      trial[i] = (obs[i][chan1] - obs[i][chan2])/(obs[i][chan1]+obs[i][chan2]);
      for (j = 0; j < 9; j++) {
        x.observed[j] = obs[i][j];
      }
      ll.lat = obs[i][0];
      ll.lon = obs[i][1];
      loc = icec.locate(ll);
      x.icec = icec[loc];
      x.land = land[loc];
      if (trial[i] > critval) {
        // write out as filtered
        fwrite(&x, sizeof(x), 1, filt);
        printf("filtered %6.2f %7.2f chans %2d %2d\n",ll.lat, ll.lon, chan1, chan2);
      }
      else {
        // write out as unfiltered
        fwrite(&x, sizeof(x), 1, unfilt);
        printf("unfiltered %6.2f %7.2f chans %2d %2d\n",ll.lat, ll.lon, chan1, chan2);
      }
    }
  }

  fclose(filt);
  fclose(unfilt);
  return;
}
