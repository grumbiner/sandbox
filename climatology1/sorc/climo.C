#include "ncepgrids.h"

#define NYEAR 32

void probability(GRIDTYPE<float> *x, int nyear, GRIDTYPE<float> &prob, GRIDTYPE<float> &cond) ;
void shaninfo(GRIDTYPE<float> *x, int nyear, GRIDTYPE<float> &shan) ;
void tradition(GRIDTYPE<float> *x, int nyear, GRIDTYPE<float> &avg,
               GRIDTYPE<float> &cmax,     GRIDTYPE<float> &cmin, 
               GRIDTYPE<float> &var,      GRIDTYPE<float> &skew, 
               GRIDTYPE<float> &kurtosis, GRIDTYPE<float> &prob) ;

void extract(GRIDTYPE<float> *x, int nyear, mvector<float> &y, ijpt &loc) ;
void cleaner(GRIDTYPE<float> *x, int nyear);
float information(mvector<int> &x, int cats) ;

int main(int argc, char *argv[]) {
  FILE *fout_trad, *fout_conditional, *fout_shannon;
  FILE *fin;
  GRIDTYPE<float> obs[NYEAR];
  GRIDTYPE<float> avg, cmax, cmin, var, skew, kurtosis;
  GRIDTYPE<float> prob, condavg, shannon;
  int i;
  ijpt loc;

// Open files, get data:
  fout_trad        = fopen(argv[1], "w");
  fout_conditional = fopen(argv[2], "w");
  fout_shannon     = fopen(argv[3], "w");

  for (i = 0; i < NYEAR; i++) {
    fin = fopen(argv[i+4], "r");
    obs[i].binin(fin);
    fclose(fin);
  }

  avg.set((float) 0.);
  prob.set((float) 0.);
  condavg.set((float) 0.);
  shannon.set((float) 0.);

  cleaner(obs, NYEAR);

// prob
  probability(obs, NYEAR, prob, condavg);
  prob.binout(fout_conditional);
  condavg.binout(fout_conditional);
  fclose(fout_conditional);

// Shannon Information
  shaninfo(obs, NYEAR, shannon);
  shannon.binout(fout_shannon);
  fclose(fout_shannon);

// Traditional statistics
  tradition(obs, NYEAR, avg, cmax, cmin, var, skew, kurtosis, prob);
  avg.binout(fout_trad);
  cmax.binout(fout_trad);
  cmin.binout(fout_trad);
  var.binout(fout_trad);
  skew.binout(fout_trad);
  kurtosis.binout(fout_trad);
  fclose(fout_trad);

// ASCII output on points which ever have ice:
  latpt ll;
  for (loc.j = 0; loc.j < avg.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < avg.xpoints(); loc.i++) {
    if (prob[loc] != 0.) {
      ll = avg.locate(loc);
      printf("%3d %3d  %6.2f %6.2f  ",loc.i, loc.j, ll.lat, ll.lon);
      printf("%5.3f  %5.3f %5.3f %5.3f  %7.5f  ",shannon[loc], prob[loc], condavg[loc], avg[loc], avg[loc] / condavg[loc]);
      printf("%5.3f %5.3f  %5.3f %6.2f %7.2f \n",cmax[loc], cmin[loc], var[loc], skew[loc], kurtosis[loc]);

    }
  }
  }

  return 0;
}
void extract(GRIDTYPE<float> *x, int nyear, mvector<float> &y, ijpt &loc) {
  for (int i = 0; i < nyear; i++) {
    y[i] = x[i][loc];
  }
  return;
}

void probability(GRIDTYPE<float> *x, int nyear, GRIDTYPE<float> &prob, GRIDTYPE<float> &cond) {
  ijpt loc;
  mvector<float> y(nyear);
  float count;
  double sum;

  for (loc.j = 0; loc.j < x[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x[0].xpoints(); loc.i++) {
    extract(x, nyear, y, loc);
    count = 0;
    sum = 0.0;
    for (int i = 0; i < nyear; i++) {
      if (y[i] > 0 && y[i] < 1.28) {
        count += 1;
        sum   += y[i];
      }
    }
    prob[loc] = count / (float) nyear;
    cond[loc] = sum / (float) count;
  }
  }
  
  return ;
}
void shaninfo(GRIDTYPE<float> *x, int nyear, GRIDTYPE<float> &shan) {
  ijpt loc;
  mvector<float> y(nyear);
  mvector<int> z(nyear);

  for (loc.j = 0; loc.j < x[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x[0].xpoints(); loc.i++) {
    extract(x, nyear, y, loc);
    for (int i = 0; i < nyear; i++) {
      if (y[i] > 1.0 && y[i] < 1.28) {
        y[i] = 1.;
      }
      if (y[i] > 1.28) {
        y[i] = 0.;
      }
      z[i] = (int) (100.*y[i] + 0.5);
      z[i] /= 5;
    }
    shan[loc] = information(z, 101);
  }
  }
 
  return ;
}
float information(mvector<int> &x, int cats) {
  int i;
  mvector<int> found(cats);
  float p, info;

  found = 0;
  for (i = 0; i < x.xpoints(); i++) {
    found[ x[i] ] += 1;
  }

  info = 0.;
  for (i = 0; i < cats; i++) {
    if (found[i] != 0) {
      p = (float) found[i] / (float) x.xpoints() ;
      info -=  p * log10(p) / log10(2.);
    }
  }
  return info;
}
void cleaner(GRIDTYPE<float> *x, int nyear) {
  int i;
  ijpt loc;
  mvector<float> y(nyear);

  for (loc.j = 0; loc.j < x[0].ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x[0].xpoints(); loc.i++) {

    for (i = 0; i < nyear; i++) {
      y[i] = x[i][loc];
      if (y[i] > 1.0 && y[i] < 1.28) {
        y[i] = 1.;
      }
      if (y[i] > 1.28) {
        y[i] = 0;
      }
      x[i][loc] = y[i];
    }

  }
  }

  return;
}
void tradition(GRIDTYPE<float> *x, int nyear, GRIDTYPE<float> &avg,
               GRIDTYPE<float> &cmax,     GRIDTYPE<float> &cmin, 
               GRIDTYPE<float> &var,      GRIDTYPE<float> &skew, 
               GRIDTYPE<float> &kurtosis, GRIDTYPE<float> &prob) {
  int i;
  double sx, sx2, sx3, sx4;
  double var2, var3, var4;
  float tmax, tmin;
  mvector<float> y(nyear);
  ijpt loc;

  for (loc.j = 0; loc.j < avg.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < avg.xpoints(); loc.i++) {
    if (prob[loc] > 0.) {
      extract(x, nyear, y, loc);
      sx = 0.; sx2 = 0.; sx3 = 0.; sx4 = 0.;
      tmax = 0.; tmin = 1.;
      for (i = 0; i < nyear; i++) {
        tmax = max(y[i], tmax);
        tmin = min(y[i], tmin);
        sx  += y[i];
        sx2 += y[i]*y[i];
        sx3 += y[i]*y[i]*y[i];
        sx4 += y[i]*y[i]*y[i]*y[i];
      }
      cmax[loc] = tmax;
      cmin[loc] = tmin;

      avg[loc] = sx / nyear;

      var2 = (sx2 - nyear*avg[loc]*avg[loc])/nyear;
      var3 = (sx3 - 3.*avg[loc]*sx2 + 3.*avg[loc]*avg[loc]*sx - 
                 nyear*avg[loc]*avg[loc]*avg[loc])/nyear;
      var4 = (sx4 - 4.*avg[loc]*sx3 + 6.*avg[loc]*avg[loc]*sx2 - 
                    4.*avg[loc]*avg[loc]*avg[loc]*sx + 
                 nyear*avg[loc]*avg[loc]*avg[loc]*avg[loc]) / nyear;

      var[loc]      = sqrt(var2);
      skew[loc]     = var3 / pow(var2, 1.5);
      kurtosis[loc] = (var4 / var2 / var2) - 3.0;
   }
  }
  }

  return;
}
