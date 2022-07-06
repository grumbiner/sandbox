#include "ncepgrids.h"

// Compute histogram-related information from an input SST field
//   from the climatology
float area_crit(global_quarter<int> &x, float resolution, float value) ;
void set_constant(global_quarter<float> &x, float flag, float value) ;
void evaluate(global_quarter<float> &x, global_quarter<float> &y, float flag) ;
double shannon(mvector<float> &x) ;

int main(int argc, char *argv[]) {
// Data to read in:
  global_quarter<float> sst, tmp;
  global_quarter<int> y;

  mvector<float> hist(4000);
  mvector<float> lonavg(sst.xpoints()), latavg(sst.ypoints());
  mvector<float> lonarea(sst.xpoints()), latarea(sst.ypoints());
  float resolution = 1.0, flag = -999, tc, hurr_temp = 2600.;
  float area, ocean, hurr = 0., cell;
  int i;
  ijpt loc;
  latpt ll;
  double suma = 0.0;

// Local and flag values
  FILE *fin;

// Get input data
  fin = fopen(argv[1],"r");
  sst.binin(fin);
  fclose(fin);

// preprocess back to bins of 0.01 degrees -- enough to make comparisons 
//   on reals reliable
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    y[loc] = round(sst[loc]/resolution);
  }
  }


// temperature histogram, binwise and cumulative
   printf("climo max min avg %f %f %f\n",sst.gridmax(flag), sst.gridmin(flag), sst.average(flag) );
   printf("y     max min avg %f %f %f\n",(float) y.gridmax(flag), (float) y.gridmin(flag), (float) y.average(flag) );
   
   hist = 0.0;
   for (i = 700; i < hist.xpoints(); i++) {
      tc = i*resolution + flag;
      area = area_crit(y, resolution, tc);
      hist[i] = area;
      suma += area;
      if (tc >= hurr_temp) hurr += area;
      printf("%7.2f  %7.2f %10.6f\n",tc/100., area/1e9, suma/1e12);
   }

   hist /= suma;
   printf("shannon information by area histogram %f\n",shannon(hist) );
   fflush(stdout);
   
// global mean temperature, latitude mean temperatures, longitude mean temperatures
  float mean = sst.integrate(flag);
  double sum2 = 0., sum3 = 0., sum4 = 0.;
  tmp = sst;
  lonavg = 0.;
  latavg = 0.;
  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
    if (tmp[loc] != flag) {
      cell = tmp.cellarea(loc);
      sum2 += cell*sst[loc]*sst[loc];
      sum3 += cell*sst[loc]*sst[loc]*sst[loc];
      sum4 += cell*sst[loc]*sst[loc]*sst[loc]*sst[loc];
      tmp[loc] = 1.0;
      latavg[loc.j]  += cell*sst[loc];
      latarea[loc.j] += cell;
      lonavg[loc.i]  += cell*sst[loc];
      lonarea[loc.i] += cell;
    }
  }
  }
  ocean = tmp.integrate(flag);
  mean /= ocean;
  sum2 /= ocean;
  sum3 /= ocean;
  sum4 /= ocean;
  printf("Global mean temperature = %f area = %f, ms = %f m3 = %f m4 = %f\n",mean, ocean/1e12, sqrt(sum2), pow(sum3,1./3.), pow(sum4, 1./4.) );
  printf("Hurricane Area = %f\n",hurr / 1.e12);

  float median = 2161;
  set_constant(tmp, flag, mean);
  evaluate(tmp, sst, flag);
  set_constant(tmp, flag, median);
  evaluate(tmp, sst, flag);

  median = sqrt(sum2);
  set_constant(tmp, flag, median);
  evaluate(tmp, sst, flag);

  median = pow(sum3, 1./3.);
  set_constant(tmp, flag, median);
  evaluate(tmp, sst, flag);

  median = pow(sum4, 1./4.);
  set_constant(tmp, flag, median);
  evaluate(tmp, sst, flag);

// Now look at lat, lon averages:
  fflush(stdout);
  for (i = 0; i < latavg.xpoints(); i++) {
    loc.j = i;
    loc.i = 1;
    ll = tmp.locate(loc);
    latavg[i] /= latarea[i];
    printf("lat %f temperature %f area %f\n",ll.lat, latavg[i], latarea[i]/1.e9);
    fflush(stdout);
    for(loc.i = 0; loc.i < tmp.xpoints(); loc.i++) {
      if (sst[loc] != flag) tmp[loc] = latavg[i];
    }
  }
  printf("latavg ");
  evaluate(tmp, sst, flag);

  for (i = 0; i < lonavg.xpoints(); i++) {
    loc.i = i;
    loc.j = 1;
    ll = tmp.locate(loc);
    lonavg[i] /= lonarea[i];
    printf("lon %f temperature %f area %f\n",ll.lon, lonavg[i], lonarea[i]/1.e9);
    loc.i = i;
    for (loc.j = 0; loc.j < tmp.ypoints(); loc.j++) {
      if (sst[loc] != flag) tmp[loc] = lonavg[i];
    }
  }
  printf("lonavg ");
  evaluate(tmp, sst, flag);


  return 0;
}
// find the area that has a value near 'value' (i.e. within 'resolution' of it)
float area_crit(global_quarter<int> &x, float resolution, float value) {
  float sum;
  global_quarter<int> y;
  ijpt loc;
  int crit = round(value/resolution);

  //debug printf("crit = %f\n",crit);
  //y = x;
  y.set(0);
//  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
//  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
  for (int i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] == crit) {y[i] = 1;}
  }
//  }
//  }
  sum = y.integrate();

  return sum;
}
void set_constant(global_quarter<float> &x, float flag, float value) {
  int i;
  for (i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] != flag) x[i] = value;
  }
  return;
}
void evaluate(global_quarter<float> &x, global_quarter<float> &y, float flag) {
  double sum = 0, sum2 = 0., sum3 = 0., sum4 = 0.;
  float mean, rms, skew, kurt;
  float scale = 100.;
  double area = 0, cell, delta;
  int npts = x.xpoints()*x.ypoints();
  ijpt loc;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] != flag && y[loc] != flag) {
      cell = x.cellarea(loc);
      delta = (x[loc] - y[loc])/scale;

      area += cell;

      sum  += cell*delta;
      sum2 += cell*delta*delta;
      sum3 += cell*delta*delta*delta;
      sum4 += cell*delta*delta*delta*delta;      
    }
  }
  }

  mean = sum / area;
  rms  = sqrt(sum2 / area);
  skew = (sum3/area) / pow( (double) sum2/area, (double) 1.5);
  kurt = (sum4/area) / (sum2/area*sum2/area) - 3;

  printf("moments %f %f %f %f\n",mean, rms, skew, kurt);

  return;
}
double shannon(mvector<float> &x) {
  int i;
  double s = 0;
  for (i = 0; i < x.xpoints(); i++) {
     if (x[i] != 0) {
       s -= x[i] * log(x[i]); 
     }
  }
  s /= log(2.);
  return s;
}
