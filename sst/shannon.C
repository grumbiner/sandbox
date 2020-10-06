#include "ncepgrids.h"

void update_histogram(llgrid<mvector<short int> > &count, llgrid<float> &x, 
                          float tmin, float tmax, float prec) ;

float find_range(mvector<short int> &count, float tmin, float prec) ;

template <class T>
float shannon(mvector<T> &x) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  char fname[900];
  global_quarter<float> x, range1, range2, range3, range4;
  global_quarter<float> info1, info2, info3, info4;
  global_quarter<mvector<short int> > count1;
  global_quarter<mvector<short int> > count2;
  global_quarter<mvector<short int> > count3;
  global_quarter<mvector<short int> > count4;
  int i;
  float tmin = -25.0, tmax = 25.0;
  //float prec1 = 16.0, prec2 = 8.0, prec3 = 4.0, prec4 = 0.5;
  float prec1 = 10.0, prec2 = 5.0, prec3 = 1.0, prec4 = 0.25;
  int nc1, nc2, nc3, nc4;
  float si1, si2, si3, si4;
  ijpt loc;
  latpt ll;

  nc1 = 1 + (short int) nearbyint( (tmax - tmin - prec1/2)/prec1);
  nc2 = 1 + (short int) nearbyint( (tmax - tmin - prec2/2)/prec2);
  nc3 = 1 + (short int) nearbyint( (tmax - tmin - prec3/2)/prec3);
  nc4 = 1 + (short int) nearbyint( (tmax - tmin - prec4/2)/prec4);
  for (i = 0; i < x.xpoints()*x.ypoints() ; i++) {
    count1[i].resize(nc1);
    count1[i] = 0;
    count2[i].resize(nc2);
    count2[i] = 0;
    count3[i].resize(nc3);
    count3[i] = 0;
    count4[i].resize(nc4);
    count4[i] = 0;
  }

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    x.binin(fin);
    x /= 100.0; // rescale out of centidegrees
    printf("%d max %f min %f\n",i,x.gridmax(), x.gridmin() );
    fflush(stdout);
    fclose(fin);
    update_histogram(count1, x, tmin, tmax, prec1);
    update_histogram(count2, x, tmin, tmax, prec2);
    update_histogram(count3, x, tmin, tmax, prec3);
    update_histogram(count4, x, tmin, tmax, prec4);
  }

  info1.set((float) 0.0);
  info2.set((float) 0.0);
  info3.set((float) 0.0);
  info4.set((float) 0.0);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    si1 = shannon(count1[loc]); 
    si2 = shannon(count2[loc]); 
    si3 = shannon(count3[loc]); 
    si4 = shannon(count4[loc]); 
    if (si1 != 0) {
      info1[loc] = si1;
      range1[loc] = find_range(count1[loc], tmin, prec1);
    }
    if (si2 != 0) {
      info2[loc] = si2;
      range2[loc] = find_range(count2[loc], tmin, prec2);
    }
    if (si3 != 0) {
      info3[loc] = si3;
      range3[loc] = find_range(count3[loc], tmin, prec3);
    }
    if (si4 != 0) {
      info4[loc] = si4;
      range4[loc] = find_range(count4[loc], tmin, prec4);
    }
  }
  }

  sprintf(fname,"info_%04.2f",prec1);
  fout = fopen(fname,"w");
  info1.binout(fout);
  range1.binout(fout);
  fclose(fout);

  sprintf(fname,"info_%04.2f",prec2);
  fout = fopen(fname,"w");
  info2.binout(fout);
  range2.binout(fout);
  fclose(fout);

  sprintf(fname,"info_%04.2f",prec3);
  fout = fopen(fname,"w");
  info3.binout(fout);
  range3.binout(fout);
  fclose(fout);

  sprintf(fname,"info_%04.2f",prec4);
  fout = fopen(fname,"w");
  info4.binout(fout);
  range4.binout(fout);
  fclose(fout);

  return 0;
}
void update_histogram(llgrid<mvector<short int> > &count, 
                      llgrid<float> &x, float tmin, float tmax, float prec) {
  int i;
  for (i = 0; i < x.xpoints()*x.ypoints(); i++) {
    count[i][ (short int) nearbyint( (x[i]-tmin-prec/2)/prec) ] += 1;  
  }
  return;
}
template <class T>
float shannon(mvector<T> &x) {
  int i, count = 0;
  float p, info = 0.0;
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] != 0) { 
      count += x[i];
    }
  }
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] != 0) { 
      p = (float) x[i] / (float) count;
      info  -= p    * log10(p);
      // note -= because SI is -sum(p*logp)
     }
  }
  return info / log10(2.);
}
float find_range(mvector<short int> &count, float tmin, float prec) {
  int imax = 0, imin=count.xpoints();
  int i = 0;
  float tmp;

  i = 0;
  while (count[i] == 0) {
    i++;
  }
  imin = i;
  //printf("minrange %d %d  %d %d ",i, count[i], imin, count[imin]);

  i = count.xpoints() - 1;
  while (count[i] == 0) {
    i--;
  }
  imax = i;
  //printf("maxrange %d %d  %d %d\n",i, count[i], imax, count[imax]);

  tmp = (float) (imax-imin+1);
  //printf("%d %d  %f %f\n",imin, imax, tmp, log(tmp)/log(2.));
  if (tmp > 0) {
    return log(tmp)/log(2.);  
  }
  else {
    return 0.;
  }

}
