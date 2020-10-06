#include "ncepgrids.h"

typedef struct {
  latpt ll;
  ijpt loc;
  float climo, rtg, delta;
  unsigned char mask;
  float dist;
} sst_check;

int main(int argc, char *argv[]) {
  global_12th<float> distance;
  global_12th<unsigned char> mask;
  float lon, lat, gridi, gridj, climo, rtg, delta;
  ijpt loc;
  latpt ll;
  sst_check tmpob;
  mvector<sst_check> sstob(250000), finish;
  FILE *fin;
  mvector<int> histogram(50), sum(50);
  float distance_toler = 50; //km
  float temp_toler;
  int i, nr, nobs;

  fin = fopen("seaice_alldist.bin","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open the distance grid\n");
    return 1;
  }
  distance.binin(fin);
  distance /= 1000.; // convert to km
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open the land mask\n");
    return 1;
  }
  mask.binin(fin);
  fclose(fin);

  i = 0;
  nr = 0;
  histogram = 0;
  temp_toler = atof(argv[2]);
  if (argc == 4) distance_toler = atof(argv[3]);

// do the basic read in and simple thinning
  fin = fopen(argv[1],"r");
  while (!feof(fin)) {
    fscanf(fin, "%f %f %f %f %f %f %f\n",&lon, &lat, &gridi, &gridj, &climo, 
                &rtg, &delta);
    nr += 1;
    if (fabs(delta) < temp_toler) continue; // do this to ensure that big deltas are the type points
    loc.i = gridi; loc.j = gridj;
    tmpob.ll.lat = lat;
    tmpob.ll.lon = lon;
    tmpob.loc.i  = gridi;
    tmpob.loc.j  = gridj;
    tmpob.climo  = climo;
    tmpob.rtg    = rtg;
    tmpob.delta  = delta;
    if (i > 0 ) {
      if (ARCDIS(tmpob.ll.lon, tmpob.ll.lat, sstob[i-1].ll.lon, sstob[i-1].ll.lat) > distance_toler) { 
        sstob[i] = tmpob;
        i++;
        //printf("%8.4f %8.4f  %4d %4d  %5.2f %5.2f %6.2f  %3d %7.2f\n",
        //        lon, lat, (int)gridi, (int)gridj, climo, rtg, delta, 
        //        mask[loc], distance[loc]);
      }
    }
    else {
      sstob[i] = tmpob;
      i++;
    }
    if (fabs(delta) < histogram.xpoints()) {
      histogram[fabs(delta)] += 1;
    }
    else {
      printf("extraordinary delta, %8.4f %8.4f  %4d %4d  %5.2f %5.2f %6.2f  %3d %7.2f\n",
                lon, lat, (int)gridi, (int)gridj, climo, rtg, delta,
                mask[loc], distance[loc]);
    }

  }
  nobs = i;
  //printf("nobs, nr = %d %d\n",nobs, nr);

  sum[histogram.xpoints() -1] = 0;
  for (i = histogram.xpoints() - 2; i >= 0; i--) {
    sum[i] = sum[i+1] + histogram[i];
  }
 
//// Print out histogram:
//  for (i = 0; i < histogram.xpoints(); i++) {
//    if (histogram[i] != 0) printf("%2d %6d %6d\n",i,histogram[i], sum[i]);
//  }

// Now do the n^2 task of ensuring that all observations are > 50 km apart 
//   (not just > 50 km from previous in list
  bool farther = true;
  int j, k, nfin = 0;
  finish.resize(nobs);
  finish[0] = sstob[0];
  i = 1;
  for (j = 1; j < nobs; j++) {
    farther = true;
    for (k = 0; k < i; k++) {
      farther = farther && (ARCDIS(finish[k].ll.lon, finish[k].ll.lat, 
                                   sstob[j].ll.lon, sstob[j].ll.lat) > distance_toler);
      if (!farther) break;
    }
    if (farther) {
      finish[i] = sstob[j];
      i += 1;
    }
  }
  nfin = i;
  //printf("nfin %d\n",nfin);
  
  for (i = 0; i < nfin; i++) {
    loc = finish[i].loc;
    printf("%7.3f %7.3f  %5.2f %5.2f %6.2f     %6.2f\n",
            finish[i].ll.lon, finish[i].ll.lat, 
            finish[i].climo, finish[i].rtg, finish[i].delta, distance[finish[i].loc] );
  }



  return 0;
}
// 34.0417  47.4583   408  510  12.81 41.99  29.18
