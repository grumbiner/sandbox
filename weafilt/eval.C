#include "mvector.h"
#include "genes.h"

#define MAXPTS (6000*1000)
float fn(mvector<float> &a, mvector<float> &b, mvector<float> *tb, int count);
float scorer(mvector<float> &a, mvector<float> &b, mvector<float> &icec, mvector<float> *tb, int counts) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  mvector<float> icec(MAXPTS), x[7]; // icec + 7xTb, pre-select to only be ocean
  mvector<float> lats(MAXPTS), lons(MAXPTS);
  float ticec, t1, t2, t3, t4, t5, t6, t7;
  float mask, lat, lon;
  int i, counts = 0, icepts = 0;

  for (i = 0; i < 7; i++) {
    x[i].resize(MAXPTS);
  }
  fin = fopen(argv[1],"r");
  while (!feof(fin) && counts < MAXPTS ) {
    fscanf(fin, "%f %f %f %f %f %f %f %f %f %f %f\n",&ticec, &mask, &lat, &lon, 
           &t1, &t2, &t3, &t4, &t5, &t6, &t7);
    if (mask == 0 && fabs(lat) > 20.0 &&
        t1 < 320 && t2 < 320 && t3 < 320 && t4 < 320 && t5 < 320 && t6 < 320 && t7 < 320) {
      icec[counts] = ticec;
      lats[counts] = lat;
      lons[counts] = lon;
      x[0][counts] = t1;
      x[1][counts] = t2;
      x[2][counts] = t3;
      x[3][counts] = t4;
      x[4][counts] = t5;
      x[5][counts] = t6;
      x[6][counts] = t7;

      counts += 1;
      if (ticec != 0) icepts++;
    }
  }
  counts -= 1;
  printf("found %d pts %d have ice %f\n",counts, icepts, (float) icepts / (float) counts); fflush(stdout);
    
  mvector<float> a(7), b(7);
  FILE *gout;
  gout = fopen(argv[2],"r");
  for (i = 0; i < 7; i++) {
    fscanf(gout,"%f\n",&t1);
    a[i] = t1;
  }
  for (i = 0; i < 7; i++) {
    fscanf(gout, "%f\n", &t1);
    b[i] = t1;
  } 

// Apply the filter to the data
  float tmp, crit = 0.999;
  int a11 = 0, a12 = 0, a22 = 0, a21 = 0;
  for (i = 0; i < counts; i++) {
    tmp = fn(a, b, x, i);
    if (icec[i] > 0 && tmp > crit) {
      a11 += 1;
    }
    else if (icec[i] == 0 && tmp < -crit) {
      a22 += 1;
    }
    else if (icec[i] == 0 && tmp > crit) {
      a21 += 1;
      printf("a21  %3.0f %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
             icec[i], lons[i], lats[i],
             x[0][i], x[1][i], x[2][i], x[3][i], x[4][i], x[5][i], x[6][i]);
    }
    else if (icec[i] > 0 && tmp < -crit) {
      a12 += 1;
      printf("a12  %3.0f %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
             icec[i], lons[i], lats[i],
             x[0][i], x[1][i], x[2][i], x[3][i], x[4][i], x[5][i], x[6][i]);
    }
    else {
      // undecided
      printf("uuu  %3.0f %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
             icec[i], lons[i], lats[i],
             x[0][i], x[1][i], x[2][i], x[3][i], x[4][i], x[5][i], x[6][i]);
    }
  }

  printf("%d %d %d %d  %d %f  %f\n",a11, a12, a21, a22, a11+a22+a12+a21, (float)(a11+a22)/(float) counts,
     (float) (counts - a11-a12-a21-a22) / (float) counts);

  return 0;
}

// pass in 7 tb and return the tanh(sum)
float fn(mvector<float> &a, mvector<float> &b, mvector<float> *tb, int count) {
  double tmp = 0;
  int i;
  for (i = 0; i < a.xpoints(); i++) {
    tmp += a[i] + b[i]*(tb[i])[count];
  }
//  if (tmp >  100) return 1;
//  if (tmp < -100) return -1;
  return tanh(tmp);
}
