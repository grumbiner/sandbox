#include <stdio.h>
#include <math.h>

//#define MOON 27.32
//#define MOON 29.53
//#define MOON 27.55
#define MOON 13.66
int main(void) {
// compute a series of monthly averages for a time series that is
//   fundamentally varying at 27.32 days (lunar month/earth-moon barycenter)
  int i, j;
  int m, md, k, l, jd;
  float sum;
  float series[40*365+40/4];
  int days[12];
  days[0] = 31;
  days[1] = 28;
  days[2] = 31;
  days[3] = 30;
  days[4] = 31;
  days[5] = 30;
  days[6] = 31;
  days[7] = 31;
  days[8] = 30;
  days[9] = 31;
  days[10] = 30;
  days[11] = 31;

  for (i = 0; i < 365.25*40; i++) {
    series[i] = cos(i/MOON*2.*3.141592654);
    //printf("%d %f\n",i,cos(i/MOON*2.*3.141592654) );
  }

  j = 0;
  m = 0;
  md = 0;
  sum = 0;
  for (i = 1; i < 365.25*40 + 1; i++) {
    k = i%(365*4+1);  //leap year cycle number
    l = k/4;           // year within cycle
    if (l == 0) {
      days[1] = 29;
    }
    else {
      days[1] = 28;
    }

    md += 1;
    if (md <= days[m]) {
      sum += series[i];
    }
    else {
      sum /= days[m];
      printf("%d %f\n",m, sum);
      sum = 0;
      m += 1; m = m % 12;
      md = 1;
      sum += series[i];
    }
  }

  return 0;
}
