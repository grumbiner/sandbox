#include "mvector.h"
#include "time_series.h"

#define MAXOBS 462 
int main(void) {
  FILE *fin;
  time_series<float> l1(MAXOBS), l2(MAXOBS), l3(MAXOBS), l4(MAXOBS);
  time_series<float> l5(MAXOBS), l6(MAXOBS), l7(MAXOBS);
  time_series<float> time(MAXOBS), tau(MAXOBS);
  int t1, t2, t3, t4, t5, t6, t7;
  float c1, c2, c3, c4, c5, c6, c7;
  
  int i, tdum, nyear;

  fin = fopen("bsice.dat", "r");
  for (i = 0; i < MAXOBS; i++) {
     fscanf(fin, "%3d %4d %4d %4d %4d %4d %4d %4d\n",&tdum, 
            &t1, &t2, &t3, &t4, &t5, &t6, &t7);
     tau[i] = (float) tdum;
     l1[i] = t1;
     l2[i] = t2;
     l3[i] = t3;
     l4[i] = t4;
     l5[i] = t5;
     l6[i] = t6;
     l7[i] = t7;

     //CDprintf("%d tdum = %d\n",i, tdum); fflush(stdout);
  }
  //CDl1.printer(stdout);
  nyear = 0;
  time[0] = tau[0];
  for (i = 1; i < MAXOBS; i++) {
    if (tau[i] < 355 && tau[i-1] > 355) {
      nyear += 1;
    }
    time[i] = tau[i] + 365 * nyear; 
    printf("%6.0f %6.0f\n",tau[i], time[i]);
  }
  printf("Number of years: %d\n",nyear);

  c1 = l1.autocovary(0);
  c2 = l2.autocovary(0);
  c3 = l3.autocovary(0);
  c4 = l4.autocovary(0);
  c5 = l5.autocovary(0);
  c6 = l6.autocovary(0);
  c7 = l7.autocovary(0);
  
  for (i = -MAXOBS/2; i < MAXOBS/2; i++) {
     printf("1-2 %d %f %f\n", i, l1.autocovary(i)/c1, l1.crossvary(l2, i)/sqrt(c1*c2) );
     printf("1-3 %d %f %f\n", i, l1.autocovary(i)/c1, l1.crossvary(l3, i)/sqrt(c1*c3) );
     printf("1-4 %d %f %f\n", i, l1.autocovary(i)/c1, l1.crossvary(l4, i)/sqrt(c1*c4) );
     printf("1-5 %d %f %f\n", i, l1.autocovary(i)/c1, l1.crossvary(l5, i)/sqrt(c1*c5) );
     printf("1-6 %d %f %f\n", i, l1.autocovary(i)/c1, l1.crossvary(l6, i)/sqrt(c1*c6) );
     printf("1-7 %d %f %f\n", i, l1.autocovary(i)/c1, l1.crossvary(l7, i)/sqrt(c1*c7) );
     printf("2-3 %d %f %f\n", i, l2.autocovary(i)/c2, l2.crossvary(l3, i)/sqrt(c2*c3) );
     printf("2-4 %d %f %f\n", i, l2.autocovary(i)/c2, l2.crossvary(l4, i)/sqrt(c2*c4) );
     printf("2-5 %d %f %f\n", i, l2.autocovary(i)/c2, l2.crossvary(l5, i)/sqrt(c2*c5) );
     printf("2-6 %d %f %f\n", i, l2.autocovary(i)/c2, l2.crossvary(l6, i)/sqrt(c2*c6) );
     printf("2-7 %d %f %f\n", i, l2.autocovary(i)/c2, l2.crossvary(l7, i)/sqrt(c2*c7) );
     printf("3-4 %d %f %f\n", i, l3.autocovary(i)/c3, l3.crossvary(l4, i)/sqrt(c3*c4) );
     printf("3-5 %d %f %f\n", i, l3.autocovary(i)/c3, l3.crossvary(l5, i)/sqrt(c3*c5) );
     printf("3-6 %d %f %f\n", i, l3.autocovary(i)/c3, l3.crossvary(l6, i)/sqrt(c3*c6) );
     printf("3-7 %d %f %f\n", i, l3.autocovary(i)/c3, l3.crossvary(l7, i)/sqrt(c3*c7) );
     printf("4-5 %d %f %f\n", i, l3.autocovary(i)/c3, l4.crossvary(l5, i)/sqrt(c4*c5) );
     printf("4-6 %d %f %f\n", i, l4.autocovary(i)/c4, l4.crossvary(l6, i)/sqrt(c4*c6) );
     printf("4-7 %d %f %f\n", i, l4.autocovary(i)/c4, l4.crossvary(l7, i)/sqrt(c4*c7) );
     printf("5-6 %d %f %f\n", i, l5.autocovary(i)/c5, l5.crossvary(l6, i)/sqrt(c5*c6) );
     printf("5-7 %d %f %f\n", i, l5.autocovary(i)/c5, l5.crossvary(l7, i)/sqrt(c5*c7) );
     printf("6-7 %d %f %f\n", i, l6.autocovary(i)/c6, l6.crossvary(l7, i)/sqrt(c6*c7) );
  }

  c1 = time.autocovary(0);
  for (i = 0; i < 200; i++) {
    printf("time %3d %f\n",i,time.autocovary(i)/c1);
  }
  for (i = 1; i < MAXOBS; i++) {
    if (time[i] - time[i-1] != 7) {
      printf("Not 7 days %d  %3.0f\n", i, time[i] - time[i-1] );
    }
  }

  return 0;
  
} 
