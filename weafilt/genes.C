#include "mvector.h"
#include "genes.h"

#define MAXPTS (6000*1000)
float fn(mvector<float> &a, mvector<float> &b, mvector<float> &flag, mvector<float> *tb, int count);
float scorer(mvector<float> &a, mvector<float> &b, mvector<float> &flag, mvector<float> &icec, mvector<float> *tb, int counts) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  mvector<float> icec(MAXPTS), x[7]; // icec + 7xTb, pre-select to only be ocean
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
    
// genome:
// ai, bi applied to the 7 Tb
// ai in [-250:250] step 1 (9 bits)
// bi in [-5:5] 10 bits (.01 steps)
// f = tanh(sum(ai+bi*ti))
// f > 0.9 = ice
// f < -0.9 = not-ice
// f intermed = d.k.
// pod, far, %correct, ...
#define POPULATION 100
#define GENMAX 1000
  float best, mean = 0;
  int generation = 0, genmax = GENMAX;
  genetic_code gc(21);
  mvector<united> weights;
  mvector<mvector<int> > genome(POPULATION);
  mvector<float> scores(POPULATION);
  united f1, f2;
  int j;

  f1.fval = -250; f2.fval = 250;
  for (i = 0; i < 7; i++) {
    gc.newgene(i, 9, FLOAT_TYPE, f1, f2);
  }
  f1.fval = -5; f2.fval = 5;
  for (i = 7; i < 14; i++) {
    gc.newgene(i, 10, FLOAT_TYPE, f1, f2);
  }
  f1.ival = 0; f2.ival=1;
  for (i = 14; i < 21; i++) {
    gc.newgene(i,1,INT_TYPE, f1, f2);
  }
  weights.resize(gc.ncodes);
  for (i = 0; i < POPULATION; i++) {
    genome[i].resize(gc.code_length);
    newgenes(genome[i]);
  }
  scores = (float) 0.;
  generation = 0;

  mvector<float> a(7), b(7), flags(7);
  FILE *gout;
  gout = fopen(argv[2],"w");
  do {
    for (i = 0; i < POPULATION; i++) {
      transcriber(genome[i],weights, gc);
      for (j = 0; j < 7; j++) {
        a[j] = weights[j].fval;
        b[j] = weights[j+7].fval;
        flags[j] = weights[j+14].ival;
      }
      scores[i] = scorer(a, b, flags, icec, x, counts);
    } 
    best = scores.maximum();
    mean = scores.average();
    printf(" generation %4d stats %5.3f %5.3f\n",generation, best, mean); fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);

    if ((generation % 1) == 0) {
      fprintf(gout, "\ngeneration %4d top 15 list\n", generation);
      for (i = 0; i < 15; i++) {
        fprintf(gout, "score %f ",scores[i]);
        showgenes(gout, genome[i], gc); fflush(gout);
      }
      fprintf(gout,"\n");
    }

    grazer(genome, scores);
    generation += 1;
  } while (generation < genmax);

  printf("best, score = %f\n",scores[0]);
  transcriber(genome[0], weights, gc);
  printf("weights:\n");
  for (i = 0; i < 7; i++) {
    printf("%7.2f  %6.3f %1d\n",weights[i].fval, weights[i+7].fval, weights[i+14].ival);
  }

  return 0;
}

float scorer(mvector<float> &a, mvector<float> &b, mvector<float> &flag, mvector<float> &icec,  mvector<float> *tb, int counts) {
  int a11 = 0, a12 = 0, a22 = 0, a21 = 0, i;
  float tmp, crit = 0.999;
  for (i = 0; i < counts; i++) {
    tmp = fn(a, b, flag, tb, i);
    if (icec[i] > 0 && tmp > crit) {
      a11 += 1;
    }
    else if (icec[i] == 0 && tmp < -crit) {
      a22 += 1;
    }
    else if (icec[i] == 0 && tmp > crit) {
      a21 += 1;
    }
    else if (icec[i] > 0 && tmp < -crit) {
      a12 += 1;
    }
  }
  //printf("%d %d %d %d  %d %f\n",a11, a12, a21, a22, a11+a22+a12+a21, (float)(a11+a22)/(float) counts);

  return (float)(a11+a22-10*a21-10*a12)/(float) counts;
}
// pass in 7 tb and return the tanh(sum)
float fn(mvector<float> &a, mvector<float> &b, mvector<float> &flag, mvector<float> *tb, int count) {
  double tmp = 0;
  int i;
  for (i = 0; i < a.xpoints(); i++) {
    tmp += flag[i]*a[i] + flag[i]*b[i]*(tb[i])[count];
  }
  // tanh essentially 1 by about 20.
  if (tmp > 20) return 1;
  if (tmp < -20) return -1;
  return tanh(tmp);
}
