#include "grid_math.h"

#include "genes.h"

void make_ref(grid2<float> &x);
void get_exact(grid2<float> &x, grid2<float> &y);
#define GRIDSIZE 16
grid2<float> working(GRIDSIZE, GRIDSIZE);

#define GRID_POINTS (GRIDSIZE*GRIDSIZE)
#define POPULATION  1024

typedef mvector<mvector<int> > genomes;

void transcriber(genomes &g, grid2<mvector<united> > &weights, genetic_code &x) ;
void findweight(mvector<int> &genes, mvector<united> &one_weight, genetic_code &x) ;
float scorer(grid2<float> &initial, grid2<mvector<united> > &weight);
float iterator(grid2<float> &initial, grid2<mvector<united> > &weight, 
                const int itmax);

int main(int argc, char *argv[]) {
  genetic_code ref_code(3);
  mvector<mvector<mvector<int> > > population(POPULATION); //1000 = population size
  mvector<float> scores(POPULATION);
  // We'll transcribe a genome in to a grid of weights.
  grid2<mvector<united> > weight(GRIDSIZE, GRIDSIZE);
  int i, j;
  united f1, f2;
  grid2<float> initial(GRIDSIZE, GRIDSIZE), exact(GRIDSIZE, GRIDSIZE);
  int generation = 0, genmax = 80, itmax;
  float best, mean;

  if (argc < 3) return 1;

  genmax = atoi(argv[1]);
  itmax = atoi(argv[2]);
  printf("Running for %d generations with %d iters \n",genmax, itmax);
  f1.fval =  .5 ; f2.fval = 1.5;
  ref_code.newgene(0, 6, FLOAT_TYPE,  f1, f2);
  f1.fval =  0. ; f2.fval = 0.5;
  ref_code.newgene(1, 6, FLOAT_TYPE, f1, f2);
  f1.ival = 1 ; f2.ival = 8;
  ref_code.newgene(2, 3, INT_TYPE, f1, f2);
  //f1.ival = 1 ; f2.ival = 16;
  //ref_code.newgene(2, 4, INT_TYPE, f1, f2);
  
  scores = 0.;
  for (i = 0; i < population.xpoints(); i++) {
    population[i].resize(GRID_POINTS);
  }
  for (i = 0; i < GRID_POINTS;  i++) {
    weight[i].resize(3);
  }
  for (i = 0; i < population.xpoints(); i++) {
    for (j = 0; j < population[i].xpoints(); j++) {
       (population[i])[j].resize(ref_code.code_length);
       newgenes(population[i][j]);
    }
  }
  
  make_ref(initial);
  get_exact(initial, exact);
  initial.laplace(working);
  printf("Score for initial field: %f\n",1./working.rms() );
  float scorelim = 1./working.rms();

  while (scores.maximum() < 2.*5.613487 && generation < genmax) {
    for (i = 0; i < population.xpoints(); i++) {
      if (scores[i] == 0.0) {
        transcriber(population[i], weight, ref_code) ;
        scores[i] = iterator(initial, weight, itmax);
        //printf("score %4d %f\n",i,scores[i]);
      }
    }
    best = scores.maximum();
    mean = scores.average();
    printf("generation \t%d\t best \t%f\t mean \t%f\n",generation, best, mean);
    fflush(stdout);

    order(population, scores);
    grazer(population, scores);
    //for (i = 0; i < 15;  i++) {
    //   printf("generation %d top 15 %2d %f\n",generation, i+1, scores[i]);
    //}
    //showgenes(stdout, population[0][105], ref_code);
    reproducer(population, scores);

    generation += 1;
    
  }
// Now display the k field for the best critter:
  mvector<united> best_weights;
  for(i = 0; i < population[0].xpoints(); i++) {
     findweight(population[0][i], best_weights, ref_code);
     printf("n %4d  %f %f  k %1d  i %2d j %2d\n",i, 
             best_weights[0].fval, best_weights[1].fval, best_weights[2].ival, 
             i % GRIDSIZE, i / GRIDSIZE);
  } 
  transcriber(population[0], weight, ref_code) ;

  for (i = itmax; i < 601; i *= 2) {
    printf("Score after %d iterations: %f\n",i, 
                            iterator(initial, weight, i) );
  }

  return 0;

}
//////////////////////////////////////////////////////////  
void make_ref(grid2<float> &x) {
  ijpt loc;
  float t2 = 2.0;
  float circle = 4.0;
  float midx = x.xpoints()/2., midy = x.ypoints()/2.;
// Build circular pattern first:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
     x[loc] = circle*( (loc.i/midx - 1.)*(loc.i/midx - 1.) + (loc.j/midy - 1.)*(loc.j/midy - 1.) );
  }
  }
// Put in boundary conditions:
  loc.j = 0;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    x[loc] = loc.i/x.xpoints() * t2;
  }
  loc.j = x.ypoints() - 1;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    x[loc] = t2 + loc.i/x.xpoints() * t2;
  }

  loc.i = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    x[loc] = loc.j/x.ypoints() * t2;
  }
  loc.i = x.xpoints() - 1;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    x[loc] = t2 + loc.j/x.ypoints() * t2;
  }

  return ;
}

void get_exact(grid2<float> &x, grid2<float> &y) {
  ijpt loc, locip, locjp, locim, locjm;
  int iter = 0, itmax = 1000;
  float lim = 0.001, del;
  grid2<float> lap;
  
  lap.resize(x.xpoints(), x.ypoints());
  lap.set( (float) 0.0);
  working.set( (float) 0.0);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    y[loc] = x[loc];
  }
  }
  
  
  do {
    del = 0.0; 
    for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
      locjp.j = loc.j + 1;
      locjm.j = loc.j - 1;
      locip.j = loc.j;
      locim.j = loc.j;
    for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
      locjp.i = loc.i;
      locjm.i = loc.i;
      locip.i = loc.i + 1;
      locim.i = loc.i - 1;      
      working[loc] = (y[locip] + y[locim] + y[locjp] + y[locjm] - 4.*y[loc]) *1.0 / 4.;
      del = max(del, fabs(working[loc]) );
    }
    } 
    y += working;
    y.laplace(lap);    
    
    printf("exact iter del = %d %f rms delta %f rms.lap %f\n",
        iter, del, working.rms(), 1./lap.rms() ); fflush(stdout);
    iter += 1;
  }
  while (iter < itmax && del > lim);
  
  return ;
}

void transcriber(genomes &g, grid2<mvector<united> > &weights, genetic_code &x) {
  int i;
  for (i = 0; i < g.xpoints(); i++) {
    //printf("%d %d\n",i, g[i].xpoints() );
    findweight(g[i], weights[i], x);
  }
  return;
}
void findweight(mvector<int> &genes, mvector<united> &one_weight, genetic_code &x) {
  int i, k, mul, base, tmp;
  float val;

  base = 0;
  for (k = 0 ; k < x.ncodes; k++) {
    mul = 1; 
    tmp = 0;
    for (i = 0; i < x.code[k].nbits; i++) {
      tmp += mul*genes[i+base];
      mul *= 2;
    }
    base += x.code[k].nbits;

    if (x.code[k].type == INT_TYPE) {
      tmp = tmp % (x.code[k].top.ival - x.code[k].base.ival + 1);
      tmp += x.code[k].base.ival;
      one_weight[k].ival = tmp;
    }
    else {
      val = (float) tmp / pow(2., x.code[k].nbits) ;
      val *= (x.code[k].top.fval - x.code[k].base.fval);
      val += x.code[k].base.fval;
      one_weight[k].fval = val;
    }

  }

  //printf("%f %f %d\n",
  //   one_weight[0].fval, one_weight[1].fval, one_weight[2].ival);

  return;
}

float scorer(grid2<float> &initial, grid2<mvector<united> > &weight) {
  ijpt loc;
  ijpt locip, locjp, locim, locjm;
  int k;
  float a, b;
  grid2<float> lap(initial.xpoints(), initial.ypoints());
  grid2<float> y(initial.xpoints(), initial.ypoints() );

  for (loc.j = 0; loc.j < initial.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < initial.xpoints() ; loc.i++) {
    y[loc] = initial[loc];
  }
  }
  for (loc.j = 1; loc.j < initial.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < initial.xpoints() - 1; loc.i++) {
    //a = weight[loc][0].fval;
    //b = weight[loc][1].fval;
    a = 1.;
    b = 0.25;
    k = weight[loc][2].ival; 

    locjp.j = min(initial.ypoints()-1,loc.j + k);
    locjm.j = max(0,loc.j - k);
    locip.j = loc.j;
    locim.j = loc.j;
    locjp.i = loc.i;
    locjm.i = loc.i;
    locip.i = min(initial.xpoints()-1,loc.i + k);
    locim.i = max(0,loc.i - k);
    working[loc] = (b*(y[locip] + y[locim] + y[locjp] + y[locjm])
                 - a*y[loc] )/(float)(k*k);
  }
  }
  y += working;
  y.laplace(lap);

  return 1./lap.rms();
}
// Modify slightly -- to return improvement after itmax iterations
float iterator(grid2<float> &initial, grid2<mvector<united> > &weight, 
               const int itmax) {
  ijpt loc;
  ijpt locip, locjp, locim, locjm;
  int k, iters;
  float a, b, s0, s1, sn;
  grid2<float> lap(initial.xpoints(), initial.ypoints());
  grid2<float> y(initial.xpoints(), initial.ypoints() );

  for (loc.j = 0; loc.j < initial.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < initial.xpoints() ; loc.i++) {
    y[loc] = initial[loc];
  }
  }
  y.laplace(lap);
  s0 = 1./lap.rms();

  a = 1.;
  b = 0.25;
  for (iters = 0; iters < itmax; iters++) {
    for (loc.j = 1; loc.j < initial.ypoints() - 1; loc.j++) {
    for (loc.i = 1; loc.i < initial.xpoints() - 1; loc.i++) {
      a = weight[loc][0].fval;
      b = weight[loc][1].fval;
      k = weight[loc][2].ival; 
  
      locjp.j = min(initial.ypoints()-1,loc.j + k);
      locjm.j = max(0,loc.j - k);
      locip.j = loc.j;
      locim.j = loc.j;
      locjp.i = loc.i;
      locjm.i = loc.i;
      locip.i = min(initial.xpoints()-1,loc.i + k);
      locim.i = max(0,loc.i - k);
      working[loc] = (b*(y[locip] + y[locim] + y[locjp] + y[locjm])
                   - a*y[loc] )/(float)(k*k);
    }
    }
    y += working;
    //if (iters == 0) {
    //  y.laplace(lap);
    //  s1 = 1./lap.rms();
    //  //printf("iterated score %d %f\n",iters, 1./lap.rms() );
    //}
  }

  y.laplace(lap);
  sn = 1./lap.rms();
  return sn - s0;
}
