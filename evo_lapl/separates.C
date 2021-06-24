#include "grid_math.h"

#include "genes.h"

void make_ref(grid2<double> &x);
int get_exact(grid2<double> &x, grid2<double> &y);
#define GRIDSIZE 12
grid2<double> working(GRIDSIZE, GRIDSIZE);

#define GRID_POINTS (GRIDSIZE*GRIDSIZE)
#define POPULATION  1024

typedef mvector<mvector<int> > genomes;

void transcriber(genomes &g, grid2<mvector<united> > &weights, genetic_code &x) ;
void findweight(mvector<int> &genes, mvector<united> &one_weight, genetic_code &x) ;
float scorer(grid2<double> &initial, grid2<mvector<united> > &weight, 
             grid2<double> &y,    const int itmax);
int similarity(genomes &x, genomes &y) ;

int main(int argc, char *argv[]) {
  genetic_code ref_code(3);
  mvector<mvector<mvector<int> > > population(POPULATION);
  mvector<mvector<int> > reference_genome;
  mvector<float> scores(POPULATION);
  // We'll transcribe a genome in to a grid of weights.
  grid2<mvector<united> > weight(GRIDSIZE, GRIDSIZE);
  int i, j;
  united f1, f2;
  grid2<double> initial(GRIDSIZE, GRIDSIZE), exact(GRIDSIZE, GRIDSIZE);
  grid2<double> tmp(GRIDSIZE, GRIDSIZE);
  int generation = 0, genmax, itmax, epoch, epmax, ref_iters;
  ijpt loc;
  float s0, s1;

  if (argc < 4) return 1;

  genmax = atoi(argv[1]);
  itmax  = atoi(argv[2]);
  epmax  = atoi(argv[3]);

  f1.fval = 0.0 ; f2.fval = 2.;
  ref_code.newgene(0, 7, FLOAT_TYPE,  f1, f2);
  f1.fval = 0.0 ; f2.fval = 1.0;
  ref_code.newgene(1, 7, FLOAT_TYPE, f1, f2);
  f1.ival = 1 ; f2.ival = 4;
  ref_code.newgene(2, 2, INT_TYPE, f1, f2);
  
  for (i = 0; i < population.xpoints(); i++) {
    population[i].resize(GRID_POINTS);
  }
  reference_genome.resize(GRID_POINTS);
  for (i = 0; i < GRID_POINTS;  i++) {
    weight[i].resize(3);
  }
  
  make_ref(initial);
  ref_iters = get_exact(initial, exact);

//////////////////// Begin Working /////////////////////////
  weight[0][0].fval = 1. ;
  weight[0][1].fval = .25;
  weight[0][2].ival = 1  ; 
  for (j = 0; j < reference_genome.xpoints(); j++) {
     reference_genome[j].resize(ref_code.code_length);
     retroscribe(reference_genome[j], weight[0], ref_code);
  }

  for (i = 0; i < population.xpoints(); i++) {
    for (j = 0; j < population[i].xpoints(); j++) {
       (population[i])[j].resize(ref_code.code_length);
       newgenes(population[i][j]);
       //retroscribe(population[i][j], weight[0], ref_code);
    }
    if (i != 0) mutate(population[i]);
  }

  for (epoch = 1; epoch <= epmax; epoch++) {
    //for (i = 0; i < population.xpoints(); i++) {
    //  for (j = 0; j < population[i].xpoints(); j++) {
    //     (population[i])[j].resize(ref_code.code_length);
    //     newgenes(population[i][j]);
    //     //retroscribe(population[i][j], weight[0], ref_code);
    //  }
    //  if (i != 0) mutate(population[i]);
    //}

    generation = 0;
    scores = 0.0;

    while (scores.maximum() < max(3.e14, 200.*pow((float)epoch,4.) ) && 
                 generation < genmax) {
      //ensure that the standard form is always present in initial population:
      population[POPULATION/4] = reference_genome;
      scores[POPULATION/4]     = 0.;
      for (i = 0; i < population.xpoints(); i++) {
        if (scores[i] == 0.0) {
          transcriber(population[i], weight, ref_code) ;
          scores[i] = scorer(initial, weight, tmp, itmax);
        }
      }
      printf("epoch %d generation \t%d\t best \t%e\t mean \t%e\n",
          epoch,generation, scores.maximum(), scores.average() );
      fflush(stdout);
  
           order(population, scores);
// look at genomic similarity -- == # different bits
      //for (i = 0; i < population.xpoints(); i++) {
      //   printf("i %4d similarity %4d score %8.6f\n",
      //           i, similarity(population[0],population[i]), scores[i] );
      //}
//
          grazer(population, scores);
      reproducer(population, scores);
  
      generation += 1;
    
    }

    transcriber(population[0], weight, ref_code) ;
    s1 = scorer(initial, weight, tmp, 1);
    transcriber(reference_genome, weight, ref_code) ;
    s0 = scorer(initial, weight, tmp, 1);
    printf("epoch_end %3d score %e %e %4d  %e\n",
            epoch, s0, s1,
            similarity(reference_genome, population[0]),
            scores.average() );
    initial = tmp;
  }
//////////////////// End main loop /////////////////////////
  //return 0;
  for (i = 0; i < population.xpoints(); i++) {
     if (scores[i] != 0.) {
       printf("i %4d similarity %4d score %e\n",
               i, similarity(population[0],population[i]), scores[i] );
     }
  }
  transcriber(population[0], weight, ref_code) ;
  for (loc.j = 1; loc.j < weight.ypoints() - 1 ; loc.j++) {
  for (loc.i = 1; loc.i < weight.xpoints() - 1 ; loc.i++) {
     printf("zz %3d %3d  %9.6f %9.6f %2d\n",loc.i, loc.j,
                                         weight[loc][0].fval,
                                         weight[loc][1].fval,
                                         weight[loc][2].ival );
  }
  }

  //return 0;
  for (i = 0; i < ref_iters; i++) {
    printf("iter %d score %e\n", i, scorer(initial, weight, tmp, 1));
    initial = tmp;
  }

  make_ref(initial);
  transcriber(reference_genome, weight, ref_code) ;
  for (i = 0; i < ref_iters; i++) {
    printf("ref iter %d score %e\n", i, scorer(initial, weight, tmp, 1));
    initial = tmp;
  }
  
return 0;

  get_exact(tmp, exact);
  exact.printer(stdout);
  tmp -= exact;
  tmp.printer(stdout);

  return 0;

}
//////////////////////////////////////////////////////////  
void make_ref(grid2<double> &x) {
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
    x[loc] = (float)loc.i/(float)(x.xpoints()-1) * t2;
  }
  loc.j = x.ypoints() - 1;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    x[loc] = t2 + (float)loc.i/(float)(x.xpoints()-1) * t2;
  }

  loc.i = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    x[loc] = (float)loc.j/(float)(x.ypoints()-1) * t2;
  }
  loc.i = x.xpoints() - 1;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    x[loc] = t2 + (float)loc.j/(float)(x.ypoints()-1) * t2;
  }

  return ;
}

int get_exact(grid2<double> &x, grid2<double> &y) {
  ijpt loc, locip, locjp, locim, locjm;
  int iter = 0, itmax = 1024;
  double lim = 0.0001, del;
  grid2<double> lap;
  
  lap.resize(x.xpoints(), x.ypoints());
  lap.set( (double) 0.0);
  working.set( (double) 0.0);
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
    
    printf("exact iter del = %d %f rms delta %e rms.lap^3 %e\n",
        iter, del, 1./working.rms(), pow(1./lap.rms(),3.) ); fflush(stdout);
    iter += 1;
  }
  while (iter < itmax && del > lim);
  
  return iter ;
}

void transcriber(genomes &g, grid2<mvector<united> > &weights, genetic_code &x) {
  int i;
  for (i = 0; i < g.xpoints(); i++) {
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

//
float scorer(grid2<double> &initial, grid2<mvector<united> > &weight, 
             grid2<double> &y,  const int itmax) {
  ijpt loc;
  ijpt locip, locjp, locim, locjm;
  int k, iters;
  double a, b, del = 0.;
  //grid2<float> y(initial.xpoints(), initial.ypoints() );
  int nx = initial.xpoints(), ny = initial.ypoints();

  for (loc.j = 0; loc.j < ny ; loc.j++) {
  for (loc.i = 0; loc.i < nx ; loc.i++) {
    y[loc] = initial[loc];
  }
  }

  for (iters = 0; iters < itmax; iters++) {
    del = 0.;
    for (loc.j = 1; loc.j < ny - 1; loc.j++) {
    for (loc.i = 1; loc.i < nx - 1; loc.i++) {
      a = weight[loc][0].fval;
      b = weight[loc][1].fval;
      //a = 1.;
      //b = 0.25;
      k = weight[loc][2].ival; 
  
      locjp.j = min(ny - 1,loc.j + k);
      locjm.j = max(0,loc.j - k);
      locip.j = loc.j;
      locim.j = loc.j;
      locjp.i = loc.i;
      locjm.i = loc.i;
      locip.i = min(nx - 1,loc.i + k);
      locim.i = max(0,loc.i - k);
      working[loc] = (b*(y[locip] + y[locim] + y[locjp] + y[locjm])
                   - a*y[loc] )/(double)(k*k);
                   //- a*y[loc] ) ;
    }
    }
    del = max(del, working.rms());
    y += working;
  }
  y.laplace(working);
  //return 1./working.rms() ;
  return pow(1./working.rms(), 3.) ;
// Lease absolute deviation:
//  return (1./max(fabs(working.gridmax()), fabs(working.gridmin()) ) );
}
int similarity(genomes &x, genomes &y) {
//typedef mvector<mvector<int> > genomes;
  int ng = x.xpoints();
  int genes;
  int i, j;
  int count = 0;

  genes = x[0].xpoints();
  for (i = 0; i < ng; i++) {
    for (j = 0; j < genes; j++) {
       if (x[i][j] != y[i][j] ) {
         count += 1;
       }
    }
  }
  //return (1. - (float)count/(float)(genes*ng) );
  return count;
}
