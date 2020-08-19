#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "cofs.h"

#include "params.h"
#include "genes.h"

#include "lapl.C"

// Program to experiment with training a genetic algorithm to
//   construct a gulf-stream finding function
// Robert Grumbine 6 November 2001

#define MAXPTS 1000
#define POPULATION 500

// Functions which are somewhat or very specialized towards the
// problem at hand.

float scorer(vector<fijpt> &flocs, int count, cfsreg<float> &sst, 
             cfsreg<float> &gradsq, cfsreg<float> &tlapl, 
             vector<united> &weights, float sstflag) ;

float result(ijpt &loc, cfsreg<float> &sst, float sstflag, 
             cfsreg<float> &gradsq, cfsreg<float> &tlapl, 
             vector<united>&weights) ;

void get_data(char *fname, cfsreg<float> &sst, cfsreg<float> &gradsq, 
              cfsreg<float> &tlapl, float &sstflag) ; 

void get_gulfstream(cfsreg<float> &sst, int &count, vector<fijpt> &gspoints, 
                    char *arg2, char *arg3, char *arg4) ;

////////////////// Main program ///////////////
int main(int argc, char *argv[]) {
//Gene-related:
  genetic_code x(4);
  vector<united> weights;
  vector<vector<int> > genome(POPULATION);
  vector<float> scores(POPULATION);
  float best, mean = 0;
  int generation = 0, genmax = 200;

// Problem-related:
  vector<fijpt> gspoints(MAXPTS);
  cfsreg<float> sst, gradsq, tlapl;
  cfsreg<float> t1;
  float sstflag;  
  latpt ll;
  ijpt loc, locmax;
  int i, j;
  int count = 0;
  united f1, f2;
  FILE *fin, *lineout, *fout;

// Utility:
  printf("argc = %d\n", argc); 
  for (i = 0; i < argc; i++) {
    printf("%s\n",argv[i]);
  }

/////// Check that we've been given an appropriate number of arguments:
  if (argc != 4 && argc != 5 && argc != 7 && argc != 8) {
    printf("Please check your invocation for correctness.\n");
    printf("  Mismatch in number of arguments; invoked with %d\n",argc);
    return 0;
  }

///// Read in the model data -- required regardless of the availability of 
//    a gulf stream
  get_data(argv[1], sst, gradsq, tlapl, sstflag) ;

////  If there is a Gulf Stream file to work with, read it in: 
//    Note that the games on indexing are necessary due to possibility of
//    having, or not, a previous construction of species
  if (argc == 7 || argc == 8) { 
    get_gulfstream(sst, count, gspoints, 
                    argv[argc-3], argv[argc-2], argv[argc-1]);  
    #ifdef VERBOSE
      printf("returned from the gulf stream getter\n"); fflush(stdout);
    #endif
  }

//Get critters, using _de_novo_ initialization of genetics if necessary:
  if (argc == 7) {
    #ifdef VERBOSE
      printf("Working on _de_novo_ initialization of genetics\n"); 
      fflush(stdout);
    #endif

    srand(1);
  
    f1.fval = -2.0; f2.fval = 2.0;
    x.newgene(0, 8, FLOAT_TYPE, f1, f2);
    x.newgene(1, 8, FLOAT_TYPE, f1, f2);
    x.newgene(2, 8, FLOAT_TYPE, f1, f2);
    x.newgene(3, 8, FLOAT_TYPE, f1, f2);
   
    weights.resize(x.ncodes);
    #ifdef VERBOSE
      printf("code length = %d\n",x.code_length); fflush(stdout);
    #endif

    for (i = 0; i < POPULATION; i++) {
       genome[i].resize(x.code_length);
       newgenes(genome[i]);
       #ifdef VERBOSE
         transcriber(genome[i], weights, x);
         showgenes(stdout, genome[i], x); fflush(stdout);
       #endif
    }
  }
  else if (argc == 5 || argc == 8) {

    fin = fopen(argv[4],"r");
    x.read(fin);
    weights.resize(x.ncodes);
    for (i = 0; i < genome.xpoints() && ! feof(fin); i++) {
      fscanf(fin,"%f : ",&scores[i]);
      for (j = 0; j < x.ncodes; j++) {
        if (x.code[j].type == FLOAT_TYPE) {
          fscanf(fin, "%f ",&weights[j].fval);
        }
        else {
          fscanf(fin, "%d ",&weights[j].ival);
        }
      }
      retroscribe(genome[i], weights, x);
      #ifdef VERBOSE2
        genome[i].printer(stdout);
        showgenes(stdout, genome[i], x);
        printf("\n");
      #endif
      if (feof(fin) ) break;
    }
    if (i < genome.xpoints() - 1) {
      for (j = i ; j < genome.xpoints(); j++) {
        newgenes(genome[j]);
        scores[j] = 0;
      }
    }
    fclose(fin);

    x.write(stdout);
    for (i = 0; i < genome.xpoints(); i++) {
      printf("%f : ",scores[i]);
      showgenes(stdout, genome[i], x);
    }
    printf("done with replicating genomes\n"); fflush(stdout);

  } 

////////////////////////////////////////////////////////////////////////////
// If we have a gulf stream file, start evaluating/evolving:
  if (argc == 7 || argc == 8) {
    #ifdef VERBOSE
      printf("Now attempting to work on gulf stream evolution\n"); 
      fflush(stdout);
    #endif

    scores = (float) 0.;
    generation = 0;

    do {
      #ifdef VERBOSE
      printf("in while loop, generation = %d\n",generation); fflush(stdout);
      fflush(stdout);
      #endif
      for (i = 0; i < POPULATION; i++) {
        #ifdef VERBOSE
          printf("in do loop, i = %d\n",i); fflush(stdout);
          fflush(stdout);
        #endif
        if (scores[i] == 0.0) {
          transcriber(genome[i], weights, x);
          scores[i] = scorer(gspoints, count, sst, gradsq, tlapl, 
                              weights, sstflag);
          printf("trial %f ", scores[i]); fflush(stdout);
          showgenes(stdout, genome[i], x); fflush(stdout);
        } 

      }
      best = scores.maximum();
      mean = scores.average();
      printf("epoch %d generation %d scores: %f %f %f %f\n", 0, generation,
         scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
      fflush(stdout);
  
      reproducer(genome, scores);
      order(genome, scores);
  
      printf("\ngeneration %3d top 15 list\n", generation);
      for (i = 0; i < 15; i++) { 
        printf("score %f ",scores[i]);
        showgenes(stdout, genome[i], x);
        fflush(stdout);
      }
      printf("\n");

      grazer(genome, scores);
      generation += 1;

    } while (best > (1.+1./3.)*mean && generation < genmax);
    printf("out of while loop for evolving\n"); fflush(stdout);

    for (i = 0; i < genome.xpoints(); i++) {
      printf("%f :  ",scores[i]);
      showgenes(stdout, genome[i], x);
    }

  }
  

///////////////// Given the best critter, find the gulf stream:
  transcriber(genome[0], weights, x);
  lineout = fopen(argv[3],"w");
  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
    locmax.j = loc.j;
    locmax.i = 0;
    for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
      t1[loc] = result(loc, sst, sstflag, gradsq, tlapl, weights);
      if (t1[loc] > t1[locmax]) {
        locmax = loc;
      }
    }
    ll = t1.locate(locmax);
    if ( (360. - ll.lon) > 60. && (360 - ll.lon < 82.) ) {
      printf("%5.2f %5.2f \n", ll.lat, 360.-ll.lon); 
      fprintf(lineout, "%5.2f %5.2f \n", ll.lat, 360.-ll.lon); 
    }
  }
  fclose(lineout);

// For all run types, print out the genetic code and the genomes 
  fout = fopen(argv[2], "w"); 
  x.write(fout);
  for (i = 0; i < genome.xpoints(); i++) {
    fprintf(fout,"%f :  ",scores[i]);
    showgenes(fout, genome[i], x);
  }
  fclose(fout);

  return 0;

}

/////////////////////////////////
// Compute the score for a set of weights.  Ensure that it gives
//  a high, positive score for a good parameter set.

float scorer(vector<fijpt> &flocs, int count, cfsreg<float> &sst, 
             cfsreg<float> &gradsq, cfsreg<float> &tlapl, 
             vector<united> &weights, float sstflag) {
  ijpt loc, off1;
  cfsreg<float> arr;
  float score = 0., s2 = 0, delta;
  int i, tmploc;

  #ifdef VERBOSE
    printf("entered scorer, count = %d\n",count); fflush(stdout);
  #endif

  for (i = 0; i < count; i++) {
    #ifdef VERBOSE2
      printf("Finding value for point %d\n",i); fflush(stdout);
    #endif
    loc.i = (int) (flocs[i].i + 0.5);
    if (loc.i > 360. / sst.dlon) loc.i -= (int) (0.5 + 360. / sst.dlon); 
    // keep on domains ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    loc.j = (int) (flocs[i].j + 0.5);
    if ( !sst.in(loc) ) {
      printf("Have run off the grid! %f %f %d %d\n",flocs[i].i, flocs[i].j,
            loc.i, loc.j);
      fflush(stdout);
      continue; 
    }
    delta = 0.;
    tmploc = -1;

    arr[loc] = 
      result(loc, sst, sstflag, gradsq, tlapl, weights);

    off1.i = loc.i;
    for (off1.j = 0; off1.j < arr.ypoints(); off1.j++) {
      #ifdef VERBOSE2
        printf("working on point %d %d\n",off1.i, off1.j); fflush(stdout);  
      #endif
      arr[off1] = 
        result(off1, sst, sstflag, gradsq, tlapl, weights);

      if (arr[off1] >= delta) {
        tmploc = off1.j;
        delta = arr[off1];
      }
    }
    delta = fabs(flocs[i].j - tmploc);
    score += delta*delta;
    s2    += delta;
  }

  #ifdef VERBOSE
    printf("about to return the scoring value\n"); fflush(stdout);
  #endif

  if (score != s2*s2/count && count > 1 ) {
    return 1./sqrt((score - s2*s2/count) / (count - 1) );
  }
  else {
    return 0.;
  }

}
float result(ijpt &loc, cfsreg<float> &sst, float sstflag, 
               cfsreg<float> &gradsq, 
               cfsreg<float> &tlapl, vector<united>&weights) {
  float tmp;

  #ifdef VERBOSE
    printf("entered result\n"); fflush(stdout);
  #endif
  if (sst[loc] == sstflag) return 0;

  tmp =  (pow(sst[loc], weights[0].fval) + 
          pow(gradsq[loc], weights[1].fval)*weights[2].fval 
          + tlapl[loc]*weights[3].fval  ) 
        / (1. + weights[2].fval + weights[3].fval) ;

  #ifdef VERBOSE
    printf("leaving result\n"); fflush(stdout);
  #endif
  return tmp;


} 
/////////////////////////////////
//  Read in the data -- required for all versions of the program:
void get_data(char *fname, cfsreg<float> &sst, cfsreg<float> &gradsq, 
              cfsreg<float> &tlapl, float &sstflag) { 

  FILE *sstin;
  float flag, flag2 = -99.0, flag3;
  ijpt loc, locnative;
  cfsnative<float> depth;
  latpt ll;
  
  sstin = fopen(fname, "r");
  if (sstin == (FILE *) NULL ){
    printf("failed to open %s file\n",fname);
    exit(2);
  }
  sst.binin(sstin);
  fclose(sstin);

// Clean up the data, remove flags, etc.
  flag = sst.gridmax() ;
  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
    if (sst[loc] == flag) sst[loc] = flag2;
  }
  }

  flag3 = sst.gridmin(flag2);
  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
// Second part flags out the shallow areas -- important for using
//   salinity and sea surface topography
    ll = sst.locate(loc);
    if (ll.lon > 180.) ll.lon -= 360.;
    locnative = depth.locate(ll);
    if (sst[loc] == flag2 || depth.land(locnative, 200.) ) sst[loc] = flag3;
  }
  }

  gradsq.set(0.);

  sst -= sst.gridmin(); // Moves min to zero
// Compute the squared gradient
  grad(sst, gradsq, sst.gridmin() );
// Compute the laplacean -- leave as value, since sign may be important (?)
  lapl(sst, tlapl, sst.gridmin() );

// Now rescale sst and gradsq to both have a comparable scales
  tlapl   /= tlapl.gridmax();
  gradsq  /= gradsq.gridmax();
  sst     /= sst.gridmax();
  sstflag  = sst.gridmin();
  printf("scaled sst, gradsq, lapl: max, min, average\n");
  printf("%f %f %f\n",sst.gridmax(), sst.gridmin(), sst.average() );
  printf("%f %f %f\n",gradsq.gridmax(), gradsq.gridmin(), gradsq.average() );
  printf("%f %f %f\n",tlapl.gridmax(), tlapl.gridmin(), tlapl.average() );

  return;
}
//////////////////////////////
//  For reading in Gulf Stream data files and other things specific to
//  the execution of a new evolutionary round.
void get_gulfstream(cfsreg<float> &sst, int &count, vector<fijpt> &gspoints, 
                    char *arg2, char *arg3, char *arg4) {
  float lonmin, lonmax;
  float tlat, tlon;  
  fijpt fijtmp;
  latpt ll;
  FILE *gsin;

  lonmin = atof(arg4);
  lonmax = atof(arg3);
  printf("min, max longitudes west %f %f\n",lonmin, lonmax);
  lonmin = 360. - lonmin;
  lonmax = 360. - lonmax; // convert to degrees east
  printf("min, max longitudes east %f %f\n",lonmin, lonmax);

  gsin = fopen(arg2, "r");
  if (gsin == (FILE *) NULL) {
    printf("failed to open gspts file %s\n",arg2);
    exit(3);
  }
  while (!feof(gsin) && count < MAXPTS) {
    fscanf(gsin, "%f %f \n", &tlat, &tlon);
    //printf("%f %f \n", tlat, tlon);
    ll.lat = tlat;
    ll.lon = -tlon; // convert to degrees east, with negatives for west
    fijtmp = sst.locate(ll);
    ll.lon = 360. + ll.lon; // convert to degrees east, as a positive number
    #ifdef VERBOSE
    printf("%f %f  %f %f  %f %f  %d\n",ll.lat, ll.lon, fijtmp.i, fijtmp.j,
          lonmin, lonmax, 
          (sst.in(fijtmp) && ll.lon < lonmax && ll.lon > lonmin) );
    #endif
    if (sst.in(fijtmp) && ll.lon < lonmax && ll.lon > lonmin) {
      gspoints[count] = sst.locate(ll);
      count += 1;
    }
  }
  printf("Found %d points to work with\n",count); fflush(stdout);

  if (count == 0) {
    printf("cannot run with zero points, aborting\n");
    // should probably be a throw to running without the gsfile
    exit(3);
  }

  return;
}
