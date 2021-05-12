#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>  
#include <fcntl.h>

#include "mvector.h"
#include "ncepgrids.h"

#define HIRES
#include "ssmi.h"
// Program to experiment with training a genetic algorithm to
//   flag weather points.
// Robert Grumbine 27 July 2001

#include "collect.C"
#include "specialized.C"


int main(int argc, char *argv[]) {
  mvector<mvector<int> > genome(POPULATION);
  mvector<mvector<int> > elite(POPULATION);
  mvector<float> weights(GENES);
  mvector<float> scores(POPULATION), elite_scores(POPULATION);
  mvector<ssmi_bufr_line> test_data(1000);
  global_ice<float> sst, ice;
  int i, j;
  float best, mean;
  int generation = 0, genmax = 20;
  int fin;
  ssmi_bufr_line line_tmp;
  FILE *sstin;

//Initialize:
  srand(1);

  for (i = 0; i < POPULATION; i++) {
     genome[i].resize(GENES*(BITS+1));
     elite[i].resize(GENES*(BITS+1));
     newgenes(genome[i]);
  }

//Obtain some data to work with (generic step with specialized implementation)
  sstin = fopen("sst", "r");
  if (sstin == (FILE *) NULL ){
    printf("failed to open sst file\n");
    return 2;
  }
  sst.binin(sstin);
  fclose(sstin);
  if (sst.average() > 30.) sst -= 273.15; // convert to C

  sstin = fopen("ice","r");
  if (sstin == (FILE *) NULL) {
    printf("failed to open ice filt\n");
    return 3;
  }
  ice.binin(sstin);
  fclose(sstin);
  if (ice.average() < 10.) ice *= 100.; // convert to percents

  fin = open(argv[1], O_RDONLY);
  if (fin <= 0) {
    printf("Failed to open %s\n",argv[1]);
    printf("Quitting now!\n");
    return 1;
  }
  for (i = 0; i < test_data.xpoints(); i++) {
    j = 1 + (int) ((40. * rand()) / (RAND_MAX+1.0));
    //CD printf("j = %d\n",j);
    lseek(fin, j*sizeof(ssmi_bufr_line), SEEK_CUR);
    read(fin, &line_tmp, sizeof(ssmi_bufr_line));
    test_data[i] = line_tmp;
  }
  close(fin);     
  //CDfor (i = 0; i < test_data.xpoints(); i++) {
    //CDprintf("%d %d %d %d %d %d %d\n",
    //CD   test_data[i].year, test_data[i].month, test_data[i].day,
    //CD   test_data[i].hour, test_data[i].mins, test_data[i].secs,
    //CD   test_data[i].scan_no);
  //CD}
 
// Now start evaluating/evolving:
  scores = (float) 0.0;
  elite_scores = (float) 0.0;
  do {
    generation += 1;
    for (i = 0; i < POPULATION; i++) {
      if (scores[i] == 0.0) {
        transcriber(genome[i], weights);
        scores[i] = scorer(sst, ice, test_data, weights);
      } 
    }
    best = scores.maximum();
    mean = scores.average();
    printf("generation %d scores: %f %f %f %f\n", generation,
       scores.maximum(),  scores.average(),  scores.rms(),  scores.norm(4) );
    fflush(stdout);

    reproducer(genome, scores);
    order(genome, scores);

    printf("\ngeneration %3d top 10 list\n", generation);
    for (i = 0; i < 15; i++) { 
      printf("score %f ",scores[i]);
      showgenes(stdout, genome[i]);
      fflush(stdout);
    }
    printf("\n");

    grazer(genome, scores);
  } while (best > 1.125*mean && generation < genmax);

  printf("\n");
  for (i = 0; i < genome.xpoints(); i++) {
    printf("%f :  ",scores[i]);
    showgenes(stdout, genome[i]);
  }


  return 0;

}
