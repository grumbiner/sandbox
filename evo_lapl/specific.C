//problem-specific:

#include "genes.h"
#include "mvector.h"

class problem {
  public:
    mvector<float> observations;

    problem(void);
    float scorer(mvector<united> &);
};
problem::problem(void) {
  observations.resize(365*40);
}
float problem::scorer(mvector<united> &x) {
  return 0;
}

float runscore(mvector<united> &weights) {
  problem x;
  return x.scorer(weights);
}

////////////////////////////////////////////////////////////////////////////////
void get_data(int argc, char *argv[]) {
  FILE *fin;
  float tmp;
  problem x;

  fin = fopen(argv[1], "r");
  for (int i = 0; i < x.observations.xpoints(); i++) {
    fscanf(fin, "%f", &tmp);
    x.observations[i] = tmp;
  }
  return;
}

void get_data(int argc, char *argv[]);
void setup_genes(mvector<mvector<int> > &genome, mvector<united> &weights, mvector<float> &scores, genetic_code &gc, int &population, int &genmax);
float runscore(mvector<united> &weights);

