#include "mvector.h"

//typedef program mvector<float>;
typedef mvector<float> program ;
 
int main(void) {
  mvector<program> population(1000);
  int i;

  for (i = 0; i < population.xpoints(); i++) {
    population[i].resize(24);
  }

  return 0;
}
