#include "ncepgrids.h"

int main(void) {
  global_ice<float> glob;
  printf("glob nx ny = %d %d\n",glob.xpoints(), glob.ypoints() );

  return 0;
}
