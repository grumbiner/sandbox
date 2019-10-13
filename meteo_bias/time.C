#include "ncepgrids.h"

#define NMRF 53
#define NSSMI 21
#define TOTAL 74

#define MRF_LAND 31
#define MRF_ICE 32

#define ALLTYPE 0
#define LONLY 1
#define IONLY 2
#define OONLY 3
#define ALLOCE 4 

int main(int argc, char *argv[]) {
  northgrid<float> nh[NMRF + NSSMI];
  FILE *finmrf, *finssmi, *nout;

  int i, j;
  int count, nx_north, ny_north;

  finmrf = fopen(argv[1], "r");
  finssmi = fopen(argv[2], "r");
  if (finmrf == (FILE*) NULL || finssmi == (FILE*) NULL || 
      nout == (FILE*) NULL) {
    printf("Failed to open a north file\n");
    return 1;
  }

  nx_north = nh[0].xpoints();
  ny_north = nh[0].ypoints();
  while (!feof(finmrf) && !feof(finssmi) ) {
    for (i = 0; i < NMRF ; i++) {
      nh[i].binin(finmrf);
    }
    for (i = NMRF; i < NMRF + NSSMI; i++) {
      nh[i].binin(finssmi);
    } 
  }

  return 0;
}
