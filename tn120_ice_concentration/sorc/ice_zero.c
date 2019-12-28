#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int ice_zero(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp, 
  int *count_n, int *count_s)
/* Zero out the information in the temporary data files for ssmi 
      average computation */
{
  int i, j;
  ssmi_tmp blank;

  blank.t19v = 0;
  blank.t19h = 0;
  blank.t22v = 0;
  blank.t37v = 0;
  blank.t37h = 0;
  blank.t85v = 0;
  blank.t85h = 0;
  blank.conc_bar = 0;

  j = (NX_NORTH)*(NY_NORTH);
  for (i = 0; i < j; i++)
  { 
    north_tmp[i] = blank;
    count_n  [i] = 0;
  }

  j = (NX_SOUTH)*(NY_SOUTH);
  for (i = 0; i < j; i++)
  { 
    south_tmp[i] = blank;
    count_s  [i] = 0;
  }

  return 0;

}
