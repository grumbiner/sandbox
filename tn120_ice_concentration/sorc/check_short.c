#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

int check_short(struct short_data *c)
{
  int nerr = 0;

  if (c->latitude > 18000) {
    nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if (c->longitude > 36000) {
    nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }

  return nerr;
  
}
