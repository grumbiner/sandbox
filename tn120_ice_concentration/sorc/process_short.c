#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

int process_short(const char *buffer, int *locate, struct short_data *c)
/* Process the short data record */
{

  int posit;

  posit = *locate;

  c->latitude     = 256*buffer[posit  ] + buffer[posit+1];
  c->longitude    = 256*buffer[posit+2] + buffer[posit+3];
  c->t85v         = 256*buffer[posit+4] + buffer[posit+5];
  c->t85h         = 256*buffer[posit+6] + buffer[posit+7];
  c->surface_type = buffer[posit+8];
  c->position_num = buffer[posit+9];

  *locate += 10;

  return check_short(c);
}
