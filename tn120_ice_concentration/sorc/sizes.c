#include <stdio.h>
#include "ssmi.h"
#include "icessmi.h"

/* Print out the sizes of defined structs */
int main(void)
{
  ssmi a;
  ssmi_tmp b;
  struct short_data c;
  struct long_data  d;
  struct data_record e;

  printf("size of ssmi        %d\n",sizeof(a));
  printf("size of ssmi_tmp    %d\n",sizeof(b));
  printf("size of short_data  %d\n",sizeof(c));
  printf("size of long_data   %d\n",sizeof(d));
  printf("size of data_record %d\n",sizeof(e));

  return 0;

}
