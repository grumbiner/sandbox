#include <rpc/rpc.h>
#include "date.h"
#include <time.h>

long *bin_date_1()
{
  static long timeval;
  timeval=time(NULL);
  return (&timeval);
}

char **str_date_1(btime)
long btime;
{
  static char *ptr;
  ptr = ctime(&btime);
  return (&ptr);
}

