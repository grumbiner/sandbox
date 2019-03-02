#include <stdio.h>

void read_data(int i, char *rline, int *rec_count, int *drec_count, FILE *test,
  int MAXLINE)
{
  fgets(rline, MAXLINE, test);
/*   if (i != 0) {  *(rline+MAXLINE*i) = ' '; } */
  *rec_count += 1;
  *drec_count += 1;
  return ;
}
