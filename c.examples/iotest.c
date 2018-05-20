#include "stdio.h"

/* Test the messes about opening and reading files, writing
  to others */

const int MAXLINE = 80;

void main()
{
FILE *fopen(), *test, *outs ;
char line[80];

test = fopen("text","r");
outs = fopen("outs","w");

while (!feof(test)) {
  fgets(line, MAXLINE, test);
  fputs(line, stdout);
  fputs(line, outs); }
;
}
