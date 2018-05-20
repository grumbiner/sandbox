#include <stdio.h>
#include <string.h>

#define MAXLIN 40

int main(void)
{
  FILE *fp;
  char line[MAXLIN], *token, *tempor;
  int stack_open;

  fp = fopen("config1.dat","r");

  while (fp != NULL && !feof(fp))
  { fgets(line, MAXLIN, fp);
    printf("line is %s",line);
    tempor = strtok(line," ") ;
    printf("tempor = %s \n",tempor);
    if (feof(fp)) {
      return(-1);
    }
  }

  return (0);
}
