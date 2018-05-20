#include <stdio.h>

main(int argc, char **argv)
{
  int i;

  for (i = 0; i < argc; i++)
  { printf ("arg number %d of %d : %s \n",i, argc, argv[i]);
    printf ("arg number %d of %d : %s \n",i, argc, *(argv+i) );
  }
 
} 
