#include <stdio.h>

main(argc, argv)
int argc;
char **argv;

/* main(int argc, char **argv) */
{
  int i;

  for (i = 0; i < argc; i++)
  { printf ("arg number %d of %d : %s \n",i, argc, argv[i]);
    printf ("arg number %d of %d : %s \n",i, argc, *(argv+i) );
  }
  printer(argc, argv); 
}

printer (int argc, char **argv)
{

  int i;

  for (i = argc-1; i >= 0; i-- )
  { printf ("arg number %d of %d : %s \n",i, argc, argv[i]);
  }
return;
}

