#include <stdio.h>
#include <rpc/rpc.h>
#include "date.h"

main (argc, argv)
int argc;
char **argv;
{
  CLIENT *cl;
  char *server;
  long *lresult;
  char **strresult;

  server = argv[1];
  if ((cl=clnt_create(server, DATE_PROG, DATE_VERS,"udp")) == NULL)
  {
    clnt_pcreateerror(server);
    exit(1);
  }
  
  if ((lresult=bin_date_1(NULL,cl))==NULL)
  {
    clnt_perror(cl,server);
    exit(1);
  }
  printf("Binary time %d \n",*lresult);

  strresult = str_date_1(lresult);

  if ( strresult == NULL)
  {
    clnt_perror(cl,server);
    exit(1);
  }
  printf("String date %s \n",*strresult);
  
  clnt_destroy(cl);

}

