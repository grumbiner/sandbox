#include <stdio.h>
#include <malloc.h>

int read_k(FILE *test, FILE *outs, FILE *lists, int *k_ndatarecs, \
         int *k_mesh_ratio, int *k_latoffset, int *k_lonoffset, \
         int *k_ngpts, int *exp_ptcnt, int *krec_count, int *rec_count, \
         int MAXLINE)
{
/* Declare local use variables */
char dum[2][10];
char *rline;
char a, b;

  rline = malloc(sizeof(char)*MAXLINE);
  fgets(rline, MAXLINE, test);
  fputs(rline, outs);
  fputc('\n', outs); 

/*  fprintf(outs," %d %d krec_count \n", *krec_count, *rec_count); */ 

  if (feof(test))
     {printf (" out of data \n"); 
      free(rline);
      return -1 ; }
    else
   {
    a = rline[0];
    b = rline[1];
    *krec_count += 1;
    *rec_count += 1;

/* verify that this is a 'k' record , read until finding one*/
    while ( (a != '=' || b != 'K' ) && !feof(test)  ) 
      { printf (" This is not a properly formatted K record \n");
        printf ("%81s\n",rline);
        fgets(rline, MAXLINE, test);
        a = rline[0];
        b = rline[1];
        *krec_count += 1;
        *rec_count += 1;
        printf (" %d %d krec_count \n", *krec_count, *rec_count);
      };
    sscanf(rline,"%2s%3d%2s%3d%3d%2s%4d%2s%2d", \
      dum[0][0], k_mesh_ratio, dum[0][1], k_latoffset, k_lonoffset, \
      dum[0][2], k_ngpts, dum[0][3], k_ndatarecs);

    *exp_ptcnt += *k_ngpts;

    printf("read_k %d %d %d %d %d %d \n", \
     *k_mesh_ratio, *k_latoffset, *k_lonoffset,\
      *k_ngpts, *k_ndatarecs, *exp_ptcnt);

    free(rline);
    return 0;

  }
}
