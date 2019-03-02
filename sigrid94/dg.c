#include <stdio.h>
#include <stdlib.h>

extern FILE *outs;
extern int inf_parn, code_count[10];
extern char inf_par[10][2];

void dg(int start, int end, int *dgrecs, char *rec1, FILE *lists)
{
int s, e, count, ptcnt, ll_count, par, j, precs;
char value[4], num[2];

  ptcnt = 0;
  ll_count = 0;
  s = start + 1;
  if (*(rec1+s) == 'R') 
    {
    s += 1;
    num[0] = *(rec1+s);
    num[1] = *(rec1+s+1);
    count  = atoi(&num[0]);
    ptcnt += count;
    s += 2;
    value[0] = *(rec1+s);
    value[1] = *(rec1+s+1);
    value[2] = *(rec1+s+2);
    value[3] = *(rec1+s+3);
    if (value[0] == 'L' && value[1] == 'L')
      { ll_count = ll_count + count;
/*        fprintf(outs," on land, count = %d \n", count); */
      *dgrecs += count;
      }
     else
     {
      while (s+1 < end)
      {
        precs = *dgrecs;
        par = 0;
        num[0] = *(rec1+s);
        num[1] = *(rec1+s+1);
        for (j = 0; j < inf_parn ; j++)
        { 
          if( *(rec1+s) == inf_par[j][0] &&  *(rec1+s+1) == inf_par[j][1] )
            { par = j ;
              code_count[par] += count;
            }
        }
        if (par == 0 &&    \
          (*(rec1+s) != 'L' && *(rec1+s+1) != 'U') )  \
        {fprintf(outs,"unknown code %2s %d \n", num, count);  }
        if ( *(rec1+s) == 'C' && *(rec1+s+1) == 'T')
          {  writerec(count, &value[0], precs, lists);
          }
        s += 4;
      } /* end while */
      *dgrecs += count;
    }  /* end else */
  }  /* end major then */
  else
   {  fprintf(outs," Grid subdiv found in record \n"); 
   }  /* end major if */

  return;
}
