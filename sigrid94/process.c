#include <stdio.h>
#include <malloc.h>

extern FILE *outs;

void process(char *rec1, FILE *lists, int MAXBLK, int MAXLINE)
{
int start, end, dgrecs, istat, j;
int go;
char colon, space, null, cr, ff, nl;
char *rec2;
size_t size;

  dgrecs = 0;
  go = 0;
  colon = ':';
  space = ' ';
  null  = '\0';
  nl    = '\n';
  ff    = '\f';
  cr    = '\r';
  size  = sizeof(char)*MAXBLK*MAXLINE;
  rec2  = malloc(size);

  trimchar(rec1, rec2, null);
  trimchar(rec1, rec2, nl);
  trimchar(rec1, rec2, ff);
  trimchar(rec1, rec2, cr);

  if (*(rec2 ) == colon) 
    { go = 1;
      start = 0;
    }
   else
    {
     fprintf(outs," Start of data group not found, abort ");
     free(rec2);
     return ;
    }

  while (go == 1)
  {
    j = start+1 ;
    istat = strpos(rec2, colon, j);
    if (istat == 0) 
      {istat = strpos(rec2, space, j);
       go = 0;
      }
    end = istat;
/*    dgrecs += 1; */
    dg(start, end, &dgrecs, rec2, lists);
    start = end;
  }

free(rec2);
return;
}
