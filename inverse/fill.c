#include <stdio.h>
#include <stdlib.h>

/* Robert Grumbine */
/* Last Modified 13 February 1996 */

void vfill(unsigned char *v, int npts);

const int ndays = 9, ny = 355, nx = 345;

int main(void)
{
  char *inames[ndays], *onames[ndays];
  unsigned char map[ndays][ny][nx], vec[ndays];

  int i, j, k;
  FILE *fin[ndays], *fout[ndays];

  for (k = 0; k < ndays; k++) {
    inames[k] = malloc(sizeof(char)*ndays);
    gets(inames[k]);

    fin[k] = fopen(inames[k], "r");
    i = fread( &map[k][0][0], sizeof(unsigned char), nx*ny, fin[k]); 
    if (i != nx*ny) printf("failed to read in full data\n");
    fclose(fin[k]);

    onames[k] = malloc(sizeof(char)*ndays);
    gets(onames[k]);

    printf("in, out = %s %s \n",inames[k], onames[k] );
  }


  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
  
      for (k = 0; k < ndays; k++) {
        vec[k] = map[k][j][i]; 
      }
      vfill(vec, ndays);
      for (k = 0; k < ndays; k++) {
        map[k][j][i] = vec[k];
      }

    }
  }

  for (k = 0; k < ndays; k++) {
    fout[k] = fopen(onames[k], "w");
    fwrite( &map[k][0][0], sizeof(unsigned char), nx*ny, fout[k]);
    fclose(fout[k]);
  }

  return 0;
}

void vfill( unsigned char *v, int npts)
{
  int i, j;
  int first, jlast, last, clean;

  clean = (1 == 1);
  first = -1;
  last = -1;
  for (i = 0; i < npts; i++) {
    if (v[i] < 128) {
      last = i;
      if (first == -1) first = i;
    }
    else { 
      clean = (1 == 0);
    }
  }
  if (clean) return;

/* Fill out start of record */
  if (first != 0) {
    for (i = 0; i < first; i++) {
       v[i] = v[first];
    }
  }

/* Fill in internal gaps */
  for (i = 1; i < last; i++) {
     if (v[i] > 128) {
       j = i;
       while (j > 0) {
          j -= 1;
          if (v[j] < 128) {
             first = j;
             j = 0;
          }
       }
       j = i;
       while (j < last) {
         j += 1;
         if (v[j] < 128) {
            jlast = j;
            j = npts;
         }
       }
       v[i] = ( (float) (i - first) ) / ( (float) (jlast - first) ) * 
              (float) (v[jlast] - v[first]) 
             + v[first] + 0.5;
     } /* end of filling in an internal gap.  Note that jlast is needed
          because last is a loop control variable. */
  }

  return ;
}
       

  
   
