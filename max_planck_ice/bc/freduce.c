#include <stdio.h>
#include <stdlib.h>

extern int fany(float *x, const int i, const int j, const int nx, 
                          const int reduce, const int type) ;

/* N:1 resolution reducer working on a floating point field */
/* Bob Grumbine 26 March 1995 */
 
int freduce(float *ffld, const int nxin, const int nyin, const int nreduce,
            FILE *out)
{
  int i, j, k, l, index;
  int nx, ny;
  float *final;
  int count;
  float avg;
  
   nx = nxin / nreduce; 
   ny = nyin / nreduce;
   if (nxin % nreduce != 0 || nyin % nreduce != 0) {
      printf("Reduced map is not an integral subset of the original, \
              will crop\n");
   }


   final = malloc(nx*ny*sizeof(float));
   
   for (j = 0; j < ny; j++)
   {
      for ( i = 0; i < nx; i++)
      {
        avg = 0.;
        count = 0;

        for (k = 0; k < nreduce; k++)
        {  for (l = 0; l < nreduce; l++)
           { 
             index = (i*nreduce + l) + (j*nreduce + k)*nxin ;
               avg  += ffld[index];
               count += 1;
           }
        }

        avg = avg / count;
        final[i+j*nx] = avg;

      }        
    }

    fwrite(final, sizeof(float), nx*ny, out);
    free(final);

    return 0;
   
}
