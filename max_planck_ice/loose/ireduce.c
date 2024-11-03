#include <stdio.h>
#include <malloc.h>

/* N:1 resolution reducer working on a integer field */
/* Bob Grumbine 26 March 1995 */
 
int ireduce(int *ffld, float *small, const int nxin, const int nyin, const int nreduce)
{
  int i, j, k, l, index;
  int nx, ny;
  int *final;
  int count;
  float avg;
  
   nx = nxin / nreduce; 
   ny = nyin / nreduce;
   if (nxin % nreduce != 0 || nyin % nreduce != 0) {
      printf("Reduced map is not an integral subset of the original, \
              will crop\n");
   }

   final = malloc(nx*ny*sizeof(int));
   
   for (j = 0; j < ny; j++)
   {
      for ( i = 0; i < nx; i++)
      {
        small[i+j*nx] = 0; 
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

        avg = (float) avg / ((float)count);
         
        small[i+j*nx] = (float) avg;
        final[i+j*nx] = avg;
      }        
    }

    free(final);

    return 0;
   
}
