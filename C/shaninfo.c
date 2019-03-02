#include <math.h>

/* Compute the Shannon information on an array of reals saved through a
   series (nt) of maps the reals are presumed to have range 0-255,
   as for a sea ice concentration map. 
  Robert Grumbine 6 April 1994.
*/
float shaninfo(float *x, float *sum, const int nx, const int ny, const int nt)
{

  int counter[nx][ny][256]; 
  float bits[nx][ny], total, tempor;

  int i, j, k;

/* Zero the counters */
  for (i = 0; i < nx; i++)
  { for (j = 0; j < ny; j++)
    { for (k = 0; k < 256; k++)
      { counter[i][j][k] = 0; } 
      bits[i][j] = 0.0;
    }
  }

/* Count occurrences of each value, separately by location */ 
  for (i = 0; i < nx; i++)
  { for (j = 0; j < ny; j++)
    { for (k = 0; k < nt; k++)
      { counter[i][j][ (int) *(x+k+j*nt+i*nt*ny) ] += 1;
      }
    }
  } 

/* Now start adding up the Shannon info */
  total = 0.0;
  for (i = 0; i < nx; i++)
  { for (j = 0; j < ny; j++)
    { for (k = 0; k < 256; k++)
      { if (counter[i][j][k] != 0)
          { tempor  = (float) counter[i][j][k] / ((float) nt);
            tempor *=  (- log(tempor) / log(2.) );
            bits[i][j] += tempor;
            total      += tempor;
          }
      }
    *(sum+j+i*ny) = bits[i][j];
    }
  }

  return(total);

}
