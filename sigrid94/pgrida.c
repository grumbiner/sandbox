#include <stdio.h>
#include <math.h>
#include "sigrid.h"

void interval(char *grid, const float radius, const float res, 
     const int lat_mins, const int nlats, const int nlongs);
void interp(const int ilat, const int jlon, const int nlongs, 
            const int nmatch, const int *matches, char *grid);

/* Open the input and output data files, 
   zero the data grid
   Insert and interpolate data to regular grid
*/

int main(int argc, char *argv[])
{
  sigrid_point point;
  char grid[nlat][nlong];
  FILE *input_e, *input_w, *output;
  int i, j, fail;

  if (argc < 4) { printf("Need east file, west file, output file names\n");
                  return -1;
                }
  input_e = fopen(argv[1], "r");
  input_w = fopen(argv[2], "r");
  output  = fopen(argv[3], "w");
  if (input_e == NULL || input_w == NULL || output == NULL) {
    printf("Failed to open one of the files\n");
    return -1;
  }

  for (i = 0; i < nlat; i++)
  {  for (j = 0; j < nlong; j++)
     { grid[i][j] = 0;
     }
  }

  i = 0;
  fail = (1==0);
  while (!feof(input_e) && !fail)
  {
    i++;
    j = fread(&point, sizeof(sigrid_point), 1, input_e);
    if (j != 1) {
      fail = (1==1);
    }
    if (fail) break;
    grid[point.lat][point.lon] = point.con;
  }

  i = 0;
  fail = (1==0);
  while (!feof(input_w) && !fail)
  {
    i++;
    j = fread(&point, sizeof(sigrid_point), 1, input_w);
    if (j != 1) {
      fail = (1==1);
    }
    if (fail) break;
    grid[point.lat][point.lon] = point.con;
  }

  printf("Now try to fill in gaps based on being closer than dx to the\\
          next pseudo-grid point\n");
  interval(&grid[0][0], rearth, nom_res, lat_min, nlat, nlong);

  if (nlat*nlong != fwrite (grid, sizeof(char), nlat*nlong, output) ) {
    printf("Failed on write out\n");
    return -1;
  }

  fclose(input_e);
  fclose(input_w);
  fclose(output);

  return 0;

}
