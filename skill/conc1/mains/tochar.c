#include <stdio.h>

#define NX_NORTH 385
#define NY_NORTH 465
#define NX_SOUTH 345
#define NY_SOUTH 355

int main(int argc, char *argv[])
{

  FILE *finn, *fins, *foutn, *fouts;
  float fnmap[NY_NORTH][NX_NORTH], fsmap[NY_SOUTH][NX_SOUTH];
  char cnmap[NY_NORTH][NX_NORTH], csmap[NY_SOUTH][NX_SOUTH];
  float nx, ny;
  int i, j;
  char fname[90];

  i = atoi(argv[1]);
  sprintf(fname, "avgout.north.%d", i); 
  finn = fopen(fname, "r");
  sprintf(fname, "avgout.south.%d", i); 
  fins = fopen(fname, "r");

  fread(&nx, sizeof(float), 1, finn);
  fread(&ny, sizeof(float), 1, finn);
  fread(fnmap, sizeof(float), nx*ny, finn);
  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
       cnmap[j][i] = fnmap[j][i] + 0.5;
    }
  }

  foutn = fopen("cavg.north", "w");
  fwrite(cnmap, sizeof(char), nx*ny, foutn);
  
  fread(&nx, sizeof(float), 1, fins);
  fread(&ny, sizeof(float), 1, fins);
  fread(fsmap, sizeof(float), nx*ny, fins);
  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
       csmap[j][i] = fsmap[j][i] + 0.5;
    }
  }

  fouts = fopen("cavg.south", "w");
  fwrite(csmap, sizeof(char), nx*ny, fouts);
  
  return 0;

}
