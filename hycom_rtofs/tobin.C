#include "resops.h"

int main(int argc, char *argv[]) {
    FILE *fina, *finb, *finlat, *finlon, *fout;
    char line[HYCOMLINELIM];
    grid2<float> lat, lon;
    int nx, ny;
    int pad_size;
    float pad_value;
    mvector<float> padding;

//NOTE Extract for read/write
    finb   = fopen(argv[1],"r");
    finlat = fopen(argv[2],"r");
    finlon = fopen(argv[3],"r");
    fout   = fopen(argv[4],"w");

    if (finb == (FILE *) NULL) {
      cout << "Failed to open the fort.061a file\n" << flush;
      exit(1);
    }

    printf("linelim = %d\n",HYCOMLINELIM); fflush(stdout);

  // Get nx, ny
    fgets(line, HYCOMLINELIM, finb);
    sscanf(line," %d ",&nx );
    //printf("line = %s\n",line); fflush(stdout);

    fgets(line, HYCOMLINELIM, finb);
    sscanf(line," %d ",&ny );
    //printf("line = %s\n",line); fflush(stdout);

    fclose (finb);

    printf("nx, ny = %d %d\n",nx, ny); fflush(stdout);

  // Compute the padding-related terms
    pad_size = ((nx*ny + 4095)/4096)*4096 - nx*ny;
    padding.resize(pad_size);
    pad_value = pow(2.0, 100.0);
    padding = pad_value;

  // Size and read in the lat, lon grids
    lat.resize(nx, ny);
    lon.resize(nx, ny);

    ijpt loc;
    int ti, tj;
    float tlat, tlon;
    for (loc.j = 0; loc.j < ny; loc.j++) {
      printf("j = %d\n",loc.j); fflush(stdout);
    for (loc.i = 0; loc.i < nx; loc.i++) {
      fscanf(finlat, "%d %d %f\n",&ti, &tj, &tlat);
      fscanf(finlon, "%d %d %f\n",&ti, &tj, &tlon);
      if (ti != loc.i || tj != loc.j) {
        printf("mis-sequenced %d %d vs. %d %d\n",loc.i, loc.j, ti, tj);
      }
      lat[loc] = tlat;
      lon[loc] = tlon;
    }
    }

    lon.binout(fout); padding.binout(fout);
    lat.binout(fout); padding.binout(fout);

    fclose (fout);

    return 0;

}
