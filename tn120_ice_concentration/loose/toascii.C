#include "resops.h"

int main(int argc, char *argv[]) {
    FILE *fina, *finb;
    char line[HYCOMLINELIM];
    grid2<float> lat, lon;
    int nx, ny;
    int pad_size;
    float pad_value;
    mvector<float> padding;

//NOTE Extract for read/write
    fina = fopen(argv[1],"r");
    finb = fopen(argv[2],"r");
    if (fina == (FILE *) NULL) {
      cout << "Failed to open the fort.061a file\n" << flush;
      exit(1);
    }
    if (finb == (FILE *) NULL) {
      cout << "Failed to open the fort.61 file\n" << flush;
      exit(1);
    }

  // Get nx, ny
    fgets(line, HYCOMLINELIM, finb);
    sscanf(line," %d ",&(nx) );
    fgets(line, HYCOMLINELIM, finb);
    sscanf(line," %d ",&(ny) );
    fclose (finb);

  // Compute the padding-related terms
    pad_size = ((nx*ny + 4095)/4096)*4096 - nx*ny;
    padding.resize(pad_size);
    pad_value = pow(2.0, 100.0);

  // Size and read in the lat, lon grids
    lat.resize(nx, ny);
    lon.resize(nx, ny);

    lon.binin(fina); padding.binin(fina);
    lat.binin(fina); padding.binin(fina);

    fclose (fina);

    FILE *fout1, *fout2;
    fout1 = fopen(argv[3], "w");
    fout2 = fopen(argv[4], "w");
    lon.printer(fout1);
    lat.printer(fout2);
    fclose(fout1);
    fclose(fout2);

    return 0;

}
