#include "ncepgrids.h"

#include "scoring_subs.C"
// experiment with the effect of resolution (reduction) on the analysis
// scores.


void reduce(global_12th<float> &hires, llgrid<float> &conc, int ratio, global_12th<unsigned char> &skip) ;

// do the verification on the analysis grid:
void scoring(global_12th<float> &obs, global_12th<float> &model, 
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) ;

void scoring(global_12th<float> &obs, global_12th<float> &model, global_12th<unsigned char> &skip,
              float &mean, float &rms, double &a11, double &a12, double &a21, double &a22,
              float &pod, float &far, float &correct) ;

int main(int argc, char *argv[]) {
  FILE *fobs, *fskip;
  global_12th<float> obsd, model;
  llgrid<float> *reduced;
  global_12th<unsigned char> skip;

  float mean, rms, pod, far, correct;
  double a11, a12, a21, a22;

  latpt ll;
  ijpt loc;
  int i;

// Get the observed ice concentrations:
  fobs = fopen(argv[1], "r");
  if (fobs == (FILE *) NULL) {
    printf("failed to open observation input %s\n",argv[1]);
    return 1;
  }
  obsd.binin(fobs);
  fclose(fobs);

// Read in the file of flagged points to skip:
  fskip = fopen(argv[2], "r");
  if (fskip == (FILE *) NULL) {
    printf("failed to open skipfile input %s\n",argv[3]);
    return 1;
  }
  skip.binin(fskip);
  fclose(fskip);
// Pre-skip' the points through the observation grid:
  preskip(obsd, skip);

  int resolution;
  FILE *resin;
  float landval = 9.9e9, nonval = 0.0;

  resin = fopen(argv[3],"r");
  while (!feof(resin) ) {
    fscanf(resin,"%d",&resolution);
    reduced = new llgrid<float>(model.xpoints() / resolution, model.ypoints()/resolution,
              model.dlat * resolution, model.dlon*resolution, 
              90 + model.dlat*resolution/2.0, model.firstlon*resolution);
    reduce(obsd, *reduced, resolution, skip);
    if (resolution == 1) {
      for (loc.j = 0; loc.j < model.ypoints(); loc.j++) {
      for (loc.i = 0; loc.i < model.xpoints(); loc.i++) {
        if ( (obsd[loc] -  reduced->operator[](loc)) != 0) {
          ll = obsd.locate(loc);
          printf("%4d %4d  %7.2f %7.2f  %1d %5.2f %5.2f  %6.2f\n",loc.i, loc.j, 
               ll.lat, ll.lon, skip[loc], obsd[loc], 
               reduced->operator[](loc), obsd[loc] - reduced->operator[](loc) );
        }
      }
      }
    }

    // interpolate from model grid to analysis grid:
    model.fromall(*reduced, landval, nonval);
  
    // Score for whole globe:
    scoring(obsd, model, mean, rms, a11, a12, a21, a22, pod, far, correct);
    printf("res %02d fcst scores %f %f  %f %f %f %f  %f %f %f  ",resolution, mean, rms, (float) a11, (float) a12, (float) a21, (float) a22, pod, far, correct);
  
    // Score only points not flagged to be 'skipped'
    scoring(obsd, model, skip, mean, rms, a11, a12, a21, a22, pod, far, correct);
    printf(" %f %f  %f %f %f %f  %f %f %f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22, pod, far, correct);
    fflush(stdout);

  }

  return 0;
}
void reduce(global_12th<float> &hires, llgrid<float> &conc, int ratio, global_12th<unsigned char> &skip) {
  ijpt loc, lowloc;
  llgrid<int> *count;
  latpt ll;

  count = new llgrid<int>(conc.xpoints(), conc.ypoints(), conc.dlat, conc.dlon, 
                            conc.firstlat, conc.firstlon);
  
  count->set(0);
  conc.set((float) 0.0);

  //printf("conc.xpoints, ypoints = %d %d ratio = %d\n",
  //  conc.xpoints(), conc.ypoints(), ratio); fflush(stdout);
  for (loc.j = 0; loc.j < hires.ypoints(); loc.j++) {
    lowloc.j = loc.j / ratio;
  for (loc.i = 0; loc.i < hires.xpoints(); loc.i++) {
    lowloc.i = loc.i / ratio;
    
    if (skip[loc] == 0) {
      conc[lowloc] += hires[loc]; 
      count->operator[](lowloc) += 1;
    }
    if (skip[loc] != 0 && hires[loc] != 0) {
      //ll = skip.locate(loc);
      //printf("unskip %4d %4d  %1d %7.2f %7.2f  %5.2f\n",loc.i, loc.j, skip[loc], ll.lat, ll.lon, hires[loc]);
      hires[loc] = 0.0; // insurance against failures in preskip
    }
  }
  }

  for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
    if (count->operator[](loc) != 0) conc[loc] /= count->operator[](loc);
    if (conc[loc] < 0.15) conc[loc] = 0.;
  }
  }
  //printf("count max min avg %d %d %d\n",count->gridmax(), count->gridmin(), count->average() );
  //printf("conc max min avg %f %f %f\n",conc.gridmax(), conc.gridmin(), conc.average() );
  //printf("hires max min avg %f %f %f\n",hires.gridmax(), hires.gridmin(), hires.average() );


  return;
}
