#include "ncepgrids.h"

#define NDAYS 17
#define NFCSTS 4
void scoring(GRIDTYPE<float> &obs, GRIDTYPE<float> &model, GRIDTYPE<float> &use, 
             GRIDTYPE<float> &areas, float &mean, float &rms, 
             double &a11, double &a12, double &a21, double &a22,
             float &pod, float &far, float &correct) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout[NDAYS];
  GRIDTYPE<unsigned char> skip;
  GRIDTYPE<float> obsd, use, areas;
  GRIDTYPE<float> fcst1, fcst2, fcst3, fcst4;

// For scores
  float mean, rms, pod, far, correct;
  double a11, a12, a21, a22;
  char fname[900];

// misc:
  int lead, nm;
  ijpt loc;

///////////////////////////////////////////////////////////////////////////////////////
// Read data 
  fin = fopen(argv[1],"r");
  skip.binin(fin);
  fclose(fin);

// notion -- this permits multiplying grid by 'use' and having nonzero 
//           only where it should be 'used'
  conv(skip, obsd);
  use.set((float) 1.0);  use -= obsd;
  for (loc.j = 0; loc.j < use.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < use.xpoints(); loc.i++) {
     if (use[loc] == 1.) {
       areas[loc] = use.cellarea(loc);
     }
     else {
       areas[loc] = 0;
     }
  }
  }
  //printf("skip max min avg %d %d %d\n",skip.gridmax(), skip.gridmin(), skip.average() );
  //fflush(stdout);
  //printf("use max min avg %f %f %f\n",use.gridmax(), use.gridmin(), use.average() );
  //fflush(stdout);

// Get true observation
  fin = fopen(argv[2], "r");
  obsd.binin(fin);
  fclose(fin);

  for (lead = 0; lead < NDAYS; lead++) {
    fin = fopen(argv[3+lead], "r");
    fcst1.binin(fin);
    fcst2.binin(fin);
    fcst3.binin(fin);
    fcst4.binin(fin);
    fclose(fin);

    nm = 1;
    sprintf(fname,"lead%dmodel%d",lead,nm);
    fout[lead] = fopen(fname, "a+");
    scoring(obsd, fcst1, use, areas, mean, rms, a11, a12, a21, a22, pod, far, correct);
    fprintf(fout[lead], "%5.2f %5.2f  %5.2f %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22, pod, far, correct);
    fclose(fout[lead]);

    nm = 2;
    sprintf(fname,"lead%dmodel%d",lead,nm);
    fout[lead] = fopen(fname, "a+");
    scoring(obsd, fcst2, use, areas, mean, rms, a11, a12, a21, a22, pod, far, correct);
    fprintf(fout[lead], "%5.2f %5.2f  %5.2f %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22, pod, far, correct);
    fclose(fout[lead]);

    nm = 3;
    sprintf(fname,"lead%dmodel%d",lead,nm);
    fout[lead] = fopen(fname, "a+");
    scoring(obsd, fcst3, use, areas, mean, rms, a11, a12, a21, a22, pod, far, correct);
    fprintf(fout[lead], "%5.2f %5.2f  %5.2f %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22, pod, far, correct);
    fclose(fout[lead]);

    nm = 4;
    sprintf(fname,"lead%dmodel%d",lead,nm);
    fout[lead] = fopen(fname, "a+");
    scoring(obsd, fcst4, use, areas, mean, rms, a11, a12, a21, a22, pod, far, correct);
    fprintf(fout[lead], "%5.2f %5.2f  %5.2f %5.2f %5.2f %5.2f  %5.2f %5.2f %5.2f\n",mean, rms, (float) a11,(float)  a12,(float)  a21,(float)  a22, pod, far, correct);
    fclose(fout[lead]);
       
  }

  return 0;
}
void scoring(GRIDTYPE<float> &obs, GRIDTYPE<float> &model, GRIDTYPE<float> &use, 
             GRIDTYPE<float> &areas, float &mean, float &rms, 
             double &a11, double &a12, double &a21, double &a22,
             float &pod, float &far, float &correct) {
  int i;
  double globe = 0.0, level = 0.0;

  for (i = 0; i < model.xpoints()*model.ypoints(); i++) {
    globe += areas[i]; // areas already = 0 for use = 0
  }

  mean = 0.0;
  rms  = 0.0;
  for (i = 0; i < model.xpoints()*model.ypoints(); i++) {
    mean += (obs[i]-model[i])*areas[i];
    rms  += (obs[i]-model[i])*(obs[i]-model[i])*areas[i];
  }
  mean /= globe;
  rms   = sqrt(rms/globe);

  a11 = 0; a12 = 0; a21 = 0; a22 = 0;
  for (i = 0; i < model.xpoints()*model.ypoints(); i++) {
  if (use[i] == 1.) {
  //if (areas[i] != 0.) { -- this is slower than test of use == 1
    if (model[i] > level ) {
      if (obs[i] > level ) {
        a11 += areas[i];
      }
      else {
        a12 += areas[i];
      }
    }
    else {
      if (obs[i] > level ) {
        a21 += areas[i];
      }
      else {
        a22 += areas[i];
      }
    }
  }
  }

  a11 /= globe;
  a12 /= globe;
  a21 /= globe;
  a22 /= globe;

// pod = probability of detection (said there was ice, and there is)
// far = false alarm rate (said there was ice, but there wasn't)
  pod = (double) a11 / (double) (a11 + a21);
  far = (double) a12 / (double) (a11 + a12);
  correct = (a11 + a22) / (a11 + a12 + a21 + a22);

  return;
}
