#include "ncepgrids.h"
#include "time_series.h"

//#define MAXPTS  57817
#define MAXPTS  54221
#define MAXDAYS 10957 

float dot(mvector<float> &x, mvector<float> &y) ;

int main(int argc, char *argv[]) {
  time_series<float> conc[MAXPTS];
  FILE *fin;
  global_ice<float> concin;
  mvector<float> avg(MAXPTS), count(MAXPTS), sd(MAXPTS), norm(MAXPTS);
  mvector<int> index(MAXPTS);
  mvector<ijpt> loc(MAXPTS);
  mvector<latpt> ll(MAXPTS);
  int i, j, ti, tj;
  int countin, indexin;
  float ain, sdin;
  float lat, lon, d1, f1, f2, f3;
  ijpt tloc;
  latpt tll;
  

// Size the vectors for the number of days
  for (i = 0; i < MAXPTS; i++) {
    conc[i].resize(MAXDAYS);
  }

// Scan the summary file for points 
  fin = fopen("allice.summary","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open allice.summary\n");
    return 1;
  }
  for (i = 0; i < MAXPTS; i++) {
    fscanf(fin, "%d %d %d %f %f %d %f %f %f %f %f %f\n",&indexin, &ti, &tj, &lat, &lon, &countin, &ain, &d1, &sdin, &f1, &f2, &f3);
    index[i] = indexin;
    tloc.i = ti; tloc.j = tj;
    loc[i] = tloc;
    tll.lat = lat; tll.lon = lon;
    ll[i]   = tll;
    count[i] = (float) countin;
    avg[i] = ain;
    sd[i]  = sqrt(sdin); 
  } 
  fclose(fin);

// now run through the input files, ice.YYYYMMDD and distribute to the time series 
  for (i = 0; i < min(MAXDAYS,argc-1); i++) {
    fin = fopen(argv[i+1],"r");
    concin.binin(fin);
    fclose(fin);
    if (concin.gridmax() > 3.0 ) concin /= 100.;
    for (j = 0; j < MAXPTS; j++) {
      conc[j][i] = concin[index[j] ];
    }
  }

// Debugging:
//  for (i = 0; i < MAXDAYS; i++) {
//    printf("%d %f\n",i,conc[0][i]);
//  }
//  conc[0] -= conc[0].average();
//  for (i = 0; i < MAXDAYS; i++) {
//    printf("%d %f\n",i,conc[0][i]);
//  }
//  printf("rms = %f dot = %f  %f\n",conc[0].rms(), dot(conc[0],conc[0]), 
//             sqrt(dot(conc[0],conc[0]))                             );
//  conc[0] /= sqrt(dot(conc[0],conc[0]));
//  printf("rms = %f dot = %f  %f\n",conc[0].rms(), dot(conc[0],conc[0]), 
//             sqrt(dot(conc[0],conc[0]))                             );


// remove mean by computation (vs. the input) and find sd (sqrt(var))
  double varsum = 0.0, asum = 0.0;
  for (j = 0; j < MAXPTS; j++) {
    avg[j]   = conc[j].average();
    conc[j] -= avg[j];
    norm[j]  = sqrt(dot(conc[j],conc[j])) ;
    //printf("first %d %f %f  %f %f\n",j, avg[j], conc[j].average(), conc[j].rms(), 
    //                     norm[j] );
    if (norm[j] != 0.0) {
      conc[j]  /= norm[j];
    }
    else {
      conc[j] = 0.0;
    }
    sd[j]     = conc[j].rms();
    asum   += concin.cellarea(loc[j]);
    varsum += sd[j]*sd[j]*concin.cellarea(loc[j]);
  }
  //printf("var before removal %e %e %e\n",varsum, asum, varsum/asum);
  //printf("sd max, min, average, rms %f %f %f %f\n",sd.maximum(), sd.minimum(), sd.average(), sd.rms() );
  //for (j = 0; j < MAXPTS; j++) {
  //  printf("sd %d %e %e\n",j, sd[j], norm[j]);
  //}

  double varsum2 = 0.0;
  int k, maxvecs = 9000, out_index = 0;
  mvector<float> figure_of_merit(MAXPTS), r(MAXPTS);
  FILE *fout;
  char fname[900];


  fout = (FILE*) NULL;
  figure_of_merit = 0.0;
  for (k = 0; k < MAXPTS; k++) {

    // need to do this because of 2 Gb file limit on desk
    if ((k % maxvecs) == 0) {
      if (fout != (FILE*) NULL) {
        fclose(fout);
        sprintf(fname,"correlout.%1d",out_index);
        fout = fopen(fname,"w");
        out_index += 1;
      }
      else {
        sprintf(fname,"correlout.%1d",out_index);
        fout = fopen(fname,"w");
        out_index += 1;
      }
    }
     
    
    varsum2 = 0.0;
    //if (count[k] < 1000) continue;
    for (j = 0; j < MAXPTS; j++) {
      r[j] = dot(conc[k], conc[j]); // vectors are now normalized
      varsum2 += (1-r[j]*r[j])*sd[j]*sd[j]*concin.cellarea(loc[j]);
      //printf("corr %d %d  %f %f\n",k, j, r, r*r);
    }
    r.binout(fout);

    printf("var  after removal %d %3d %3d %e %e %e  %f\n",k, 
             loc[k].i, loc[k].j, varsum2, asum, varsum2/asum, 
                   (varsum-varsum2)/varsum);
    fflush(stdout);
    figure_of_merit[k] = (varsum-varsum2)/varsum;

  }
  fclose(fout);


  for (i = 0; i < MAXPTS; i++) {
    printf("%5d %6d %7.2f %7.2f %f\n",i, index[i], ll[i].lat, ll[i].lon, figure_of_merit[i]);
  } 

// write out the vectors so that next time(s) can just read in directly, vs. 
// scanning full set


  return 0;
}
float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0;
  
  for (int i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return (float) sum;
}
