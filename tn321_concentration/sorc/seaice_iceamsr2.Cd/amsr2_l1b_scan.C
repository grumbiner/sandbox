#include <cstdio>
#include <cstdlib>
#include <cmath>

#include "ncepgrids.h"
#include "amsr2.h"

#define land 0
void scanner(FILE *fin, FILE *fout_lr, FILE *fout_hr, global_12th<unsigned char> &icec) ;
void headprint(amsr2head &head) ;
void lrprint(amsr2_lrpt &x, global_12th<unsigned char> &icec) ;
void hrprint(amsr2_hrpt &x, global_12th<unsigned char> &icec) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout_lr, *fout_hr, *fin_icec;
  global_12th<unsigned char> icec;

// Open data files:
  fin = fopen(argv[1], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open input satellite data file %s\n",argv[1]);
    return 1;
  }
  fout_lr = fopen(argv[2],"w");
  if (fout_lr == (FILE*) NULL) {
    printf("failed to open lr_output satellite data file %s\n",argv[2]);
    return 1;
  }
  fout_hr = fopen(argv[3],"w");
  if (fout_hr == (FILE*) NULL) {
    printf("failed to open hr_output satellite data file %s\n",argv[3]);
    return 1;
  }
  fin_icec = fopen(argv[4],"r");
  if (fin_icec == (FILE*) NULL) {
    printf("failed to open icec input file, continuing without %s\n",argv[4]);
    icec.set(224);
  }
  else {
    icec.binin(fin_icec);
    fclose(fin_icec);
  }

// Now read (scan) the input files for data to work with 
  //debug: printf("about to call scanner\n"); fflush(stdout);
  scanner(fin, fout_lr, fout_hr, icec);

  fclose(fin);

  return 0;
}

void scanner(FILE *fin, FILE *fout_lr, FILE *fout_hr, global_12th<unsigned char> &icec) {
  amsr2head  x;
  amsr2_hrpt hr;
  amsr2_lrpt lr;
  amsr2_spot spot[12];

  int i, nobs, gridskip = 0, nuse = 0, nread = 0;

  //debug: printf("entered scanner\n"); fflush(stdout);

  rewind(fin);
  while (!feof(fin)) {
    fread(&x, sizeof(x), 1, fin);
    nobs = x.nspots;
    //printf("nobs = %d\n",nobs);

    fread(&spot[0], sizeof(amsr2_spot), nobs, fin);
    if (feof(fin)) continue;
    if (x.clat < 25 && x.clat > -40.0) continue;

    nread += 1;

    if (nobs == 2) {
      hr.head = x;
      for (i = 0; i < nobs; i++) { hr.obs[i] = spot[i]; }
    }
    else {
      lr.head = x;
      for (i = 0; i < nobs; i++) { lr.obs[i] = spot[i]; }
    }

    if (x.clat >= 40.0 && x.clat <= 60.0 ) {
      nuse += 1;
      // do something useful
      if (nobs == 2) {
        //hrgrid[loc] = hr;
        fwrite(&x, sizeof(x), 1, fout_hr);
        fwrite(&spot[0], sizeof(amsr2_spot), nobs, fout_hr);
	hrprint(hr, icec);
      }
      else {
        //lrgrid[loc] = lr;
        fwrite(&x, sizeof(x), 1, fout_lr);
        fwrite(&spot[0], sizeof(amsr2_spot), nobs, fout_lr);
	lrprint(lr, icec);
      }
    }
  }

  printf("scanner nread = %d nuse = %d \n",nread, nuse);
  fflush(stdout);

  return ;
}
void headprint(amsr2head &head) {
  printf("%3d %9.5f %10.5f ",head.satid, head.clat, head.clon);
}
void lrprint(amsr2_lrpt &x, global_12th<unsigned char> &icec) {
  latpt ll;
  fijpt floc;
  ijpt  loc;
  ll.lat = x.head.clat;
  ll.lon = x.head.clon;
  floc = icec.locate(ll);
  loc.i = rint(floc.i);
  loc.j = rint(floc.j);
  printf("%3d lr ",icec[loc]);
  headprint(x.head);
  //printf("%5.1f %4.2f  %6.2f ",x.obs[0].sccf/10., x.obs[0].alfr, x.obs[0].tmbr);
  printf("%4.2f  %6.2f ", x.obs[0].alfr, x.obs[0].tmbr);
  for (int i = 1; i < 12; i++) {
    printf("%6.2f ",x.obs[i].tmbr);
  }
  printf("\n");
  return;
}

void hrprint(amsr2_hrpt &x, global_12th<unsigned char> &icec) {
  latpt ll;
  fijpt floc;
  ijpt  loc;
  ll.lat = x.head.clat;
  ll.lon = x.head.clon;
  floc = icec.locate(ll);
  loc.i = rint(floc.i);
  loc.j = rint(floc.j);

  printf("%3d hr ",icec[loc]);
  headprint(x.head);
  printf("%4.2f  %6.2f ", x.obs[0].alfr, x.obs[0].tmbr);
  printf("%4.2f  %6.2f ", x.obs[1].alfr, x.obs[1].tmbr);
  //printf("%5.1f %4.2f  %6.2f  ",x.obs[0].sccf/10., x.obs[0].alfr, x.obs[0].tmbr);
  //printf("%5.1f %4.2f  %6.2f  ",x.obs[1].sccf/10., x.obs[1].alfr, x.obs[1].tmbr);

  printf("\n");
  return;
}
