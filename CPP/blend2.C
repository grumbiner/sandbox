#include "ncepgrids.h"

// Program to blend multiple source grids, for each hemisphere, on to a unified
//   target grid.
// 30 March 2012: Extend to having 3 inputs -- AMSR, SSMI, SSMI-S -- per hemiphere.

void blend(metricgrid<unsigned char> &namsr, metricgrid<unsigned char> &nssmi, 
           metricgrid<unsigned char> &nssmis, metricgrid<unsigned char> &nout) ;
void blend2(metricgrid<unsigned char> &namsr, metricgrid<unsigned char> &nssmi, 
           metricgrid<unsigned char> &nssmis, metricgrid<unsigned char> &nout) ;
void stats(metricgrid<unsigned char> &n, metricgrid<unsigned char> &n2,
           metricgrid<unsigned char> &land) ;

bool somedata(grid2<unsigned char> &x) ;
void splice(global_12th<float> &noice, global_12th<float> &imsice, metricgrid<unsigned char> &x);

int main(int argc, char *argv[]) {
  northhigh<unsigned char> namsr, nout, nout2, nland;
  northhigh<unsigned char> nssmi;
  northhigh<unsigned char> nssmis;
  southhigh<unsigned char> samsr, sout, sout2, sland;
  southhigh<unsigned char> sssmi;
  southhigh<unsigned char> sssmis;
  global_12th<float> noice, imsice;
  FILE *fin, *nfout, *sfout;

// Read in the hemispheric grids
  fin = fopen(argv[1],"r");
    namsr.set((unsigned char) NO_DATA);
  if (fin == (FILE *) NULL) {
    printf("failed to open namsr\n"); fflush(stdout);
    namsr.set(NO_DATA);
  }
  else {
    namsr.binin(fin);
    fclose(fin);
  }
  printf("past namsr\n"); fflush(stdout);

  fin = fopen(argv[2],"r");
  nssmi.set((unsigned char) NO_DATA);
  if (fin == (FILE *) NULL) {
    printf("failed to open nssmi\n"); fflush(stdout);
  }
  else {
    nssmi.binin(fin);
    fclose(fin);
  }

  fin = fopen(argv[3],"r");
  nssmis.set((unsigned char) NO_DATA); fflush(stdout);
  if (fin == (FILE *) NULL) {
    printf("failed to open nssmis\n"); fflush(stdout);
  }
  else {
    nssmis.binin(fin);
    fclose(fin);
  }

  nfout = fopen(argv[4],"w");
  if (nfout == (FILE *) NULL) {
    printf("Failed to open the northern hemisphere file %s for writing!\n",argv[4]);
    return 1;
  }

  samsr.set((unsigned char) NO_DATA);
  fin = fopen(argv[5],"r");
  if (fin == (FILE *) NULL) {
    printf("failed to open samsr\n"); fflush(stdout);
  }
  else {
    samsr.binin(fin);
    fclose(fin);
  }
  printf("past samsr\n"); fflush(stdout);

  sssmi.set((unsigned char) NO_DATA);
  fin = fopen(argv[6],"r");
  if (fin == (FILE *) NULL) {
    printf("failed to open sssmi\n"); fflush(stdout);
  }
  else {
    sssmi.binin(fin);
    fclose(fin);
  }

  sssmis.set((unsigned char) NO_DATA);
  fin = fopen(argv[7],"r");
  if (fin == (FILE *) NULL) {
    printf("failed to open sssmis\n"); fflush(stdout);
  }
  else {
    sssmis.binin(fin);
    fclose(fin);
  }

  sfout = fopen(argv[8],"w");
  if (sfout == (FILE *) NULL) {
    printf("Failed to open the southern hemisphere file %s for writing!\n",argv[8]);
    return 1;
  }

  noice.set((float) NO_DATA/100.0);
  imsice.set((float) NO_DATA/100.0);
  fin = fopen(argv[11], "r");
  if (fin == (FILE *) NULL) {
    printf("failed to open noice file\n"); fflush(stdout);
  }
  else {
    noice.binin(fin);
    fclose(fin);
  }
  fin = fopen(argv[12], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open imsice file\n"); fflush(stdout);
  }
  else {
    imsice.binin(fin);
    fclose(fin);
  }

// now have all inputs
  blend(namsr, nssmi, nssmis, nout);
  blend2(namsr, nssmi, nssmis, nout2);
  if (!somedata(nout) ) {
    splice(noice, imsice, nout);
  }

  blend(samsr, sssmi, sssmis, sout);
  blend2(samsr, sssmi, sssmis, sout2);
  if (!somedata(sout) ) {
    splice(noice, imsice, sout);
  }

  nout.binout(nfout);
  sout.binout(sfout);
  fclose(nfout);
  fclose(sfout);

  fin = fopen(argv[9], "r");
  if (fin == (FILE *) NULL) {
    printf("failed to open nland\n");
    return 1;
  }
  nland.binin(fin);
  fclose(fin);
  fin = fopen(argv[10], "r");
  if (fin == (FILE *) NULL) {
    printf("failed to open sland\n");
    return 1;
  }
  sland.binin(fin);
 
  printf("stats for north intercomparison\n");
  stats(nout, nout2, nland);
  printf("stats for south intercomparison\n");
  stats(sout, sout2, sland);

  return 0;
}

void blend(metricgrid<unsigned char> &namsr, metricgrid<unsigned char> &nssmi, 
           metricgrid<unsigned char> &nssmis, metricgrid<unsigned char> &nout) {
  ijpt loc;
  int c, tmp;

  for (loc.j = 0; loc.j < nout.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nout.xpoints(); loc.i++) {
    // Pre-bound:
    if (namsr[loc] > 100 && namsr[loc] <= 128) namsr[loc] = 100;
    if (nssmi[loc] > 100 && nssmi[loc] <= 128) nssmi[loc] = 100;
    if (nssmis[loc] > 100 && nssmis[loc] <= 128) nssmis[loc] = 100;

    tmp = 0; c = 0;

    if (namsr[loc] <= 100) {
      c++; tmp += namsr[loc];
    }
    if (nssmi[loc] <= 100) {
      c++; tmp += nssmi[loc];
    }
    if (nssmis[loc] <= 100) {
      c++; tmp += nssmis[loc];
    }


    if (c != 0) {
      nout[loc] = (int) (((float) tmp / (float) c) + 0.5 );
    }
    else {
      nout[loc] = NO_DATA;
    }

// Ensure properly bounded from below:
    if (nout[loc] < MIN_CONC) nout[loc] = 0;
  }
  }

  return;
}

// Do a blending in sequence of guessed better to worser instruments:
void blend2(metricgrid<unsigned char> &namsr, metricgrid<unsigned char> &nssmi, 
            metricgrid<unsigned char> &nssmis, metricgrid<unsigned char> &nout) {
  ijpt loc;

  for (loc.j = 0; loc.j < nout.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < nout.xpoints(); loc.i++) {
    if (namsr[loc] <= 100) {
      nout[loc] = namsr[loc];
    }
    else if (nssmi[loc] <= 100 ) {
      nout[loc] = nssmi[loc];
    }
    else if (nssmis[loc] <= 100 ) {
      nout[loc] = nssmis[loc];
    }
    else {
      nout[loc] = NO_DATA;
    }
  }
  }

  return;
}
void stats(metricgrid<unsigned char> &n, metricgrid<unsigned char> &n2, metricgrid<unsigned char> &land) {
  ijpt loc;
  double sum = 0, sumsq = 0;
  int count = 0;
  
  for (loc.j = 0; loc.j < n.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < n.xpoints(); loc.i++) {
    if ((n[loc] != n2[loc]) && (land[loc] == 0) ) {
      count += 1;
      sum += (n[loc] - n2[loc]);
      sumsq += (n[loc] - n2[loc])*(n[loc] - n2[loc]);
    }
  }
  }
  printf("count %d mean, rms difference %f %f\n",count, sum/count, sqrt(sumsq/count) );

  return;
}
bool somedata(grid2<unsigned char> &x) {
  int i;
  for (i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] != NO_DATA) return true;
  }
  return false;
} 
void splice(global_12th<float> &noice, global_12th<float> &imsice, 
            metricgrid<unsigned char> &x) {
  ijpt loc, tloc;
  latpt ll;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] == NO_DATA) {
      ll = x.locate(loc);
      tloc = imsice.locate(ll);
      if (imsice[tloc] <= 1.00) {
        x[loc] = (int) (0.5 + 100.*imsice[tloc]);
      }
      else if (noice[tloc] <= 1.00) {
        x[loc] = (int) (0.5 + 100.*noice[tloc]);
      }
    }
  }
  }

  return;
}
