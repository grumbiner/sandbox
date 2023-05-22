#include "ncepgrids.h"

// Analyze model contents from the restart file

void checker(metricgrid<float> &h, char *field, char *param, ijpt &sp) ;

int main(int argc, char *argv[]) {
  northgrid<float> h, a, hsn, qt, qs, qh, qtb, qsb, qhb, qdt, qds;
  northgrid<float> tice, ticm[7]; 
  char field[900], parameter[900];

  FILE *fin;
  int i;
  ijpt sploc;

  sploc.i = 384;
  sploc.j = 438;

  fin = fopen (argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open input file %s\n", argv[1]);
    return 1;
  }
  h.ftnin(fin);
  a.ftnin(fin);
  hsn.ftnin(fin);
  tice.ftnin(fin);
  qt.ftnin(fin);
  qs.ftnin(fin);
  qh.ftnin(fin);
  qtb.ftnin(fin);
  qsb.ftnin(fin);
  qhb.ftnin(fin);
  qdt.ftnin(fin);
  qds.ftnin(fin);
  for (i = 0; i < 7; i++) {
    ticm[i].ftnin(fin);
  }

// Thickness
  sprintf(field, "Ice thickness information");
  sprintf(parameter, "h");
  checker(h, field, parameter, sploc);

// Concentration
  sprintf(field, "Ice Concentration information");
  sprintf(parameter, "A");
  checker(a, field, parameter, sploc);

// snow Thickness
  sprintf(field, "snow thickness information");
  sprintf(parameter, "hsn");
  checker(hsn, field, parameter, sploc);

// ice skin temperature
  sprintf(field, "ice skin temperature");
  sprintf(parameter, "tice");
  checker(tice, field, parameter, sploc);
// Ice column temperatures:
  for (i = 0; i < 7; i++) {
    sprintf(field, "ice level temp %1d",i);
    sprintf(parameter,"ticm[%1d]",i);
    checker(ticm[i], field, parameter, sploc);
  }

// Mixed layer depth:
  sprintf(field, "mixed layer depth");
  sprintf(parameter, "hml");
  checker(qh, field, parameter, sploc);
  sprintf(field, "qhb");
  sprintf(parameter, "qhb");
  checker(qhb, field, parameter, sploc);

//  northgrid<float> qt, qs, qtb, qsb, qdt, qds;
//
  sprintf(field, "qs"); sprintf(parameter, "qs");
  checker(qs, field, parameter, sploc);
  sprintf(field, "qsb"); sprintf(parameter, "qsb");
  checker(qsb, field, parameter, sploc);
  sprintf(field, "qds"); sprintf(parameter, "qds");
  checker(qds, field, parameter, sploc);

  sprintf(field, "qt"); sprintf(parameter, "qt");
  checker(qt, field, parameter, sploc);
  sprintf(field, "qtb"); sprintf(parameter, "qtb");
  checker(qtb, field, parameter, sploc);
  sprintf(field, "qdt"); sprintf(parameter, "qdt");
  checker(qdt, field, parameter, sploc);

  return 0;
} 

void checker(metricgrid<float> &h, char *field, char *param, ijpt &sp) {
  ijpt loc;
  latpt ll;

  printf("%s\n",field);
  printf("max, min, average, rms %f %f %f %f  %f\n",h.gridmax(), h.gridmin(),
      h.average(0.), h.rms(0.), h[sp]  );
  for (loc.j = 0; loc.j < h.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < h.xpoints(); loc.i++) {
     if (! finite(h[loc]) ) {
       ll = h.locate(loc);
       printf("Found nan in %s at %d %d %f %f  %f\n",param, loc.i, loc.j, 
             ll.lat, ll.lon, h[loc]);
     }
  }
  }

} 
