#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_quarter<short int> qdoi;
  global_quarter<float> sst_qd;

  global_12th<unsigned char> rtghr;
  global_12th<float> sst_final, sst_rtg;

  ijpt loc1, loc2;
  latpt ll;
  FILE *fin1, *fin2;
  int rtgland = 157, qdoiland = 0;
  int rtgwater = 0, qdoiwater = 1;

  fin1 = fopen(argv[1],"r");
  qdoi.binin(fin1);
  fclose(fin1);

  fin2 = fopen(argv[2],"r");
  rtghr.binin(fin2);
  fclose(fin2);

  fin1 = fopen(argv[3],"r");
  sst_qd.binin(fin1);
  fclose(fin1);
  printf("qd max min %f %f\n",sst_qd.gridmax(), sst_qd.gridmin() );
  
  fin2 = fopen(argv[4],"r");
  sst_rtg.binin(fin2);
  fclose(fin2);
  printf("rtg max min %f %f\n",sst_rtg.gridmax(), sst_rtg.gridmin() );
  
// scale to centigrees, as is qdoi:
  if (sst_rtg.gridmax() < 39) { 
     sst_rtg *= 100.;
  } 
  else if (sst_rtg.gridmax() > 200 && sst_rtg.gridmax() < 320) {
     sst_rtg -= 273.15;
     sst_rtg *= 100.;
  }

  if (sst_qd.gridmax()  < 39) sst_qd  *= 100.;

  for (loc1.j = 0; loc1.j < rtghr.ypoints(); loc1.j++) {
  for (loc1.i = 0; loc1.i < rtghr.xpoints(); loc1.i++) {

    ll = rtghr.locate(loc1);
    loc2 = qdoi.locate(ll);

    if (qdoi[loc2] == qdoiwater) {
      sst_final[loc1] = sst_qd[loc2];
    }
    else if (rtghr[loc1] != rtgland) {
      sst_final[loc1] = sst_rtg[loc1];
    }
    else {
      //printf("weaver %4d %4d  %7.2f %7.2f  %7.2f %3d\n",loc1.i, loc1.j, ll.lat, 
      //          ll.lon, sst_rtg[loc1], rtghr[loc1] );
      sst_final[loc1] = sst_rtg[loc1];
    }

  }
  }

  fin2 = fopen(argv[5],"w");
  sst_final.binout(fin2);
  fclose(fin2);
  printf("final grid max min %f %f\n",sst_final.gridmax(), sst_final.gridmin() );

  fin1 = fopen("printer","w");
  for (loc1.j = 0; loc1.j < rtghr.ypoints(); loc1.j++) {
  for (loc1.i = 0; loc1.i < rtghr.xpoints(); loc1.i++) {
    fprintf(fin1, "%4d %4d %7.2f\n",loc1.i, loc1.j, sst_final[loc1]) ;
  }
  }  
  //sst_final.printer(fin1);
  fclose(fin1);

  return 0;
}
