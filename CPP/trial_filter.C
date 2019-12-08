#include <stdio.h>

#define MAXOBS (10*1000*1000)
#define NFREQS 8

#include "ncepgrids.h"
#include "mvector.h"

// Demonstrate filter:
int acquire(mvector<float> &lat, mvector<float> &lon, mvector<float> *tb, int &count) ;

int main(int argc, char *argv[]) {
  mvector<float> lat(MAXOBS), lon(MAXOBS);
  mvector<float> tb[NFREQS];
  int ret, count;
  float thot[NFREQS] = {275., 260., 270., 268., 260., 268., 272., 271.} ; 
  float tcold[NFREQS] = {155., 50., 175., 180., 130., 172., 170., 160.};

  int i;
/////
  ret = acquire(lat, lon, tb, count);
// p(ti > 273.15) -- may do by counts

  FILE *fin;
  global_12th<float> icec;
  global_12th<unsigned char> land;
  global_12th<float> distance;

  fin = fopen("icec", "r");
  icec.binin(fin);
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  land.binin(fin);
  fclose(fin);
  fin = fopen("seaice_alldist.bin","r");
  distance.binin(fin);
  fclose(fin);

  latpt ll;
  fijpt floc;
  ijpt  loc;
  int j, ncoast = 0, nwater = 0, nice = 0, nland = 0;
  int nhot[NFREQS], nhotwater[NFREQS], nhotland[NFREQS], nhotcoast[NFREQS], nhotice[NFREQS];
  int ncold[NFREQS], ncoldwater[NFREQS], ncoldland[NFREQS], ncoldcoast[NFREQS], ncoldice[NFREQS];

  for (j = 0; j < NFREQS; j++) { 
    nhot[j] = 0; 
    nhotwater[j] = 0;
    nhotland[j] = 0;
    nhotcoast[j] = 0;
    nhotice[j] = 0;
    ncold[j] = 0; 
    ncoldwater[j] = 0;
    ncoldland[j] = 0;
    ncoldcoast[j] = 0;
    ncoldice[j] = 0;
  }

  int nused = 0, nfiltered = 0;
  bool hot = false, cold = false, is_land = false, mixed = false;
  FILE *foutland, *foutmixed, *foutcold;

  foutland = fopen("landout","w");
  foutmixed = fopen("mixedout","w");
  foutcold = fopen("coldout","w");

  for (i = 0; i < count; i++) {
    if (lat[i] < 24.0 && lat[i] > -45.0) continue; // skip the 'tropics'
    nused++;
    ll.lat = lat[i];
    ll.lon = lon[i];
    floc = icec.locate(ll);

    if (land[floc] == 195) {
      ncoast++;
    }
    if (land[floc] == 157) {
      nland++;
    }
    if (land[floc] == 0) {
      nwater++;
    }
    if (icec[floc] > 0 && icec[floc] < 1.28) {
      nice++;
    }

    hot = false;
    cold = false;
    for (j = 0; j < NFREQS; j++) {
      if (tb[j][i] > thot[j]) {
        nhot[j]++;
        hot = true;
        if (j == 1 || j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
          is_land = true;
        }
        else {
          mixed = true;
        }
        
      }
      if (tb[j][i] < tcold[j]) {
        ncold[j]++;
        cold = true;
      }
      if (land[floc] == 157 && tb[j][i] > thot[j]) nhotland[j]++;
      if (land[floc] == 195 && tb[j][i] > thot[j]) nhotcoast[j]++;
      if (land[floc] == 0  && tb[j][i] > thot[j] && icec[floc] == 0.) nhotwater[j]++;
      if (land[floc] == 0  && tb[j][i] > thot[j] && icec[floc] != 0.0) { nhotice[j]++; }
      if (land[floc] == 157 && tb[j][i] < tcold[j]) ncoldland[j]++;
      if (land[floc] == 195 && tb[j][i] < tcold[j]) ncoldcoast[j]++;
      if (land[floc] == 0  && tb[j][i] < tcold[j] && icec[floc] == 0.) ncoldwater[j]++;
      if (land[floc] == 0  && tb[j][i] < tcold[j] && icec[floc] != 0.0) { ncoldice[j]++; }

      
    } // nfreqs
    //if (is_land && !mixed) {
    if (is_land ) {
      nfiltered++;
      fprintf(foutland, "%7.3f %8.3f  ",lat[i], lon[i]);
      for (int k = 0; k < NFREQS; k++) {
        fprintf(foutland, "%6.2f ",tb[k][i]);
      }
      fprintf(foutland, "  %3d %4.2f km %6.1f\n",land[floc], icec[floc], distance[floc]/1000.);
      hot = false;
      is_land = false;
    }
    if (mixed && !is_land) {
      nfiltered++;
      fprintf(foutmixed, "%7.3f %8.3f  ",lat[i], lon[i]);
      for (int k = 0; k < NFREQS; k++) {
        fprintf(foutmixed, "%6.2f ",tb[k][i]);
      }
      fprintf(foutmixed, "  %3d %4.2f km %6.1f\n",land[floc], icec[floc], distance[floc]/1000.);
      hot = false;
      mixed = false;
      is_land  = false;
    }
    if (cold) {
      nfiltered++;
      fprintf(foutcold, "%7.3f %8.3f  ",lat[i], lon[i]);
      for (int k = 0; k < NFREQS; k++) {
        fprintf(foutcold, "%6.2f ",tb[k][i]);
      }
      fprintf(foutcold, "  %3d %4.2f km %6.1f\n",land[floc], icec[floc], distance[floc]/1000.);
      cold = false;
    }

  } // ndata points
  printf("nfiltered = %d nused = %d count = %d\n",nfiltered, nused, count);

  float pland  =  (float) nland / (float) nused;
  float pcoast =  (float) ncoast / (float) nused;
  float pwater =  (float) nwater / (float) nused;
  float pice   =  (float) nice / (float) nused;
  float phot[NFREQS], photwater[NFREQS], photland[NFREQS], photcoast[NFREQS], photice[NFREQS];
  float pcold[NFREQS], pcoldwater[NFREQS], pcoldland[NFREQS], pcoldcoast[NFREQS], pcoldice[NFREQS];
  
  for (j = 0; j < NFREQS; j++) {
    phot[j] =  (float)nhot[j] / (float) nused;
    photwater[j] = (float) nhotwater[j] / (float) nwater;
    photland [j] = (float) nhotland [j] / (float) nland;
    photcoast [j] = (float) nhotcoast [j] / (float) ncoast;
    photice [j] = (float) nhotice [j] / (float) nice;

    pcold[j] =  (float)ncold[j] / (float) nused;
    pcoldwater[j] = (float) ncoldwater[j] / (float) nwater;
    pcoldland [j] = (float) ncoldland [j] / (float) nland;
    pcoldcoast [j] = (float) ncoldcoast [j] / (float) ncoast;
    pcoldice [j] = (float) ncoldice [j] / (float) nice;

    printf("H%1d %7d %5.3f  %6.4f %6.4f %6.4f %f   water %f land %f coast %f ice %f\n",
      j,nhot[j], phot[j], 
      photwater[j], photland[j], photcoast[j], photice[j],
          photwater[j]*pwater/phot[j],
          photland [j]*pland /phot[j],
          photcoast[j]*pcoast /phot[j],
          photice  [j]*pice  /phot[j]
      );
    printf("C%1d %7d %5.3f  %6.4f %6.4f %6.4f %f   water %f land %f coast %f ice %f\n",
      j,ncold[j], pcold[j], 
      pcoldwater[j], pcoldland[j], pcoldcoast[j], pcoldice[j],
          pcoldwater[j]*pwater/pcold[j],
          pcoldland [j]*pland /pcold[j],
          pcoldcoast[j]*pcoast /pcold[j],
          //pcoldwater[j]*pwater/pcold[j] + pcoldland [j]*pland /pcold[j] + pcoldcoast[j]*pcoast /pcold[j],
          pcoldice  [j]*pice  /pcold[j]
      );
  }


  return 0;
}

////////////////////////////////////////////////////////////////
int acquire(mvector<float> &lat, mvector<float> &lon, mvector<float> *tb, int &count) {
  int qc;
  double clat, clon;
  int nfreqs = NFREQS;
  float tmp[nfreqs], top[nfreqs], bottom[nfreqs];
  float conc, flag = 330.0;
  FILE *flatout;
  int i;

  count = 0;
  for (i = 0; i < nfreqs; i++) {
    top[i] = 0.0;
    bottom[i] = 330.0;
    tb[i].resize(MAXOBS);
    tb[i] = flag; 
  }

  flatout = fopen("flatout","r");
  if (flatout == (FILE*) NULL) {
    printf("failed to open flatout\n");
    return 1;
  }

  //int freq = 100000;
  while (!feof(flatout)) {
    //if ((count% freq) == 0) { printf("%d *100k obs\n",count/ freq); fflush(stdout); }
    fread(&clon, sizeof(clon), 1, flatout);
    fread(&clat, sizeof(clat), 1, flatout);
    fread(&conc, sizeof(conc), 1, flatout);
    fread(&qc, sizeof(qc), 1, flatout);
    fread(&tmp[0], sizeof(float), nfreqs, flatout);
    for (i = 0; i < nfreqs; i++) {
      if (tmp[i] > top[i]   ) top[i]    = tmp[i];
      if (tmp[i] < bottom[i]) bottom[i] = tmp[i];
    }
    
    if (!feof(flatout)) {
      lat[count] = (float) clat;
      lon[count] = (float) clon;
      for (i = 0; i < nfreqs; i++) {
        tb[i][count] = tmp[i];
      }
       
      count++;
    }
  }
  fclose(flatout);

  return 0;
}
