#include <stdio.h>
#include "mvector.h"
#include "ssmi.h"
#include "ncepgrids.h"

#define MAX_LINES 100000

typedef struct {
  float lat[NSCANS*4], lon[NSCANS*4];
  float t19v[NSCANS*4];
  float t19h[NSCANS*4];
  float t22v[NSCANS*4];
  float t37v[NSCANS*4];
  float t37h[NSCANS*4];
  float t85v[NSCANS*4];
  float t85h[NSCANS*4];
} obs;

typedef struct {
  float lat, lon, t19v, t19h, t22v, t37v, t37h, t85v, t85h;
} short_obs;

void read_line(bufr_line &x, FILE *fin) ;
void split(bufr_line &x, obs &y);


int main(int argc, char *argv[]) {
  bufr_line all;
  obs x;
  FILE *fin, *maskin, *fout;
  int i = 0, j;
  short_obs y;
  global_12th<unsigned char> posteriori;
  ijpt tloc;
  latpt ll;

  fin = fopen(argv[1], "r");
  maskin = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");

  posteriori.binin(maskin);
  fclose(maskin);

  while (!feof(fin) && i < MAX_LINES) {
    read_line(all, fin);
    split(all, x);

// Here is where to put in a posteriori filtering -- look only at points
//   that might have ice at some point in year
    for (j = 0; j < NSCANS*4; j++) {
      ll.lat = x.lat[j];
      ll.lon = x.lon[j];
      tloc = posteriori.locate(ll);
      if (posteriori[tloc] == 165 ||       // ocean warmer than -3, colder than 2.15 C
          posteriori[tloc] == 173 ||       // lakes warmer than 0, colder than 2.15 C
          posteriori[tloc] == 174    ) {   // lakes warmer than -3, colder than 0 C
        y.lat = x.lat[j];
        y.lon = x.lon[j];
        y.t19v = x.t19v[j];
        y.t19h = x.t19h[j];
        y.t22v = x.t22v[j];
        y.t37v = x.t37v[j];
        y.t37h = x.t37h[j];
        y.t85v = x.t85v[j];
        y.t85h = x.t85h[j];
        fwrite(&y, sizeof(short_obs), 1, fout);
      }
    }

//    fwrite(&x, sizeof(obs), 1, fout);

    i++;
  }

  printf("size of bufr_line %d size of all %d\n",sizeof(bufr_line), sizeof(all) );
  printf("size of obs %d size of x %d \n",sizeof(obs), sizeof(x) );
  printf("finished reading, i = %d\n",i);

  fclose(fout);

  return 0;
}


void read_line(bufr_line &x, FILE *fin) {
  int i, k;
  int satno, year, month, day, hour, min, sec, scan;
  int counter, stype, posn;
  float lat, lon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  
  fscanf(fin, "%d %d %d %d %d %d %d %d\n",&satno, &year, &month, &day,
           &hour, &min, &sec, &scan);
  //printf("%d %d %d %d %d %d %d %d\n",satno, year, month, day,
  //         hour, min, sec, scan); fflush(stdout);
  x.satno = satno;
  x.year  = year;
  x.month = month;
  x.day   = day;
  x.hour  = hour;
  x.mins  = min;
  x.secs  = sec;
  x.scan_no = scan;

  int kwrit;
  float xlat, xlon, tmp1, tmp2;
  for (i = 0; i < NSCANS; i++) {
    fscanf(fin, "%d %f %f %d %d %f %f %f %f %f %f %f\n", &counter,
           &lat, &lon, &stype, &posn, 
           &t19v, &t19h, &t22v, &t37v, &t37h, &t85v, &t85h);
    //printf("%d %f %f %d %d %f %f %f %f %f %f %f\n", counter,
    //       lat, lon, stype, posn, 
    //       t19v, t19h, t22v, t37v, t37h, t85v, t85h); fflush(stdout);
    x.full[i].scan_counter = counter;
    x.full[i].latitude     = lat;
    x.full[i].longitude    = lon;
    x.full[i].surface_type = stype;
    x.full[i].position_num = posn;
    x.full[i].t19v         = t19v;
    x.full[i].t19h         = t19h;
    x.full[i].t22v         = t22v;
    x.full[i].t37v         = t37v;
    x.full[i].t37h         = t37h;
    x.full[i].t85v         = t85v;
    x.full[i].t85h         = t85h;
    for (k = 0; k < 3; k++) {
      fscanf(fin, "%d %f %f %d %f %f\n",&kwrit,
         &xlat, &xlon, &posn, &tmp1, &tmp2);
      x.full[i].hires[k].kwrit     = kwrit;
      x.full[i].hires[k].latitude  = xlat;
      x.full[i].hires[k].longitude = xlon;
      x.full[i].hires[k].t85v      = tmp1;
      x.full[i].hires[k].t85h      = tmp2;
    }
  }

  return;
}  
void split(bufr_line &x, obs &y) {
  int i, k, index = 0;

  for (i = 0; i < NSCANS; i++) {
    y.lat[index]  = x.full[i].latitude;
    y.lon[index]  = x.full[i].longitude;
    y.t19v[index] = x.full[i].t19v;
    y.t19h[index] = x.full[i].t19h;
    y.t22v[index] = x.full[i].t22v;
    y.t37v[index] = x.full[i].t37v;
    y.t37h[index] = x.full[i].t37h;
    y.t85v[index] = x.full[i].t85v;
    y.t85h[index] = x.full[i].t85h;
    index++; 
    for (k = 0; k < 3; k++) {
      y.lat[index]  = x.full[i].hires[k].latitude;
      y.lon[index]  = x.full[i].hires[k].longitude;
      y.t19v[index] = x.full[i].t19v;
      y.t19h[index] = x.full[i].t19h;
      y.t22v[index] = x.full[i].t22v;
      y.t37v[index] = x.full[i].t37v;
      y.t37h[index] = x.full[i].t37h;
      y.t85v[index] = x.full[i].hires[k].t85v;
      y.t85h[index] = x.full[i].hires[k].t85h;
      index++;
    }
  }

  return;

}
