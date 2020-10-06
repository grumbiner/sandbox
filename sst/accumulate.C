#include "ncepgrids.h"

//Accumulate the simple-minded sst analyses and update the
//  age since last observation as needed.
//Construct a grid of age = 255 if there's no age file specified
//Robert Grumbine 24 December 2008

// Update -- extend limit of possible ages as it appears that some points 
//   exceeded 255 days at some locations

#define NEVER 32767
#define SSTFLAG 265.0

int main(int argc, char *argv[]) {
  global_ice<float> oldsst, newsst;
  global_ice<int> age;
  FILE *fin1, *fin2, *fin3, *fout1, *fout2;

  ijpt loc;
  float rate, toler = 0.5*1.414*2.575; //given 0.5 stderr, difference 
                                       //between 2 obs, 
                                       //1% (2-sided) level on gaussian
  latpt ll;

  fin1 = fopen(argv[1], "r");
  oldsst.binin(fin1);
  fclose(fin1);
  fin2 = fopen(argv[2], "r");
  newsst.binin(fin2);
  fclose(fin2);

  fin3 = fopen(argv[3], "r");
  if (fin3 == (FILE *) NULL) {
    age.set(NEVER);
  }
  else {
    age.binin(fin3);
    fclose(fin3);
  }

// Loop over newsst grid.  Fill in with oldsst if no obs, and then
//   update the data age.  Else, zero the age and use new sst.
  printf("tolerance = %f\n",toler);
  for (loc.j = 0; loc.j < age.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < age.xpoints(); loc.i++) {
    if (newsst[loc] == SSTFLAG) {
      newsst[loc] = oldsst[loc];
      if (age[loc] != NEVER) age[loc] += 1;
    }
    else {
      rate = (newsst[loc] - oldsst[loc])/(age[loc]+1);
      if (fabs(rate) > toler && age[loc] != NEVER) {
        ll = age.locate(loc);
        printf("jump %3d %3d  %6.2f %6.2f  %6.2f %6.2f  %6.2f %3d\n",
           loc.i, loc.j, ll.lat, ll.lon, oldsst[loc], newsst[loc], 
           rate, age[loc]); 
      }
      age[loc] = 0;
    }
  }
  }

  fout1 = fopen(argv[4], "w");
  newsst.binout(fout1);
  fclose(fout1);
  palette<unsigned char> gg(19,65);
  newsst.scale();
  newsst.xpm("fill.xpm",7,gg);

  fout2 = fopen(argv[5], "w");
  age.binout(fout2);
  fclose(fout2);

  return 0;
} 
