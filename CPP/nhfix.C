#include <stdio.h>

#include "ncepgrids.h"
#include "color.h"

template <class T>
int stable (T x1, T x2, T x3, T x4, T &avg);

int main(void) {
  northgrid<unsigned char> nh, nland, nhout;
  unsigned char avg;
  FILE *nin, *landin;
  int changes = 0;
  int started, iters;
  ijpt loc;
  int indexpi, indexpj, indexmi, indexmj;
  palette<unsigned char> gg(19, 65);

//Read in data
  nin = fopen("north","r");
  landin = fopen("nland.map","r");
  if (nin == (FILE *) NULL) {
    printf("failed to read in the north input file\n");
    return 1;
  }
  if (landin == (FILE*) NULL) {
    printf("failed to read in the north land input file\n");
    return 1;
  }

  nh.binin(nin);
  nland.binin(landin);
  fclose(nin);
  fclose(landin); 

  started = 1==1;
  iters = 0;
//Iterate the following until nothing is changed:
  while ( (changes > 0 || started) && iters < 5 ) {
     started = 1==0;
     changes = 0;
     iters += 1;
     
     for (loc.j = 1; loc.j < nh.ypoints() - 1; loc.j++) {
     for (loc.i = 1; loc.i < nh.xpoints() - 1; loc.i++) {
       indexpi = loc.i + 1 + loc.j * nh.xpoints();
       indexpj = loc.i + nh.xpoints() + loc.j * nh.xpoints();
       indexmi = loc.i - 1 + loc.j * nh.xpoints();
       indexmj = loc.i - nh.xpoints() + loc.j * nh.xpoints();
       
//Search for undefined (224) or bad data (166) surrounded by:
//     -> average if range of 4 adjacent points is 'small' (will set to
//  weather if surrounded by weather, land if that, coast, ice concentration
//    average ...  Difference between max and min can be no greater than 4.
       if ( (int) nh[loc] == (int) 224 ||
            (int) nh[loc] == (int) 177 ||
            (int) nh[loc] == (int) 166    ) {
         if (stable(nh[indexpi], nh[indexmi], nh[indexpj], 
                    nh[indexmj], avg) ) { 
           if (nh[loc] != avg) {
             printf("avg = %d nh[loc] = %d no data stable\n",avg, nh[loc] );
             nh[loc] = avg;
             changes += 1; 
           }
         }

       }

     }
     }
     printf("iter %d changes %d\n",iters, changes);
  } 

  nh.xpm("nh.new.xpm", 7, gg);

  return 0;
}

template <class T>
int stable (T x1, T x2, T x3, T x4, T &avg) {
   T tmax, tmin;
   double tmpavg;

   tmax = max(x1, x2);
   tmax = max(tmax, x3);
   tmax = max(tmax, x4);
   tmin = min(x1, x2);
   tmin = min(tmin, x3);
   tmin = min(tmin, x4);
 
   tmpavg = (double) x1 + (double) x2 + (double) x3 + (double) x4;
   tmpavg /= 4.0;
   avg = (T) tmpavg;

   if (tmax - tmin <= 4.0) {
     return 1==1;
   }
   else {
     return 0==1;
   }

}
