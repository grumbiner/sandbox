#include "ncepgrids.h"

#define LAG 1
#define NFIELDS 2555
#define FRAG 15

float relate(mvector<float> &x, mvector<float> &y, ijpt &loc) ;
template<class T> 
class northfrag : public psgrid<T> {
  public:
   northfrag(void); /* Construction creator */
   //northfrag(northfrag<T> &);
   //void operator=(grid2<T> );
};
template<class T>
northfrag<T>::northfrag(void)
{
  this->nx = 385;
  this->ny = 465/FRAG;
  this->dx = 25.4e3;
  this->dy = 25.4e3;
  this->xorig = (-38.*5* this->dx );
  this->yorig = (-46.*5* this->dy );
  this->sgn = 1.0;
  this->slat = 60.0;
  this->slon = (-10.0);
// rearth and eccen2 are taken from psgrid base class

  this->grid = new T[this->nx*this->ny] ;
  if (this->grid == (T *) NULL) { cout << "Failed to new in northfrag(void)\n";
                            fflush(stdout); }
  this->pds.set_gridid(255);

  return ;
}



int main(int argc, char *argv[]) {
   northfrag<float> ice[NFIELDS];
   northgrid<float> acor, tice;
   
   mvector<float> x(NFIELDS), y(NFIELDS); 
   int i, k;
   FILE *fin;
   ijpt loc, tloc;
   
   
   for (k = 0; k < FRAG; k++) {
     for (i = 1; i < NFIELDS+1; i++) {
       fin = fopen(argv[i],"r");
       //printf("%d %s\n",i,argv[i]);
       tice.binin(fin);
       fclose(fin);
       // transfer:
       for (loc.j = 0; loc.j < ice[0].ypoints(); loc.j++) {
         tloc.j = loc.j + k*ice[0].ypoints();
       for (loc.i = 0; loc.i < ice[0].xpoints(); loc.i++) {
         tloc.i = loc.i;
         ice[i-1][loc] = tice[tloc];
       }
       }
     }
  
     for (loc.j = 0; loc.j < ice[0].ypoints(); loc.j += 1) {
         tloc.j = loc.j + k*ice[0].ypoints();
     for (loc.i = 0; loc.i < ice[0].xpoints(); loc.i += 1) {
         tloc.i = loc.i;
       // get the initial time
       //for (i = 0; i < NFIELDS; i++) {
       //}
       for (i = 0; i < NFIELDS-LAG; i++) {
         x[i] = ice[i][loc];
         y[i] = ice[i+LAG][loc];
       }
       acor[tloc] = relate(x, y, tloc);
     }
     }
   } // k
   return 0;
}
float relate(mvector<float> &x, mvector<float> &y, ijpt &loc) {
  double sx = 0, sy = 0, sx2 = 0, sy2 = 0, sxy = 0, r, a, b;
  int i, nx = NFIELDS-LAG, count;
  // note that it is y.xpoints, not x, as y is the leading target vector

  count = 0;
  for (i = 0; i < nx; i++) {
    if (x[i] <= 1.0 && y[i] <= 1.0) {
      sx  += x[i];
      sy  += y[i];
      sxy += x[i]*y[i];

      sx2 += x[i]*x[i];
      sy2 += y[i]*y[i];
      count += 1;
    }
  }

  // y = ax + b
  if (count > 1 && ((double)count*sx2 - sx*sx) > 0 && ((double)count*sy2 - sy*sy) > 0 ) {
    a = ((double)count*sxy - sx*sy)/((double)count*sx2 - sx*sx);
    b = (sy - a*sx)/(double)count;
    r = ((double)count*sxy - sx*sy)/sqrt((double)count*sx2 - sx*sx)/sqrt((double)count*sy2 - sy*sy);
    printf("%f %f %f  %f %f %f   %f %f  %3d %3d  %4d\n",r, a, b, sx, sy, sxy, 
         sqrt((double)count*sx2 - sx*sx), sqrt((double)count*sy2 - sy*sy), loc.i, loc.j, count);
    return (float) r;
  }
  else {
    return 2.24;
  }

}
