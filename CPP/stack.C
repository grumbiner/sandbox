#include <stack>
//10 Dec 2008
//Robert Grumbine

#include "ncepgrids.h"

class datapt {
  public:
    latpt ll;
    float datum;
    datapt();
    datapt(const datapt&);    
    //datapt operator=(const datapt &ref);
};
datapt::datapt(void) {
  ll.lat = -99.;
  ll.lon = 0.0;
  this->datum = 0;
}
datapt::datapt(const datapt &ref) {
  ll.lat = ref.ll.lat;
  ll.lon = ref.ll.lon;
  this->datum = ref.datum;
}
  


int main(int argc, char *argv[]) {
  global_ice<unsigned char> land;
  grid2<stack<datapt> > x(land.xpoints(), land.ypoints() );
  float dx = 0.5, dy = 0.5, datum;
  ijpt loc;
  latpt ll;
  datapt satellite, ref;
  bool more_data = true;
  FILE *fin;

  fin = fopen(argv[1],"r");

  while (more_data) {
    //parse to data point
    ll = satellite.ll;
    loc = land.locate(ll); 
    x[loc].push(satellite);
    
  } 

// Sample output:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j += 36) {
    ll.lat = loc.j * dy - 89.75;
    for (loc.i = 0; loc.i < x.xpoints(); loc.i += 36) {
      ll.lon = 0.25 + loc.i*dx;
      while (!x[loc].empty() ) {
        ref = x[loc].top();
        printf("%f %f vs. %f %f val %f\n",ll.lat, ll.lon, ref.ll.lat, ref.ll.lon, ref.datum);
        x[loc].pop();
      }
    }
  } 


  return 0;
}
