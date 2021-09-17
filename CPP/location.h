#include <stdio.h>

//Apr 26 2000

extern "C" float arcdis_(float &lon1, float &lat1, float &lon2, float &lat2);

class location {
public:
  location();
  location(float, float);
  float latitude();
  float longitude();
  void show();

  float operator%(location &); // return distance between two points
//  location operator+=
//  location operator-=
//  bool qc(); // -- is position on earth.
  location & operator=(location& );
  location & move(location &);  // incremental position changes
  location & set(location &);

private:
  float lat, lon;
};
void location::show() {
  printf("%f and %f\n",lat, lon);
}
location & location::set(location &x) {
  lat = x.lat;
  lon = x.lon;
  return *this;
}
location::location() {
  lat = 0.;
  lon = 0.;
}
location & location::operator=(location &x) {
  lat = x.lat;
  lon = x.lon;
  return *this;
}
location & location::move(location &x) {
  lat += x.lat;
  lon += x.lon;
  return *this;
}
location::location(float x, float y) {
  lat = x;
  lon = y;
}
float location::operator%(location &x) {
  return arcdis_(this->lon, this->lat, x.lon, x.lat);
}
float location::longitude() {
  return this->lon;
}
float location::latitude() {
  return this->lat;
}


// A class with a 'has a ' inheritance
class drifter : public location {
  public:
    int year;
    drifter();
    drifter& operator=(location&);
};
drifter::drifter() {
  year = 5;
}
drifter& drifter::operator=(location&x) {
  this->set(x);
  year = 5;
}
