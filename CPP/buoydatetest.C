#include "dates.h"
#include "location.h"

class buoy : public date, public location {
  public:
    float sst;
    buoy();
    buoy(date&, location&);
    void show();
};
buoy::buoy() {
  location x(0., 0.);
  set(x);
  set_date(70, 10, 1, 12.0);
  sst = 273.15;
}
buoy::buoy(date &x, location &y) {
  sst = 273.15;
  set_date(x);
  set(y);
}
void buoy::show() {
  printf("sst = %f\n",sst);
  this->location::show();
  show_date();
}

int main(void) {
  date d1(70, 10, 1, 12.0), d2(70, 10, 2, 12.0);
  location l1(45.0, -90.0), l2(40.0, -77.5);
  buoy x(d1, l1), y(d2, l2), z;
  date tmp;

  printf("Time difference %f\n", y - x);
  printf("space difference %f\n", y%x);
  printf("Buoy data:\n");
  x.show();
  tmp = (date) y;
  printf("now display result of tmp = (date) y;\n");
  tmp.show_date();

  return 0;
}
