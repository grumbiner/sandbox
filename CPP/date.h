#include <stdio.h>

// Class to implement NCEP standards on date-time manipulations
// Robert Grumbine 15 June 2001

class Date {
  private: 
    int yy, mm, dd;
  public:
    Date();
    Date(int, int, int);
    Date& nextday();
    char* show();
};
Date::Date() {
  yy = 1900;
  mm = 1;
  dd = 1;
}
Date::Date(int y, int m, int d) {
  yy = y;
  mm = m;
  dd = d;
}
Date& Date::nextday() {
  dd += 1;
  if (dd <= 28) return *this; // don't need to worry about wrapping of month
  switch (mm) {
    case 1:
      if (dd == 32) {
        mm += 1;
        dd = 1;
      }
      break;
    case 2:
      if (dd == 30) {
        mm += 1;
        dd = 1;
      }
      else if (dd == 29 && (yy % 4 != 0) ) {
        mm += 1;
        dd = 1;
      }
      else if (dd == 29 && (yy %4 == 0) && (yy %400 == 0) ) {
        mm += 1;
        dd = 1;
      }
      break;
    case 3:
      if (dd == 32) {
        mm += 1;
        dd = 1;
      }
      break;
    case 4:
      if (dd == 31) {
        mm += 1;
        dd = 1;
      }
      break;
    case 5:
      if (dd == 32) {
        mm += 1;
        dd = 1;
      }
      break;
    case 6:
      if (dd == 31) {
        mm += 1;
        dd = 1;
      }
      break;
    case 7:
      if (dd == 32) {
        mm += 1;
        dd = 1;
      }
      break;
    case 8:
      if (dd == 32) {
        mm += 1;
        dd = 1;
      }
      break;
    case 9:
      if (dd == 31) {
        mm += 1;
        dd = 1;
      }
      break;
    case 10:
      if (dd == 32) {
        mm += 1;
        dd = 1;
      }
      break;
    case 11:
      if (dd == 31) {
        mm += 1;
        dd = 1;
      }
      break;
    case 12:
      if (dd == 32) {
        yy += 1;
        mm = 1;
        dd = 1;
      }
      break;
    default:
      printf("error!\n");
  }
  return *this;
}
char* Date::show() {
  char *tag;
  tag = new char[8];
 
  sprintf(tag, "%4d%02d%02d",yy,mm,dd);
  return tag;
}
