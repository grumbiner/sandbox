#include <stdio.h>
#include <time.h>

class date {
  private:
    tm tmtime;
    time_t secs;
  public:
    date();
    date(int, int, int, float);
    void set_date(int, int, int, float);
    void set_date(date &);
    void show_date();
    double operator-(date &);

};
void date::set_date(date &x) {
  tmtime.tm_sec = x.tmtime.tm_sec;
  tmtime.tm_min = x.tmtime.tm_min;
  tmtime.tm_hour = x.tmtime.tm_hour;
  tmtime.tm_mday = x.tmtime.tm_mday;
  tmtime.tm_mon = x.tmtime.tm_mon;
  tmtime.tm_year = x.tmtime.tm_year;
  secs = mktime(&tmtime);
}
  
date::date() {
  tmtime.tm_sec = 0;
  tmtime.tm_min = 0;
  tmtime.tm_hour = 0;
  tmtime.tm_mday = 0;
  tmtime.tm_mon = 0;
  tmtime.tm_year = 70;
  secs = mktime(&tmtime);
}
void date::set_date(int x, int y, int z, float h) {
  tmtime.tm_year  = x;
  tmtime.tm_mon   = y;
  tmtime.tm_mday  = z;
  tmtime.tm_hour  = (int) h;
  tmtime.tm_min   = (int) ((h - (int) h) * 60 + 0.5);
  tmtime.tm_sec   = (int) h*3600. -   tmtime.tm_hour*3600. - tmtime.tm_min*60.;
  secs = mktime(&tmtime);
}
date::date(int x, int y, int z, float h) {
  tmtime.tm_year  = x;
  tmtime.tm_mon   = y;
  tmtime.tm_mday  = z;
  tmtime.tm_hour  = (int) h;
  tmtime.tm_min   = (int) ((h - (int) h) * 60 + 0.5);
  tmtime.tm_sec   = (int) h*3600. -   tmtime.tm_hour*3600. - tmtime.tm_min*60.;
  secs = mktime(&tmtime);
}
double date::operator-(date &x) {
  // operation returns the difference, in seconds, between dates
  printf("%d %d\n",this->secs, x.secs);
  return difftime(this->secs, x.secs);
}
void date::show_date() {
  printf("%4d %2d %2d %d\n",1900+tmtime.tm_year, tmtime.tm_mon+1, tmtime.tm_mday, tmtime.tm_hour);
}

