#include <stdio.h>
#include <time.h>

int main(int argc, char *argv[]) {
  int i;
  tm end_time;
  time_t end_secs;

  end_time.tm_sec = 0; end_time.tm_min = 0; end_time.tm_hour = 0;
  end_time.tm_mday = 31;  end_time.tm_mon = 11;   end_time.tm_year = 98;
  for (i = 0; i < 1e6; i++) {
    end_secs = mktime(&end_time);
  }


  return 0;
}
