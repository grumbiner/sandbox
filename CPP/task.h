#ifndef PROJECTH

#define PROJECTH

#include <stdio.h>

//class library for 'projects' -- things that need to be done, may have due
//  date/times, ...

//Class for times -- may be calendar or simple differences
class time {
  public:
    int year, month, day, hour, minute;
    float second;
  //Ctor-dtor
    time();
    time(int, int, int, int, int, float);
    ~time();
  //Operations:
    void operator=(time );
    time operator-(time &);
    void show(FILE *);
    void fix(); // Fix up the time to match a real calendar date
};
time::time() {
  year = 1900;
  month = 01;
  day =   01;
  hour =  00;
  minute = 00;
  second = 0.0;
}
time::time(int a1, int a2, int a3, int a4, int a5, float secs) {
  year = a1;
  month = a2; 
  day   = a3;
  hour  = a4;
  minute = a5;
  second = secs;
}
time::~time() {
  year = 0;
  month = 0;
  day =   0;
  hour =  0;
  minute = 0;
  second = 0.0;
}
void time::operator=(time x) {
  year = x.year;
  month = x.month;
  day =   x.day;
  hour =  x.hour;
  minute = x.minute;
  second = x.second;
}
time time::operator-(time &x) {
  time y;
  float temp_seconds;
  y.year = year - x.year;
  y.month = month - x.month;
  y.day =   day - x.day;
  y.hour =  hour - x.hour;
  y.minute = minute - x.minute;
  y.second = second - x.second;

// Do a back-conversion to get consistent times, at least for the short
//   time periods.  i.e., avoid +1s -2 min +3 hr.
  temp_seconds = y.second + 60 * (y.minute + 60*(y.hour + 24*y.day) );
  y.day = (int)(temp_seconds / 86400);
  temp_seconds -= y.day * 86400;
  y.hour = (int) (temp_seconds / 3600);
  temp_seconds -= y.hour * 3600;
  y.minute = (int) (temp_seconds / 60);
  temp_seconds -= y.minute * 60;
  y.second = temp_seconds;
  return y;
}
void time::show(FILE *fout) {
   fprintf(fout, "%d %d %d %d %d %f\n",year, month, hour, minute, second);
}
void time::fix() {
// Fix up to match with legal dates.  First check to see if dates are
//  already in bounds:
  if (year >= 1770 && month > 0 && month <= 12 &&
      day <= 28 && day >= 1 &&
      hour < 24 && hour >= 0 &&
      minute < 60 && minute >= 0 &&
      second <= 61.0 && second >= 0 ) {
   return;
  }
// Some thing(s) to fix:
  if (second > 61.0) {
  }
  if (second < 0.0) {
  }
  if (minute >= 60) {
  }
  if (minute < 0) {
  }
  if (hour >= 24) {
  }
  if (hour < 0) {
  }
  if (day > 28) {
    //leap year checks + month checks
  }
  if (month > 12) {
    // logic
  }
  if (month <= 0) {
  }
  if (year < 1770) {
    printf("warning, can't fix years that may be pre-gregorian\n");
    return;
  }

  return;
}
  
   
  

//
//class date : public time {
//// Class for dates -- strict following of calendar
//};
//
//#define MAX_PRIORITY 128
//
//// A task is a single well-defined activity with no dependencies
//class task {
//  public:
//  int   current_priority;  // current, or apparent priority
//  char *name;
//  project();
//  project(date &, time&, int, char *);
//  ~project();
//  set_due(date &);       // Set the due date to be this
//  set_due_rel(time &);   // Set the due date 'time' from the present time
//  set_priority(int );    // set/reset current priority.  Note that inherent
//				// priority cannot be changed by user
//  set_length(time &);
//  set_name(char *);
//  age_priority(time &);  // Adjust current priority for the fact that time
//                         //has passed.  Priority hits limit value at a 
//			//date of (due - length)
//protected :
//inherent_priority(int );  // so that internals can set inherent priority
//
//private :
//date  due;
//time  length;
//int   priority;  //inherent priority.  Will be taken from project
//}
//
//// A project is a collection of tasks.  To finish project, each of the tasks
////   must be completed.  If there are task dependencies, the task priority
////   must reflect this.
//class project : public task {
//public :
//  project();
//  ~project();
//  addtask(task &);
//  addtask(date &, time &, int, char *);
//private :
//  int ntask;
//  task *tasks;
//  int priority;  //project priority  Intrinsic priority of a task may not
//                 //exceed this
//};

#endif
