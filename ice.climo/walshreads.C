// 30 July 1999
#include "metric.h"

void walshtof(metricgrid<char> &c, metricgrid<float> &f) ;
void walshtobin(metricgrid<char> &c, metricgrid<int> &f) ;

/////////////////////////////////////zzzzzzzzzzzzzzzzzzz////////////////////
void walshtof(metricgrid<char> &c, metricgrid<float> &f) {
// Translate out of walsh character encoding to floating point concentrations
  ijpt loc;
  int tmpint;
  char *c1;
  
  c1 = new char[1];
  for (loc.j = 0; loc.j < c.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < c.xpoints() ; loc.i++) {
      
    //printf("character %2d %2d %1c\n",loc.i, loc.j, c[loc]);

    if (isdigit(c[loc]) ) {
      c1[0] = c[loc];
      tmpint = atoi(c1);
      f[loc] = ((float) tmpint) / 10.0;
    }
    else if (c[loc] == '*') {
      f[loc] = 1.0; 
    }
    else if (c[loc] == '.') {
      f[loc] = 1.50; // Land tag in RG systems
    }
    else {
      printf("Unknown character type %2d %2d %1c\n",loc.i, loc.j, c[loc]);
    }
  }
  }

  return;
}

void walshtobin(metricgrid<char> &c, metricgrid<int> &f) {
// Translate out of walsh character encoding to floating point concentrations
  ijpt loc;
  int tmpint;
  char *c1;
  
  c1 = new char[1];
  
  for (loc.j = 0; loc.j < c.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < c.xpoints() ; loc.i++) {
    if ( isdigit(c[loc]) ) {
      c1[0] = c[loc];
      tmpint = atoi(c1);
      if (tmpint > 0) {
        f[loc] = 1;
      }
      else {
        f[loc] = 0;
      }
    } 
    else if ( c[loc] == '*') {
      f[loc] = 1;
    } 
    else if (c[loc] == '.') {
      f[loc] = 0;
    }
    else {
      printf("Unknown character type %2d %2d %1c\n",loc.i, loc.j, c[loc]);
      f[loc] = 0;
    }
  }
  } 
    
  return;
} 
