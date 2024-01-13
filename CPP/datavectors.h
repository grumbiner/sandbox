#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <limits>
#include <climits>
#include <cfloat>
#include <iostream>
using namespace std;

// Class for working with mathematical vectors (mvectors)
// Robert Grumbine (oldest extant 1997/11/26)
//
// Modifications:
//   1998/06/17 add printing
//   1998/06/27 use references to vectors as arguments rather than 
//                vectors (avoid recopying large data spans)
//   1998/09/30 start developing a time-series capable class
//   1998/12/03 add many vector operators
//   1999/      begin move to iostream, add otislev
//   1999/      add nodc levels, Levitus levels
//   1999/10/04 template otis, levitus levels
// On 2 April 2004 change everything from vector.h to mvector.h for the
// mathematical vectors.  Vector.h is actually part of the standard
// template library and it is time to stop conflicting with it.
//   2005/03/16
//     Add operator==constant
//     Add crosscorrel(timeseries)
//   20 Nov 2007: 'thisification'
//   2009/11/05
//     Add type cast to double (for metricgrid<mvector<T> > situations)
//     member initialization (per effc++)
//     make random vector, shuffle vector, construct histograms to 
//       specified resolution
// Extract data vectors to own include file:


#ifndef MVECTORH
  #include "mvector.h"
#endif
#ifndef DATAVECTORS
  #define DATAVECTORS

/////////////////////////////////////////////////////////////
// The following are specialized mvector for general use
//////////
class otislev : public mvector<float> {
  public:
    otislev();
};
otislev::otislev() {
  mvector<float>::setsize(34);
  mvector<float>::operator[](0) = 0.0;
  mvector<float>::operator[](1) = 2.5;
  mvector<float>::operator[](2) = 7.5;
  mvector<float>::operator[](3) = 12.5;
  mvector<float>::operator[](4) = 17.5;
  mvector<float>::operator[](5) = 25.0;
  mvector<float>::operator[](6) = 32.5;
  mvector<float>::operator[](7) = 40.0;
  mvector<float>::operator[](8) = 50.0;
  mvector<float>::operator[](9) = 62.5;
  mvector<float>::operator[](10) = 75.0;
  mvector<float>::operator[](11) = 100.0;
  mvector<float>::operator[](12) = 125.0;
  mvector<float>::operator[](13) = 150.0;
  mvector<float>::operator[](14) = 200.0;
  mvector<float>::operator[](15) = 300.0;
  mvector<float>::operator[](16) = 400.0;
  mvector<float>::operator[](17) = 500.0;
  mvector<float>::operator[](18) = 600.0;
  mvector<float>::operator[](19) = 700.0;
  mvector<float>::operator[](20) = 800.0;
  mvector<float>::operator[](21) = 900.0;
  mvector<float>::operator[](22) = 1000.0;
  mvector<float>::operator[](23) = 1100.0;
  mvector<float>::operator[](24) = 1200.0;
  mvector<float>::operator[](25) = 1300.0;
  mvector<float>::operator[](26) = 1400.0;
  mvector<float>::operator[](27) = 1500.0;
  mvector<float>::operator[](28) = 1750.0;
  mvector<float>::operator[](29) = 2000.0;
  mvector<float>::operator[](30) = 2500.0;
  mvector<float>::operator[](31) = 3000.0;
  mvector<float>::operator[](32) = 4000.0;
  mvector<float>::operator[](33) = 5000.0;
}

class otistag : public mvector<int> {
  public:
    otistag();
};
otistag::otistag() {
  mvector<int>::setsize(34);
  mvector<int>::operator[](0) = 1;
  mvector<int>::operator[](1) = 3;
  mvector<int>::operator[](2) = 8;
  mvector<int>::operator[](3) = 13;
  mvector<int>::operator[](4) = 18;
  mvector<int>::operator[](5) = 25;
  mvector<int>::operator[](6) = 33;
  mvector<int>::operator[](7) = 40;
  mvector<int>::operator[](8) = 50;
  mvector<int>::operator[](9) = 63;
  mvector<int>::operator[](10) = 75;
  mvector<int>::operator[](11) = 100;
  mvector<int>::operator[](12) = 125;
  mvector<int>::operator[](13) = 150;
  mvector<int>::operator[](14) = 200;
  mvector<int>::operator[](15) = 300;
  mvector<int>::operator[](16) = 400;
  mvector<int>::operator[](17) = 500;
  mvector<int>::operator[](18) = 600;
  mvector<int>::operator[](19) = 700;
  mvector<int>::operator[](20) = 800;
  mvector<int>::operator[](21) = 900;
  mvector<int>::operator[](22) = 1000;
  mvector<int>::operator[](23) = 1100;
  mvector<int>::operator[](24) = 1200;
  mvector<int>::operator[](25) = 1300;
  mvector<int>::operator[](26) = 1400;
  mvector<int>::operator[](27) = 1500;
  mvector<int>::operator[](28) = 1750;
  mvector<int>::operator[](29) = 2000;
  mvector<int>::operator[](30) = 2500;
  mvector<int>::operator[](31) = 3000;
  mvector<int>::operator[](32) = 4000;
  mvector<int>::operator[](33) = 5000;
}

// Levitus levels
class levitus_depths : public mvector<float> {
  public:
    levitus_depths();
};
levitus_depths::levitus_depths() {
  mvector<float>::setsize(33);
  mvector<float>::operator[](0) = 0.0;
  mvector<float>::operator[](1) = 10.;
  mvector<float>::operator[](2) = 20.;
  mvector<float>::operator[](3) = 30.;
  mvector<float>::operator[](4) = 50.;
  mvector<float>::operator[](5) = 75.;
  mvector<float>::operator[](6) = 100.;
  mvector<float>::operator[](7) = 125.;
  mvector<float>::operator[](8) = 150.;
  mvector<float>::operator[](9) = 200.;
  mvector<float>::operator[](10) = 250.;
  mvector<float>::operator[](11) = 300.;
  mvector<float>::operator[](12) = 400.;
  mvector<float>::operator[](13) = 500.;
  mvector<float>::operator[](14) = 600.;
  mvector<float>::operator[](15) = 700.;
  mvector<float>::operator[](16) = 800.;
  mvector<float>::operator[](17) = 900.;
  mvector<float>::operator[](18) = 1000.;
  mvector<float>::operator[](19) = 1100.;
  mvector<float>::operator[](20) = 1200.;
  mvector<float>::operator[](21) = 1300.;
  mvector<float>::operator[](22) = 1400.;
  mvector<float>::operator[](23) = 1500.;
  mvector<float>::operator[](24) = 1750.;
  mvector<float>::operator[](25) = 2000.;
  mvector<float>::operator[](26) = 2500.;
  mvector<float>::operator[](27) = 3000.;
  mvector<float>::operator[](28) = 3500.; //Not NODC level
  mvector<float>::operator[](29) = 4000.;
  mvector<float>::operator[](30) = 4500.; //Not NODC level
  mvector<float>::operator[](31) = 5000.;
  mvector<float>::operator[](32) = 5500.; //Not NODC level
}
// NODC levels
class nodc_depths : public mvector<float> {
  public:
    nodc_depths();
};
nodc_depths::nodc_depths() {
  mvector<float>::setsize(30);
  mvector<float>::operator[](0) = 0.0;
  mvector<float>::operator[](1) = 10.;
  mvector<float>::operator[](2) = 20.;
  mvector<float>::operator[](3) = 30.;
  mvector<float>::operator[](4) = 50.;
  mvector<float>::operator[](5) = 75.;
  mvector<float>::operator[](6) = 100.;
  mvector<float>::operator[](7) = 125.;
  mvector<float>::operator[](8) = 150.;
  mvector<float>::operator[](9) = 200.;
  mvector<float>::operator[](10) = 250.;
  mvector<float>::operator[](11) = 300.;
  mvector<float>::operator[](12) = 400.;
  mvector<float>::operator[](13) = 500.;
  mvector<float>::operator[](14) = 600.;
  mvector<float>::operator[](15) = 700.;
  mvector<float>::operator[](16) = 800.;
  mvector<float>::operator[](17) = 900.;
  mvector<float>::operator[](18) = 1000.;
  mvector<float>::operator[](19) = 1100.;
  mvector<float>::operator[](20) = 1200.;
  mvector<float>::operator[](21) = 1300.;
  mvector<float>::operator[](22) = 1400.;
  mvector<float>::operator[](23) = 1500.;
  mvector<float>::operator[](24) = 1750.;
  mvector<float>::operator[](25) = 2000.;
  mvector<float>::operator[](26) = 2500.;
  mvector<float>::operator[](27) = 3000.;
  mvector<float>::operator[](28) = 4000.;
  mvector<float>::operator[](29) = 5000.;
}

#endif
