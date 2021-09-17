#include <stdio.h>

#include "ncepgrids.h"

int main(void) {
  northgrid<unsigned char> n;
  southgrid<unsigned char> s;
  FILE *finn, *fins;
  palette<unsigned char> h(25);
  int i, rate;

  finn = fopen("ncount","r");
  fins = fopen("scount","r");
  n.binin(finn);
  s.binin(fins); 

  rate = n.gridmax();
  rate = max(rate, s.gridmax() );
  rate = 255 / rate;
  printf("Max number of obs, %d, color rate %d\n",
               max(n.gridmax(), s.gridmax() ), rate);
  
  for (i = 0; i < 25; i++) {
    h.set_color(i, rate*i, rate*i, rate*i);
  }

  n.xpm("ncount.xpm",1,h);
  s.xpm("scount.xpm",1,h);

  return 0;
}
