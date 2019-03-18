#include <math.h>

void spiral(px, py, npts, xnot, ynot,turns, alpha)
int npts, xnot, ynot, *px, *py;
float turns, alpha;
{ 
  int i, *pxt, *pyt;
  float pi;
  
  pxt = px;
  pyt = py;
  
  *pxt = xnot;
  *pyt = ynot;
  pxt++;
  pyt++;
	pi = acos(-1.0);
	for (i = 1; i< npts; i++)
	 {
	  *pxt = xnot + i*alpha*cos(2.*pi*turns*i/(npts-1));
	  *pyt = ynot + i*alpha*sin(2.*pi*turns*i/(npts-1));
	  pxt++;
	  pyt++;
       }

}

