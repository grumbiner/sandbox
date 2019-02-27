#include <stdio.h>
#include <math.h>
#include <Quickdraw.h>
#include <WindowMgr.h>
/*extern long Ticks : 0x16A;*/
      main()
{
    int x[1023], y[1023], *px, *py;
    int i, radius, npts, xnot, ynot;
    float eccen;    

    printf("what is npts\n");
    scanf ("%d", &npts);
    printf("what is radius\n");
    scanf ("%d", &radius);
    printf("what is eccentricity\n");
    scanf ("%f", &eccen);
    printf("npts  %d\n",npts);
    printf("radius %d\n",radius);
    printf("eccentricity %f\n",eccen);

    px = &x[0];
    py = &y[0];
    xnot = 200;
    ynot = 200;

    ellipse(&x[0], &y[0], npts, xnot, ynot, radius, eccen);
    for (i = 0; i< npts; i++)
    {
      printf("%d %d\n",x[i], y[i]);
     }
    printf(" all done\n");
    printf ("%ld tics\n",Ticks);
    
}