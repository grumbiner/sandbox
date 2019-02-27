#include <stdio.h>
#include <Quickdraw.h>
#include <WindowMgr.h>

      main()
{
    int x[1023], y[1023], *px, *py;
    int i, radius, npts, xnot, ynot;
    float eccen;
    int fontnum, fontsize, color, pwid, pht;
    
/* Toolbox declarations */
    WindowPtr window, text_window, view_window;
    Rect vrect, trect;
    WindowRecord view_record, text_record;
/* Note that the initializations must be called explicitly */    
    InitMacintosh();
    
/* Note that there is no default window */
 	trect.top    =  40;
 	trect.bottom = 480;
 	trect.left   =   1;
 	trect.right  = 640;
 	text_window = NewWindow(0L, &trect,"\pText",true,0,-1L,true,1);

 	vrect.top    =  40;
 	vrect.bottom = 480;
 	vrect.left   =   1;
 	vrect.right  = 640;
 	view_window = NewWindow(0L,&vrect,"\pView",true,0,-1l,true,0);

    SetPort(text_window);
    fontnum = 4;
    fontsize = 9;
    color = 409;
    pwid = 3;
    pht = 3;
    TextFont(fontnum);
    TextSize(fontsize);
    ForeColor(color);
    ShowPen();
    PenSize(pwid,pht);
    
    DrawString("\p test of draw string"); 
    
    DrawString("\pwhat is npts\n");
    scanf ("%d", &npts);
    DrawString("\pwhat is radius\n");
    scanf ("%d", &radius);
    DrawString("\pwhat is eccentricity\n");
    scanf ("%f", &eccen);
    printf("npts  %d\n",npts);
    printf("radius %d\n",radius);
    printf("eccentricity %f\n",eccen);

    px = &x[0];
    py = &y[0];
    xnot = 200;
    ynot = 200;
    npts = 50;
    radius = 150;
    eccen = 0.5;
    ellipse(&x[0], &y[0], npts, xnot, ynot, radius, eccen);
    for (i = 0; i< npts; i++)
    {
      printf("%d %d\n",x[i], y[i]);
     }
    printf(" all done\n");
    printf ("%ld tics\n",Ticks);

}
