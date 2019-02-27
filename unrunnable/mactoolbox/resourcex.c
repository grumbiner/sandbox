#include <stdio.h>
#include <math.h>
#include <MacTypes.h>
#include <ResourceMgr.h>
#include <Quickdraw.h>
#include <WindowMgr.h>

/* Note that window information must be declared hyper-globally */
extern WindowPtr    view_window;
extern WindowRecord view_record;
extern RgnHandle    alpha;

      main()
{
    int x[256], y[256];
    int i, j, npts;

/* Toolbox declarations */
    PolyHandle   hubert;
    
/* Note that the initializations must be called explicitly */
    InitMacintosh();

/* Note that there is no default window */
    startwindow();

/* Resource stuff example */
   resourcer();
       
/* Polygon operations example */
    npts = 40;
    ellipse(&x[0], &y[0], npts, 200, 200, 50, .7);
    polyex(&x[0], &y[0], npts);

    MoveTo(1,72);

}