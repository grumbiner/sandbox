#include <stdio.h>
#include <math.h>
#include <MacTypes.h>
#include <Quickdraw.h>
#include <WindowMgr.h>

/* Note that window information must be declared hyper-globally */
extern WindowPtr	view_window;
extern WindowRecord view_record;
extern RgnHandle	alpha;

main()
{
	int x[256], y[256];
	int i, j, npts;

	/* Toolbox declarations */
	Rect		 trect, 	  vrect, *prect;
	RgnHandle	 *palpha;
	PolyHandle	 hubert;
	Pattern 	 beta;

	/* Note that the initializations must be called explicitly */
	InitMacintosh();

	/* Note that there is no default window */
	startwindow();

	/* Set up an Ellipse */
	/* Note Usage of Drawstring \p is required to notify routine of the fact
	that the argument is supposed to be a Pascal string */
	npts = 40;
	MoveTo(1,36);
	DrawString("\pcalling ellipse");
	ellipse(&x[0], &y[0], npts, 200, 210, 100, 0.0);
	MoveTo(1,48);
	DrawString("\preturned from ellipse");

	/* Draw the Ellipse -- example of moveto, lineto*/
	/*	  MoveTo(x[0], y[0]);
	for (i = 1; i< npts; i++)
	{ LineTo(x[i], y[i]);} */

	/* Polygon operations example */
	polyex(&x[0], &y[0], npts);

	/* Scrolling operations example */
	/*	  scrollex();*/

	/* Picture Creation Example */
	/*	  pictex();*/

	/* Rectangle Operations example */
	/*	  rectex();*/

	/* Oval Operations example */
	/*	  ovalex();*/

	/* RoundRect Operations example not given. */
	/* Region and Arc examples not given, do not look handy */

	MoveTo(1,72);
	DrawString("\pall done");

}
