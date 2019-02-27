#include <stdio.h>
#include <pascal.h>

void InitMacintosh(void);

void WindowSet(void);
void TestWindows(void);
void event_loop(void);

#define firstwindow 128
#define nwindows 3
#define kmovetofront  (WindowPtr)-1L

/* Define the windows to be known globally, and pointers to the offset */
WindowPtr window[nwindows];
#define ROCKET_VIEW 0
#define FLIGHT_VIEW 1
#define CURRENT_SETTINGS 2


/*****
 * main()
 *	This is where everything happens
 *****/
void main()

{
	short fontNum, i;
	int ystart;
	Rect picturerect;
	PicHandle picture;
	WindowPtr pwind;
	
	InitMacintosh();
	WindowSet();
	TestWindows();

	GetCursor(128);
	ShowWindow(window[FLIGHT_VIEW]);
    SetPort(window[FLIGHT_VIEW]);

/* Illustrate some font behavior */
	GetFNum("\pTimes",&fontNum);
	if ( fontNum != 0 ) { TextFont(fontNum) ; } else {DrawString("\pFailed times");}
	
	ystart=50;
	for (i = 9; i < 12; i++) {
      ystart += i;
      TextSize(i);
      MoveTo(40,ystart);
      DrawString("\pIn window Flight View");
    }

/* Show how to display a Pict */
	ShowWindow(window[ROCKET_VIEW]);
    SetPort(window[ROCKET_VIEW]);
    pwind = FrontWindow();
    picturerect = pwind->portRect;
    picture = GetPicture(128);
    if (picture == nil) { SysBeep(10); ExitToShell(); }
    MoveTo(0,0);
    DrawPicture(picture, &picturerect);
   	
/* Dummy while to wait for a button to be pressed, then exit */
/*	while (!Button()); */
/* Begin consideration of Event loops */
	ShowWindow(window[FLIGHT_VIEW]);
    SetPort(window[FLIGHT_VIEW]);

	event_loop();
	while(!Button() );
	
}
/* end main */

void event_loop()
{
	EventRecord event;
	Point curloc;
	int done, nnull;
	RgnHandle tmpr;
	WindowPtr frwin;
	long winpart;
	char buf[80];

    SetPort(window[FLIGHT_VIEW]);
	ShowWindow(window[FLIGHT_VIEW]);
	SelectWindow(window[FLIGHT_VIEW]); /* Note that this is needed to bring window forward */
		
	frwin = FrontWindow();
	
	MoveTo(5 ,12);
	TextSize(10);
	done = (1==0);
	nnull = 0;
	
	while (!done) {
	  WaitNextEvent( everyEvent, &event, 1024, nil) ;
	  switch (event.what) {
	   case nullEvent :
	    nnull += 1;
	    break;
	   case mouseDown : 
		DrawString("\p Mouse Down");
		tmpr=NewRgn();
		ScrollRect(&frwin->portRect, 0, -10, tmpr);
		DisposeRgn(tmpr);
/* Handle possible close box */
		winpart = FindWindow(event.where, &frwin);
		if ( winpart == inGoAway ) { done = (1 == 1) ; }
		if (winpart == inDrag) { DragWindow(frwin, event.where, &screenBits.bounds) ; }
		break;
	   case mouseUp :
		DrawString("\p Mouse up");
		Move(-100,+10);
		break;
	   case keyDown :
	    GetPen(&curloc);
	    Move(-curloc.h, 0);
		DrawString("\p key Down");
		break; 
	   case keyUp : 
		DrawString("\p key up");
		break;
	   case autoKey :
		DrawString("\p auto key");
		done = (1==1);
		break;
	   case updateEvt :
  		DrawString("\p update evt");
  		BeginUpdate( (WindowPtr)event.message );
  		EndUpdate  ( (WindowPtr)event.message );
		break;
	   case diskEvt : 
		DrawString("\p disk evt");
		break;
	   case activateEvt :
		DrawString("\p activate evt");
		break;
	   case driverEvt :
		DrawString("\p driver evt");
		break;
	   case app1Evt :
		DrawString("\p app1 evt");
		break;
	   case app2Evt :
		DrawString("\p app2 evt");
		break;
	   case app3Evt :
		DrawString("\p app3 evt");
		break;
	   case osEvt :
		DrawString("\p os evt");
		break;
      }
    }

  GetPen(&curloc);
  Move(-curloc.h, 100); 
  sprintf(buf, "Number of Null Events %d", nnull);
  DrawString( CtoPstr(buf) ) ;
    
  GetPen(&curloc);
  Move(-curloc.h, 10); 
  DrawString("\pLeaving Loop");
  return;
}



void WindowSet(void)
{
  int i;
  
  for (i = 0; i < nwindows; i++)
  {
    window[i] = GetNewWindow(firstwindow+i, nil, kmovetofront);
    if (window[i] == nil)
    {
      SysBeep(10);
      ExitToShell();
    }
  }
  
}

void TestWindows(void)
{
  int i;
  
  for (i = 0; i < nwindows; i++)
  {
    ShowWindow(window[i]);
    SetPort(window[i]);
    MoveTo(40,40);
    DrawString("\p In window");
  }
  
}


/****
 * InitMacintosh()
 * Initialize all the managers & memory
 ****/
void InitMacintosh(void)
{
	MaxApplZone();
	InitGraf(&thePort);
	InitFonts();
	FlushEvents(everyEvent, 0);
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(0L);
	InitCursor();

}
/* end InitMacintosh */


