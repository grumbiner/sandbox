#include <stdio.h>
#  #include <pascal.h>
#include <stdlib.h>
#include <math.h>
#include "rocket.h"


void InitMacintosh(void);

void WindowSet(void);
void TestWindows(void);
void event_loop( rocket *r);
void MenuBarInit(void);
void HandleMouseDown( EventRecord *eventPtr, rocket *r);
void HandleUpdate(EventRecord *event);

void HandleMenuChoice(long choice, rocket *r);
void HandleApple(short item);
void HandleFile(short item);
void HandleFly(short item, rocket *r);

#define firstwindow 128
#define nwindows 3
#define kmovetofront  ((void *) -1L)

/* Define the windows to be known globally, and pointers to the offset */
WindowPtr window[nwindows];
#define ROCKET_VIEW 0
#define FLIGHT_VIEW 1
#define CURRENT_SETTINGS 2

/* Begin Defining the Menu elements */
#define mApple 134
#define mFile  136
#define  iOpen   1
#define  iClose  2
#define  iSave   3
#define  iAs     4
#define  iSetup  6
#define  iPrint  7
#define  iQuit   9

#define mEdit   137
#define mCreate 128
#define mBody	140
#define mEngine	132
#define mPara	130
#define mNose	129
#define mFly	135

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
	char buf[20];
	Point curloc;
	rocket *r;
	long tstart, tfin;
	CCrsrHandle cur;
	
/* Initializations */
	InitMacintosh();
/* Set up the windows */
	WindowSet();
	
	MenuBarInit();

/* Get a program-specific cursor */
	cur = GetCCursor(128) ;
	if (cur == NULL) { SysBeep(10) ; return ; }
	SetCCursor(cur);

/* Illustrate some font behavior */
	GetFNum("\pTimes",&fontNum);
	if ( fontNum != 0 ) { TextFont(fontNum) ; } else {DrawString("\pFailed times");}
   	
/* Begin consideration of Event loops */
	ShowWindow(window[FLIGHT_VIEW]);
    SetPort(window[FLIGHT_VIEW]);
    SelectWindow(window[FLIGHT_VIEW]);
    HiliteWindow(window[FLIGHT_VIEW], TRUE);
    
    r = new rocket;
	r->set();
	
	event_loop(r);

	r->showPos(); 
	
	while (!Button());
	
}
/* end main */
void MenuBarInit(void)
{
  Handle menuBar;
  MenuHandle menu;
  ControlHandle control;
  long feature;
  OSErr myerr;
  
  menuBar = GetNewMBar( 128 );
  SetMenuBar(menuBar);
  
  menu = GetMHandle( mApple );
  AddResMenu ( menu, 'DRVR');
  
  DrawMenuBar();
  
  return;
  
}

void HandleMouseDown( EventRecord *event, rocket *r)
{
  WindowPtr which;
  GrafPtr   oldPort;
  short		thePart;
  long		menuChoice;
  ControlHandle	control;
  short		ignored;
  
  thePart = FindWindow( event->where, &which );
  switch(thePart) {
    case inMenuBar:
    	menuChoice = MenuSelect( event->where ); /* Returns a long which indicates 
    	                                       both the menu and the item on that menu */
    	HandleMenuChoice(menuChoice, r);
    	break;
    case inSysWindow:
    	SystemClick(event, which);
    	break;
    case inContent:
    	SetPort(which);
		break;
	case inDrag :
		DragWindow(which, event->where, &screenBits.bounds);
		break;
	case inZoomIn:
	case inZoomOut:
		DrawString("\p Clicked in zoom in/out");
		break;
  }

}

void HandleMenuChoice(long choice, rocket *r)
{    	
	char buf[20];
	WindowPtr frwin;
	Point curloc;
	short menu, item;
	
	frwin = FrontWindow();
    GetPen(&curloc);
    Move(-curloc.h, 10); 
	sprintf(buf, "%d %d", HiWord(choice), LoWord(choice) );
	DrawString(CtoPstr(buf));
	
	if (choice != 0) {
	  menu = HiWord(choice);
	  item = LoWord(choice);
	  switch (menu) {
	    case mApple:
	    	HandleApple(item);
	    	break;
	    case mFile:
	    	HandleFile(item);
	    	break;
	    case mEdit:
	 /*   	HandleEdit(item);*/
	    	break;
	    case mCreate:
	 /*   	HandleCreate(item);*/
	    	break;
	    case mFly:
	    	HandleFly(item,  r);
	    	break;
	   }
	}
}

void HandleFly(short item, rocket *r) 
{ long start, fin;
  char buf[20];
  Point curloc;
  
  start = TickCount();
  if (item == 4) {
    r->launch(1.5);
  }
  fin = TickCount();
  
  sprintf(buf," %d", (int)(fin - start) );
  GetPen(&curloc);
  MoveTo(0, 50);
  DrawString( CtoPstr(buf) );
  
  return;
}

void HandleFile(short item)
{
  switch(item) {
    case iOpen :
    	DrawString("\p Open");
    	break;
    case iClose :
    	DrawString("\p Close");
    	break;
	case iSave : 
		DrawString("\p Save");
		break;
	case iAs :
		DrawString("\p Save As");
		break;
	case iSetup :
		DrawString("\p Setup");
		break;
	case iPrint :
		DrawString("\p Print");
		break;
	case iQuit :
		DrawString("\p Quit");
		ExitToShell ();
	}   	
}

void HandleApple(short item)
{
  MenuHandle appleMenu;
  Str255	accName;
  short 	accNumber;
  
  switch(item) {
    case 1:
    	DrawString("\p About Rocket, c. 1995 R. W. Grumbine and A. G. Haliburton");
    	break;
    default:
    	appleMenu = GetMHandle( mApple);
    	GetItem (appleMenu, item, accName);
    	accNumber = OpenDeskAcc(accName);
    	break;
   }
   
}

void HandleUpdate(EventRecord *event)
{
  WindowPtr window;
  Str255 timeString;
  unsigned long curTimeInSecs;
  
  window = (WindowPtr)event->message;
  BeginUpdate(window) ;
  MoveTo(5,5);
  SetPort(window);
  ShowWindow(window);
  SelectWindow(window);
  HiliteWindow(window, TRUE);
  EndUpdate(window);
  
}

void event_loop( rocket *r)
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
	HiliteWindow(window[FLIGHT_VIEW], TRUE);
	
	frwin = FrontWindow();
	
	MoveTo(5 ,12);
	TextSize(10);
	done = (1==0);
	nnull = 0;
	
	while (!done) {
	  WaitNextEvent( everyEvent, &event, 1024*32-1, NULL) ;
	  switch (event.what) {
	   case nullEvent :
	    nnull += 1;
	    break;
	   case mouseDown : 
	    HandleMouseDown(&event, r);
		break;
	   case mouseUp :
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
	    HandleUpdate(&event);
		break;
	   case diskEvt : 
		break;
	   case activateEvt :
		break;
	   case driverEvt :
		break;
	   case app1Evt :
		break;
	   case app2Evt :
		break;
	   case app3Evt :
		break;
	   case osEvt :
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
    window[i] = GetNewWindow(firstwindow+i, NULL, kmovetofront);
    if (window[i] == NULL)
    {
      SysBeep(10);
      ExitToShell();
    }
  }
  
}

/****
 * InitMacintosh()
 * Initialize all the managers & memory -- Symantec program fragment
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
