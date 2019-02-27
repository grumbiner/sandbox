void InitMacintosh(void);

void WindowSet(void);

void TestWindows(void);

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
	InitMacintosh();
	WindowSet();
	TestWindows();

	ShowWindow(window[ROCKET_VIEW]);
    SetPort(window[ROCKET_VIEW]);
    MoveTo(40,80);
    DrawString("\p In window Rocket View");

	
	/* Dummy while to wait for a button to be pressed, then exit */
	while (!Button());
}
/* end main */

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


