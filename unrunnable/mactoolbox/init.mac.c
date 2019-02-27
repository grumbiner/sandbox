/****
 * InitMacintosh()
 *
 * Initialize all the managers & memory
 *
 ****/

InitMacintosh()

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
