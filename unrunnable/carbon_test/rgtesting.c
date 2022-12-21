/*  Includes  */
#include "CarbonPrefix.h"

/*  Defines  */
static 	BitMap	globalBitMap;			//declared here to use in a #define (for carbonization)
#define	TOTALCOLORS	256
#define	WWIDTH		(TOTALCOLORS * 2)
#define	WHALFWIDTH	TOTALCOLORS
#define	WHEIGHT		TOTALCOLORS

//Notice that the old QuickDraw treatment is replaced by a newer version:
//#define WLEFT		(((qd.screenBits.bounds.right - qd.screenBits.bounds.left) - WWIDTH) / 2)
//#define WTOP		(((qd.screenBits.bounds.bottom - qd.screenBits.bounds.top) - WHEIGHT) / 2)
#define WLEFT		(((GetQDGlobalsScreenBits(&globalBitMap)->bounds.right - GetQDGlobalsScreenBits(&globalBitMap)->bounds.left) - WWIDTH) / 2)
#define WTOP		(((GetQDGlobalsScreenBits(&globalBitMap)->bounds.bottom - GetQDGlobalsScreenBits(&globalBitMap)->bounds.top) - WHEIGHT) / 2)

/*  Global Variable Definitions  */
// What is recommended way to not need palette?
WindowPtr			gWindow;

/*  Procedure Prototypes  */
void initMac( void );
void createWindow( void );
void createImage( void );
void doEventLoop( void );

/*             */
/*  Main loop. */
/*             */

int main()  {
	initMac();
	createWindow();	
	createImage();
	doEventLoop();
        
    return 0;
}


/*                      */
/*  Mac initialization. */
void initMac( void )
{
//Notice the many inits that are no longer needed
	MoreMasterPointers(2);
	InitCursor();
	FlushEvents( 0, everyEvent );
}


/*                                                                     */
/*  Create the window to display the images and set the palette to it. */
/*                                                                     */
void createWindow( void )
{
	Rect	bounds;
	
	SetRect( &bounds, WLEFT, WTOP, WLEFT + WWIDTH, WTOP + WHEIGHT );
	gWindow = NewCWindow( 0L, &bounds, "\pPalette&RG testing", true, documentProc,
							(WindowPtr)-1L, true, 0L );						
// Notice that the simple SetPort is gone -- replaced by a longer-named function
	SetPortWindowPort( gWindow );

}


/*                                                              */
/*  Create theimage                                             */
void createImage( void )
{
	int             i;
	RGBColor	color;
	FontInfo	info;
	Str255		string = "\p RG messing with strings";
	short cdel;
    //these tend to be globals, but I'm moving it around
    short				gMode    = srcOr;
    short				gText    = kFontIDTimes;
    short				gSize    = 24;
	
	PenSize ( 1, 1 );
	
      color.red = 0;   color.green = 65535;    color.blue = 0;
      cdel = 65536  / TOTALCOLORS / 2;
      cdel -= 1;
        
	for (i = 0; i < TOTALCOLORS*2; i++){
        color.red   += cdel;     color.green -= cdel;    color.blue  += cdel;

		RGBForeColor( &color );
		MoveTo( i, 0 );
                LineTo( WHALFWIDTH, WHEIGHT );  //line draws with delta position, lineto goes there
	}
	
	/*  Draw label; RGBForeColor should now be black. */
        color.red = 0;  color.green = 0;    color.blue = 0;
        RGBForeColor( &color );
	TextMode( gMode );
	TextFont( gText );
	TextSize( gSize );		
    GetFontInfo( &info );
	MoveTo( (WHALFWIDTH - StringWidth(string))/2 + WHALFWIDTH/2, WHEIGHT/2 - info.ascent/3 );
	DrawString( string );
	
}

/*              */
/*  Event loop. */
/*              */

void doEventLoop( void )
{
	EventRecord event;
	WindowPtr   window;
	short       clickArea;
	Rect        screenRect;
    Point pt;

	for (;;){
		if (WaitNextEvent( everyEvent, &event, 0, nil )){
			if (event.what == mouseDown){
				clickArea = FindWindow( event.where, &window );
                pt  = event.where;
                GlobalToLocal(&pt);
                printf("mouse location in local coordinates %d %d\n",pt.h, pt.v);
				
				if (clickArea == inDrag){
					GetRegionBounds(GetGrayRgn(), &screenRect);
					DragWindow( window, event.where, &screenRect );
				}
				else if (clickArea == inContent){
					if (window != FrontWindow())
						SelectWindow( window );
				}
				else if (clickArea == inGoAway)
					if (TrackGoAway( window, event.where ))
						return;
			}
			else if (event.what == updateEvt){
				window = (WindowPtr)event.message;
				SetPortWindowPort( window );			
				BeginUpdate( window );
				EndUpdate( window );
			}
            else if (event.what == keyDown) {
            
            }
            else if (event.what == keyUp) {
              printf("key up at %d ",(int)event.when);
              printf("keyup masked %d  %d ", (int) event.message & charCodeMask, (int)(event.message & keyCodeMask)/256);
              if (isalpha(event.message & charCodeMask) ) {
                printf("yes, I'm a character %c\n",(char) (event.message & charCodeMask) );
              }
              else {
                printf("no, I'm not\n");
              }
            }
		}
	}
}