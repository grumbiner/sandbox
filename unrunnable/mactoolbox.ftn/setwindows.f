	SUBROUTINE setwindows(view_window,   report_window, 
     1                      status_window, motion_window  )
     
	IMPLICIT none
	INCLUDE HD.I40:Macfortran020:Include Files:window.inc
*
*  The following structure defines a window record for making calls to the
*  Macintosh window manager. The two windows defined by this program will
*  not need to manipulate the window record, but the structure is defined
*  for your reference
*

	INTEGER*4 view_window		! pointer to the view window
	INTEGER*4 report_window		! pointer to the report window
	INTEGER*4 status_window		! pointer to status window
	INTEGER*4 motion_window		! pointer to movement window

	INTEGER*1 view_record(154)	! view window record
	INTEGER*1 report_record(154)	! report window record
	INTEGER*1 status_record(154)	! status window record
	INTEGER*1 motion_record(154)	! motion window record
	
	INTEGER*2 port(53)		! grafport
	INTEGER*2 windowkind		! type of window:
c					!   0 = standard document window
c					!   1 = alert box or modal dialog box
c					!   2 = plain box
c					!   3 = plain box with shadow
c					!   4 = document window without size box
c					!  10 = round cornered window
	LOGICAL*1 visible		! true when window is visible
	LOGICAL*1 hilite		! highlighted when true
	LOGICAL*1 goaway		! has a go away box when true
	LOGICAL*1 spareflag		! reserved	
	INTEGER*4 strucrgn		! Quickdraw region
	INTEGER*4 contrgn		! Quickdraw region
	INTEGER*4 updatergn		! Quickdraw region
	INTEGER*4 windowdefproc		! pointer to window definition function
	INTEGER*4 datahandle		! pointer for window definition function
	INTEGER*4 titlehandle		! pointer to title
	INTEGER*4 titlewidth		! width (in pixels) of title
	INTEGER*4 controllist		! pointer to control list for Control Mgr
	INTEGER*4 nextwindow		! pointer to next window in window list
	INTEGER*4 windowpic		! pointer to Quickdraw picture of contents
	INTEGER*4 refcon		! reference value field
	
	equivalence (view_record(1),port)
	equivalence (view_record(104),windowkind)
	equivalence (view_record(109),visible)
	equivalence (view_record(110),hilite)
	equivalence (view_record(111),goaway)
	equivalence (view_record(112),spareflag)
	equivalence (view_record(113),strucrgn)
	equivalence (view_record(114),contrgn)
	equivalence (view_record(121),updatergn)
	equivalence (view_record(125),windowdefproc)
	equivalence (view_record(129),datahandle)
	equivalence (view_record(133),titlehandle)
	equivalence (view_record(137),titlewidth)
	equivalence (view_record(139),controllist)
	equivalence (view_record(143),nextwindow)
	equivalence (view_record(147),windowpic)
	equivalence (view_record(151),refcon)
	
C	
C	Attach a part of the quickdraw include file for setting typeface ...
*
*  Quickdraw (portions)

        integer INITGRAF,OPENPORT,INITPORT,CLOSEPORT,SETPORT,GETPORT,
     +          GRAFDEVICE,SETPORTBITS,PORTSIZE,MOVEPORTTO,SETORIGIN,
     +          SETCLIP,GETCLIP,CLIPRECT,BACKPAT
        integer INITCURSOR,SETCURSOR,HIDECURSOR,SHOWCURSOR,OBSCURECURSOR
        integer HIDEPEN,SHOWPEN,GETPEN,GETPENSTATE,SETPENSTATE,PENSIZE,
     +          PENMODE,PENPAT,PENNORMAL,MOVETO,MOVE,LINETO,LINE
        integer TEXTFONT,TEXTFACE,TEXTMODE,TEXTSIZE,SPACEEXTRA,DRAWCHAR,
     +          DRAWSTRING,DRAWTEXT,CHARWIDTH,STRINGWIDTH,TEXTWIDTH,
     +          GETFONTINFO
        parameter (INITGRAF=Z'86E10000',OPENPORT=Z'86F10000',
     +             INITPORT=Z'86D10000',CLOSEPORT=Z'87D10000',
     +             SETPORT=Z'87310000',GETPORT=Z'87430000',
     +             GRAFDEVICE=Z'87208000',SETPORTBITS=Z'87530000',
     +             PORTSIZE=Z'87609000',MOVEPORTTO=Z'87709000',
     +             SETORIGIN=Z'87809000',SETCLIP=Z'87910000',
     +             GETCLIP=Z'87A10000',CLIPRECT=Z'87B30000',
     +             BACKPAT=Z'87C30000')
        parameter (INITCURSOR=Z'85000000',SETCURSOR=Z'85130000',
     +             HIDECURSOR=Z'85200000',SHOWCURSOR=Z'85300000',
     +             OBSCURECURSOR=Z'85600000')
        parameter (HIDEPEN=Z'89600000',SHOWPEN=Z'89700000',
     +             GETPEN=Z'89A30000',GETPENSTATE=Z'89830000',
     +             SETPENSTATE=Z'89930000',PENSIZE=Z'89B09000',
     +             PENMODE=Z'89C08000',PENPAT=Z'89D30000',
     +             PENNORMAL=Z'89E00000',MOVETO=Z'89309000',
     +             MOVE=Z'89409000',LINETO=Z'89109000',LINE=Z'89209000')
        parameter (TEXTFONT=Z'88708000',TEXTFACE=Z'88808000',
     +             TEXTMODE=Z'88908000',TEXTSIZE=Z'88A08000',
     +             SPACEEXTRA=Z'88E10000',DRAWCHAR=Z'88308000',
     +             DRAWSTRING=Z'88430000',DRAWTEXT=Z'88511200',
     +             CHARWIDTH=Z'88D48000',STRINGWIDTH=Z'88C70000',
     +             TEXTWIDTH=Z'88651200',GETFONTINFO=Z'88B30000')
C
	INTEGER PTR
      PARAMETER (PTR=Z'C0000000')
	
C	Non-window terms used
	INTEGER*4 toolbx
	INTEGER*2 rect(4)
	character*256 str255	! function to create a Pascal LSTRING
	CHARACTER*20 name
	INTEGER*4 window		! general purpose pointer
	
C	Save the window records so that they will be available later
	SAVE view_record
	SAVE report_record
	SAVE status_record
	SAVE motion_record
*
*  Create the windows, only display the view window; the SETPORT call
*  allows FORTRAN I/O to take place:
*
	view_window = toolbx(PTR,view_record)
	rect(1) =  39
	rect(2) =   1
	rect(3) = 300
	rect(4) = 460
	view_window = toolbx(NEWWINDOW,view_window,rect,
     +		     str255("view"),.true.,0,-1,.true.,0)
	call toolbx(SETPORT,view_window)
	call toolbx(TEXTFONT,4)		! monaco
	call toolbx(TEXTSIZE,9)		! 9 point
	
	report_window = toolbx(PTR,report_record)
	rect(1) =  39
	rect(2) = 461
	rect(3) = 300
	rect(4) = 639	
	report_window = toolbx(NEWWINDOW,report_window,rect,
     +			str255("Reports"),.true.,0,view_window,.true.,0)
     
	status_window = toolbx(PTR,status_record)
	rect(1) = 321
	rect(2) = 321
	rect(3) = 479
	rect(4) = 639
	status_window = toolbx(NEWWINDOW,status_window,rect,
     1                  str255("Status"),.true.,0,view_window,.true.,0)

	ENTRY reset_motion
	
	motion_window = toolbx(PTR,motion_record)
	rect(1) = 320
	rect(2) = 1
	rect(3) = 479
	rect(4) = 320
	name = "Motion"
	motion_window = toolbx(NEWWINDOW,motion_window,rect,
     1                  str255(name),.true.,0,view_window,.true.,0)

 9001 FORMAT (I3)
 9002 FORMAT (A20)
 
	RETURN
	END