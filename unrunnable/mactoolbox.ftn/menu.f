	PROGRAM demo
C	Initial framework from absoft program demo.  most of code changed/deleted
C	since that point
	IMPLICIT none			! helps keep us out of trouble

	INCLUDE HD.I40:Macfortran020:demobg.inc
	
	INTEGER teinit, addresmenu
	PARAMETER (teinit     = z'9CC00000')
	PARAMETER (addresmenu = z'94D12000')
	
*
*  The following structure defines an event record for making calls to the
*  Macintosh event manager. Events are occurances such as the detection of
*  a change in the state of the mouse button, capturing keyboard input, and
*  detecting the insertion of a disk. You can specify the specific events
*  that your application is interested in handling. A typical call to the
*  event manager for returning the next event is:
*
*	eventflag = toolbx(GETNEXTEVENT,eventmask,eventrecord)
*

	INCLUDE HD.I40:Macfortran020:Include Files:event.inc

*
C	Pointers alone may be used to reference the window, BUT
C	The related record must be saved in the subroutine which creates the window
*

	INTEGER*4 view_window		! pointer to the view window
	INTEGER*4 report_window		! pointer to the report window
	INTEGER*4 status_window		! pointer to status window
	INTEGER*4 motion_window		! pointer to movement window

*
*  Miscellaneous declarations
*

	INTEGER*4 toolbx		! the tool box interface
	character*256 str255	! function to create a Pascal LSTRING
	INTEGER*4 window		! general purpose pointer
	INTEGER*4 size,w,h	! for growing windows
	INTEGER*2 rect(4)		! rectangle coordinates
	INTEGER mouseloc		! mouse location from FINDWINDOW:
c					!   0 = none of the following
c					!   1 = in the menu bar
c					!   2 = in system window
c					!   3 = in content region
c					!   4 = in drag region
c					!   5 = in grow region
c					!   6 = in go away region


*
*  Set up the event manager mask (you should accept responsibility for all
*  events to insure that the event queue is flushed; some calls such as 
*  MENUSELECT will not work properly if there are extra mouse up events
*  lying around):
*
	eventmask = -1

*	
*  Close MacFortran I/O window (never make a DISPOSEWINDOW call on this
*  window):
*

	window = toolbx(FRONTWINDOW)
	call toolbx(CLOSEWINDOW,window)
	
C
C  Establish the menus for the program
C
	CALL setmenus

*
*  Create the windows, only display the view window; the SETPORT call
*  allows FORTRAN I/O to take place:
*
	CALL setwindows(view_window,   report_window, 
     1                status_window, motion_window )	
	
*
*  main event processing loop
*
*  constraints on window dragging:
*
	rect(1) =  19
	rect(2) =   1
	rect(3) = 479
	rect(4) = 639
	
	
	  do
	    if (toolbx(GETNEXTEVENT,eventmask,eventrecord)) then
	    
	      select case (what)
	      
	        case (1)		! mouse down

		  mouseloc = toolbx(FINDWINDOW,where,window)

		    if (mouseloc=1) then
		      call menus(where,view_window,   report_window,
     1				     status_window, motion_window)
		      
		    else if (mouseloc=3) then
		      call toolbx(SELECTWINDOW,window)
		      
		    else if (mouseloc=4) then 
		      call toolbx(DRAGWINDOW,window,where,rect)
		      
		    else if (mouseloc=5) then
		      size = toolbx(GROWWINDOW,window,where,rect)
		      w = size .and. z'ffff'
		      h = shift(size,-16)
		      call toolbx(SIZEWINDOW,window,w,h,.true.)
		      
		    else if (mouseloc=6) then
		      if (toolbx(TRACKGOAWAY,window,where))
     +		          call toolbx(HIDEWINDOW,window)
		    end if
	
	        case default

C		do nothing
		  
	      end select
	    end if
	  repeat
	end
*
* menus: a mouse down event was detected in the menu area; process menu
*	 selection
*

	SUBROUTINE menus(where,view_window,   report_window,
     1                       status_window, motion_window  )
	
	IMPLICIT none
	
	INCLUDE HD.I40:Macfortran020:demobg.inc

	INTEGER*4 toolbx
	
	INTEGER*2 where(2)		! mouse location from the event record
	INTEGER view_window		! view window pointer
	INTEGER report_window		! report window pointer
	INTEGER status_window		! status window pointer
	INTEGER motion_window		! motion window pointer
	INTEGER window
	
*	
*  variables for making menu selections
*
	INTEGER*2 menuselection(2)	! menu selection information
c						!  (1) = menuid
c						!  (2) = menu item number
	INTEGER*4 menudata, item4	! for use left of equals sign
	
	equivalence (menuselection,menudata)
	
*
*  Menu selection constants:
*

	INTEGER Commands,Windows,Expts,Apple		! menus
	INTEGER Chain, Play, Exit				! "Commands" menu selections
	INTEGER view,reports,status,motion			! "Windows" menu selections
	INTEGER Place,Position,Distance,Class,Sense	! Expts menu selections

	parameter (Apple=1, Commands=30, Windows=31, Expts=32)	
	parameter (Chain=1, Play= 2, Exit=3)
	parameter (view=1, reports=2, status=3 , motion=4)
	parameter (Place=1, Position=2, Distance=3, Class=4, Sense=5)

	
	CHARACTER*20 name
	INTEGER GETITEM, GETMENU, OPENDESKACC, menuhandle, refnum
	PARAMETER (GETITEM = z'94611C00')
	PARAMETER (GETMENU = z'9BF88000')
	PARAMETER (OPENDESKACC = z'9B670000')
	
*
*  The MENUSELECT tool box call handles the messy details of highlighting
*  menus and menu selections, pulling menus down, and determining which
*  item was selected. The HILITEMENU call at the end of the select case
*  blocks actually unhighlights the menus.
*

	menudata = toolbx(MENUSELECT,where)
	
	  select case (menuselection(1))
	  
	    case (Commands)		! the "Commands" menu was selected

	      call toolbx(SELECTWINDOW,view_window)

	        select case (menuselection(2))
		
		  case (Chain)
		    call chain
		  
		  case (Play)
		    call reset_motion
		     
		  case (Exit)
		    stop
		
		end select
	    
              call toolbx(HILITEMENU,0)
	
	    case (Windows)		! the "Windows" menu was selected
	    
	        select case (menuselection(2))
		
		  case (view)
		    window = view_window
		    
		  case (reports)
		    window = report_window
		    
		  case (status)
		    window = status_window
		    
		  case (motion)
		    window = motion_window

		  end select
		  
	        call toolbx(SHOWWINDOW,window)
	        call toolbx(SELECTWINDOW,window)
              call toolbx(HILITEMENU,0)

C		Grumbine test	      
	      case (expts)
	        select case(menuselection(2))
		  case (Place)
		    PRINT *,'Called place'
		  case (Position)
		    PRINT *,'Called position'
		  case (Distance)
		    PRINT *,'Called distance'
		  case (Class)
		    PRINT *,'giving class information'
		  case (Sense)
		    PRINT *,'giving sensory information'
		  end select
              call toolbx(HILITEMENU,0)
	      
	      case (Apple)
	      	menuhandle = toolbx(GETMENU,1)
			item4 = menuselection(2)
	      	CALL toolbx(GETITEM,menuhandle,item4,name)
			refnum = toolbx(OPENDESKACC,name)
			PRINT *,'info from apple menu'
			PRINT *,'menuhandle = ', menuhandle
			PRINT *,'name = ', name
			PRINT *,'refnum = ', refnum
		      call toolbx(HILITEMENU,0)
		
	    case default		! just playing with the mouse
	    
	  end select
	  
	end

*
*  chain: transfer control to specified application (the application had
*	  better be executable)
*

	SUBROUTINE chain
	
	IMPLICIT none

	character*64 filename		! name of file to execute
	
	
	call getfil('APPL', filename)
	if (filename = " ") return
	
 	execute trim(filename)
	 
	end
*
*  getfile: prompt user for file name and look it up
*

	SUBROUTINE getfil(ftype, filename)
	
	IMPLICIT none
	
	INCLUDE HD.I40:Macfortran020:demobg.inc
	
	INTEGER*4 toolbx
	
	INTEGER*1 reply(76)
	INTEGER*2 good
	character*4 ostype
	INTEGER*2 vrefnum
	INTEGER*2 version
	character*64 fname
	INTEGER flength
	character*64 filename

	equivalence (reply(1), good)
	equivalence (reply(3), ostype)
	equivalence (reply(7), vrefnum)
	equivalence (reply(9), version)
	equivalence (reply(11), fname)
	
	INTEGER*4 where
	INTEGER*2 wh(2)
	equivalence (wh(1), where)
	
	LOGICAL exist
	
* File parameter block.  Used to set the default volume in response to a
* Standard File call.
	INTEGER*1 params(80)		! Enough for all variants.
	INTEGER   iolink
	INTEGER*2 iotype
	INTEGER*2 iotrap
	INTEGER   iocmdaddr
	INTEGER   iocompletion
	INTEGER*2 ioresult
	INTEGER   ionameptr
	INTEGER*2 iovrefnum
	
	equivalence (params(1), iolink)
	equivalence (params(5), iotype)
	equivalence (params(7), iotrap)
	equivalence (params(9), iocmdaddr)
	equivalence (params(13), iocompletion)
	equivalence (params(17), ioresult)
	equivalence (params(19), ionameptr)
	equivalence (params(23), iovrefnum)
	
	INTEGER filerr
	
	wh(1) = 50
	wh(2) = 50

	call toolbx(SFGETFILE, where, 0, 0, 1, 
     +	  toolbx(PTR, ftype), 0, reply, 2)

	if (good = 0) then
	  filename = ' '
	  return
	endif
	flength = ichar(fname(1:1))
	filename = fname(2:flength+1)
	ionameptr = 0
	iovrefnum = vrefnum
	filerr = toolbx(PBSETVOL, 
     +	  toolbx(PTR, params))
	end
	
	
*
*  clrtty: clear the TTY window and postion the pen at 3,12 (v,h)
*

	SUBROUTINE clrtty
	
	INCLUDE HD.I40:Macfortran020:demobg.inc

	INTEGER*2 rect(4)
	
	data rect /1,1,300,500/
	
	call toolbx(ERASERECT,rect)
	call toolbx(MOVETO,3,12)
	
	end


*
*  str255: converts a FORTRAN string to a Pascal LSTRING
*

	character*256 function str255(string)

	character*(*) string
	
	str255 = char(len(trim(string)))//string
	
	end
*
* This routine force the compiler to expand the heap by 2k bytes so that
* the spool SUBROUTINE can be loaded by the print command.
* If this heap space is no allocated in this manner or with the linker
* the program will give an error 64 (out of memory) when the print option is
* selected from the menu.
*

	SUBROUTINE dum
	INTEGER*2 ary(1000)
	common ary
	return
	end
	
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
	SUBROUTINE setmenus
	 
	IMPLICIT none			! helps keep us out of trouble

	INCLUDE HD.I40:Macfortran020:Include Files:menu.inc
	
	INTEGER teinit
	PARAMETER (teinit     = z'9CC00000')

	INTEGER*4 menuhandle		! pointer for creating menus
	INTEGER*4 toolbx
	CHARACTER*256 str255
*
*  Build the menus (for details refer to "Menu Initialization and
*  Allocation" in "The MacFortran Tool Box Interface"):
*
C	Set up the APPLE menu BG (from desk manager, p. 8)
	CALL toolbx(TEINIT)
	menuhandle = toolbx(NEWMENU,1, char(1)//char(z'14') )
	CALL toolbx(ADDRESMENU, menuhandle, 'DRVR')
	CALL toolbx(INSERTMENU, menuhandle, 0     )

	menuhandle = toolbx(NEWMENU,30,str255("Commands"))
	call toolbx(APPENDMENU,menuhandle,
     +		    str255("Chain;Play;Exit"))
	call toolbx(INSERTMENU,menuhandle,0)		! insert at end of list
	
	menuhandle = toolbx(NEWMENU,31,str255("Windows"))
	call toolbx(APPENDMENU,menuhandle,
     +		    str255("View;Reports;Status;Motion"))
	call toolbx(INSERTMENU,menuhandle,0)		! insert at end of list
	
	menuhandle = toolbx(NEWMENU,32,str255("Expts"))
	CALL toolbx(APPENDMENU,menuhandle,
     1              str255("Place;-Position;Distance;Class;Sense"))
     	CALL toolbx(INSERTMENU,menuhandle,0)
	
	call toolbx(DRAWMENUBAR)
	
	RETURN
	END