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
