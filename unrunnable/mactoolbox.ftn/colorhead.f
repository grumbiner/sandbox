*
*  The following is a demo program designed to illustrate the use of
*   the palette manager. It creates a color window and a palette and
*  changes the palette colors based on menu selection
*
* Obviously, this program only works on a Mac II
*
	implicit none			! helps keep us out of trouble

	include :include files:window.inc
	include :include files:quickdraw.inc
	include :include files:misc.inc
	include :include files:palette.inc
	include :include files:event.inc
	include :include files:menu.inc

	integer*4 color_window		! pointer to the graphics window
	integer*1 color_record(154)	! graphics window record
	integer*4 i,j			! counters
	integer*4 toolbx		! the tool box interface
	character*256 str255		! function to create a Pascal LSTRING
	integer*4 window		! general purpose pointer
	integer*2 rect(4)		! rectangle coordinates
	integer*4 menuhandle
	integer*4 menu,wordmask
	parameter (wordmask = z'0000FFFF')
	integer*4 x1,y1,x2,y2		! line end points
	integer*2 thecolor(3)		! RGB record for setting up palette
	integer*4 phandle		! palette handle
	integer*4 Grey,Blue,Red,Green,Yellow		! "Commands" menu selections
	integer*4 Random,Quit
	parameter (Grey=1,Blue=2,Red=3,Green=4,Yellow=5)
	parameter (Random=6,Quit=7)
	integer*4 penw,penh
	integer*4 rinc,ginc,binc
	integer*4 mouseloc,which
	integer*4 oldpal
	real*4 rnd
	eventmask = -1

*	
*  Close MacFortran I/O window 

	window = toolbx(FRONTWINDOW)
 	call toolbx(CLOSEWINDOW,window)
*
* Build Menu
*

	menuhandle = toolbx(NEWMENU,30,str255("Choose a Color"))
	call toolbx(APPENDMENU,menuhandle,
     +   str255("Greys;Blues;Reds;Greens;Yellows;Random;Quit;"))
	call toolbx(INSERTMENU,menuhandle,0)		! insert at end of list
	call toolbx(DRAWMENUBAR)
	
		
* Make our own color winow
	color_window = toolbx(PTR,color_record)
	rect(1) = 40
	rect(2) = 7
	rect(3) = 475
	rect(4) = 631

	color_window = toolbx(NEWCWINDOW,color_window,rect,
     +		        str255("Palette Demo"),.true.,4,-1,.false.,0)
     
* Make it the current port
     
	call toolbx(SETPORT,color_window)
	
* Get a handle to the current palette so we can restore it when 
* we are done

	oldpal = toolbx(GETPALETTE,color_window)
	
* Get a new palette with 16 entries (Numbered 0-15)

	phandle = toolbx(NEWPALETTE,16,0,2,0)
	
* Install our palette and set the pen mode and size
		
	call toolbx(SETPALETTE,color_window,phandle,0)
	penw = 39
	penh = 2
	call toolbx(PENSIZE,penw,penh)
	call toolbx(PENMODE,8)		! SrcCopy
	
* Set rect for clearing window

	rect(1) =0
	rect(2) =0
	rect(3) = 475
	rect(4) = 631

* Main loop

	do
	    if (toolbx(GETNEXTEVENT,eventmask,eventrecord)) then
	      select case (what)
	        case (1)		! mouse down
		  mouseloc = toolbx(FINDWINDOW,where,window)
		  if (mouseloc=1) then
		      which = menu(where)
		      if (which .ne. 0) then
		      	select case (which)
			case (Grey)
			  thecolor(1) = 65535
			  thecolor(2) = 65535
			  thecolor(3) = 65535
			  rinc = 4095
			  ginc = 4095
			  binc = 4095
			case (Blue)
			  thecolor(1) = 65535
			  thecolor(2) = 65535
			  thecolor(3) = 65535
			  rinc =  4095
			  ginc = 4095
			  binc = 0
			 case (Red)
			  thecolor(1) = 65535
			  thecolor(2) = 65535
			  thecolor(3) = 65535
			  rinc = 4095	! orig rinc = 0
			  ginc = 0	! ginc =  4095
			  binc = 0	! binc =  4095
			case (Green)
			  thecolor(1) = 65535
			  thecolor(2) = 65535
			  thecolor(3) = 65535
			  rinc = 0	! originally rinc =  4095
			  ginc = 4095	! originally ginc = 0
			  binc = 0	! originally binc =  4095
			case (Yellow)
			  thecolor(1) = 65535
			  thecolor(2) = 65535
			  thecolor(3) = 65535
			  rinc = 0
			  ginc = 0
			  binc = 4095
			case (Random)
			  continue
			case (Quit)
			  call toolbx(ERASERECT,rect)
			  call toolbx(DISPOSEPALETTE,phandle) ! get rid of our palette 
			  call toolbx(SETPALETTE,color_window,
     +				oldpal)			       ! restore old palette
			  call toolbx(ACTIVATEPALETTE,color_window)
			  stop
			case default
			  continue
			end select
			
			if (which .ne. Random) then
			  do (i=0,15)
			    call toolbx(SETENTRYCOLOR,phandle,i,
     +			  	toolbx(PTR,thecolor(1)))
	  		    thecolor(1) = (thecolor(1) - rinc)*5/10
	  		    thecolor(2) = (thecolor(2) - ginc)*5/10
	  		    thecolor(3) = (thecolor(3) - binc)*5/10
			  repeat
			else
			  do (i=0,15)
			   thecolor(1) = (wordmask.and.INT(65535*rnd()))
			   thecolor(2) = (wordmask.and.INT(65535*rnd()))
			   thecolor(3) = (wordmask.and.INT(65535*rnd()))
		 	   call toolbx(SETENTRYCOLOR,phandle,i,
     +			  	toolbx(PTR,thecolor(1)))
     			  repeat
			end if

* install changes made to palette

     			call toolbx(ERASERECT,rect)
			call toolbx(ACTIVATEPALETTE,color_window)
 			x1 = 0
 			y1 = 0
 			x2 = 0
 			y2 = 440
	
* draw some lines with our new palette

 			do (i = 0,15)
 			  call toolbx(PMFORECOLOR,i)
     			  call toolbx(MOVETO,x1,y1)
          		  call toolbx(LINETO,x2,y2)
 	   		  x2 = x2 + 39
         		  x1 = x1 + 39
   			repeat
		   end if
		 endif
	        case default
		  continue
	       end select
	    end if
	  repeat
	end
	
