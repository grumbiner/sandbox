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
