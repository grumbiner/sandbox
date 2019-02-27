

*
* menus: a mouse down event was detected in the menu area; process menu
*	 selection
*

	integer function menu(where)
	
	implicit none
	include :include files:quickdraw.inc
	include :include files:menu.inc
	include :include files:misc.inc
	
	integer*4 toolbx
	
*	
*  variables for making menu selections
*
	integer*2 menuselection(2)	! menu selection information
c					!  (1) = menuid
c					!  (2) = menu item number
	integer*4 menudata		! for use left of equals sign
	
	equivalence (menuselection,menudata)
	
*
*  Menu selection constants:
*

	integer*4 Commands
	integer*4 Grey,Blue,Red,Green,Yellow		! "Commands" menu selections
	integer*4 Random,Quit
	parameter (Commands=30)
	parameter (Grey=1,Blue=2,Red=3,Green=4,Yellow=5)
	parameter (Random=6,Quit=7)

	menudata = toolbx(MENUSELECT,where)
	
	  select case (menuselection(1))
	  
	    case (Commands)		! the "Commands" menu was selected

	        select case (menuselection(2))
		 
		 case (Grey)
		 	menu = Grey
		 case (Blue)
		 	menu = Blue
		 case (Red)
		  	menu = Red
		 case (Green)
		   	menu = Green
		 case (Yellow)
		   	menu = Yellow
		 case (Random)
		 	menu = Random
		 case (Quit)
		    	menu  = Quit
		end select
              call toolbx(HILITEMENU,0)				    		
	    case default		! just playing with the mouse
	   	menu = 0 
	  end select
	  
	end