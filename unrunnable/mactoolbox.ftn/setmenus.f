	SUBROUTINE setmenus
	 
	IMPLICIT none

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