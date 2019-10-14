      PROGRAM plotter
      IMPLICIT none

C     Declare the variables which are passed to and from other routines.
      INTEGER size
      INTEGER x(1000), y(1000)

C     Get hold of the MacIntosh drawing routines
      INCLUDE HD.I40:MacFortran020:Include Files:quickdraw.inc
      INCLUDE HD.I40:MacFortran020:Include Files:window.inc
	INTEGER PTR
      PARAMETER (PTR=Z'C0000000')
      INTEGER toolbx
      INTEGER window
      INTEGER*2 grafport(54),rect(4)
	CHARACTER*256 str255
      
      INTEGER*4 view_window
      INTEGER*1 view_record(154)
	INTEGER*4 text_window
      INTEGER*1 text_record(154)
      
C     Locally used variables:
      INTEGER   i, nleaf
	REAL turns, alpha, radius, eccen
	INTEGER ticstr, ticend
	CHARACTER*6 mess
	
C     Begin the program.
C***********************************************************----------!!

      window = toolbx(FRONTWINDOW)
      CALL toolbx(CLOSEWINDOW,window)

 	view_window = toolbx(PTR,view_record)
 	rect(1) =  40
 	rect(2) =   1
 	rect(3) = 480
 	rect(4) = 640
 	view_window = toolbx(NEWWINDOW,view_window,rect,str255("View"),
     1		         .true.,0,-1,.true.,0)
     
      text_window = toolbx(PTR,text_record)
 	rect(1) =  40
 	rect(2) =   1
 	rect(3) = 480
 	rect(4) = 640
 	text_window = toolbx(NEWWINDOW,text_window,rect,str255("Text"),
     1		         .true.,0,-1,.true.,1)
     
 	CALL toolbx(SETPORT,text_window)
	CALL toolbx(TEXTFONT,4)
	CALL toolbx(TEXTSIZE,9)
	CALL toolbx(FORECOLOR,409)
	
C     Set up a data file
      PRINT *,' '
      PRINT *,'How many points do you want in the ellipse?'
	READ (*,9001) size
	PRINT *,'How large a radius (pixels)?'
	READ (*,9001) radius
	PRINT *,'What eccentricity?'
	READ (*,9002) eccen
	CALL ellips(x, y, size, 310, 220, radius, eccen)
C	CALL rose(x, y, size, 320, 240, radius, nleaf)
C	CALL spiral(x, y, size, 320, 240,turns, alpha)
 9001 FORMAT (I3)
 9002 FORMAT (E13.6)
                
      CALL toolbx(SELECTWINDOW,view_window)
 	CALL toolbx(SETPORT,view_window)
	CALL toolbx(SHOWPEN)
	CALL toolbx(PENSIZE,3,3)
	CALL toolbx(TEXTFONT,4)
	CALL toolbx(TEXTSIZE,9)
	CALL toolbx(FORECOLOR,409)
	
CD	ticstr = LONG(362)
      CALL toolbx(MOVETO, x(1), y(1))
      DO 1000 i = 2, size
        CALL toolbx (LINETO, x(i), y(i))
 1000 CONTINUE
CD      ticend = LONG(362)
	CALL toolbx(MOVETO, 60, 60)
	WRITE (mess,9003) ticend-ticstr
	CALL toolbx(DRAWSTRING,str255(mess))
 9003 FORMAT (I6)
 
      PAUSE

      END 
